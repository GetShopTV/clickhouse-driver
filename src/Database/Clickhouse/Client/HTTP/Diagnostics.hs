{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE LambdaCase #-}

module Database.Clickhouse.Client.HTTP.Diagnostics (httpStreamingWithMetrics) where

import Control.Concurrent.MVar (readMVar)
import Control.Exception (mask_)
import Control.Monad (unless, void)
import Control.Monad.Trans.Resource (MonadResource, register, unprotect)
import Data.ByteString qualified as BS
import Data.IORef (atomicModifyIORef', newIORef)
import Database.Clickhouse.Client.Types (ClickhouseTransferMetrics (..))
import HCurl.Agent (Agent)
import HCurl.Internal.Body qualified as Body
import HCurl.Internal.Easy (RequestBodyMode (..), RequestHandler (..))
import HCurl.Internal.Headers (awaitHeaderBlock)
import HCurl.Internal.Metrics (extractMetrics)
import HCurl.Internal.Response (getCompletedHttpParts, getHttpParts, getHttpPartsState, getTransferResult)
import HCurl.Internal.Result (getCurlCode)
import HCurl.Internal.Transfer (RunningTransfer (..), startTransferWith)
import HCurl.Request qualified as Request
import HCurl.Response (HttpParts (..), StreamingResponse (..))
import HCurl.Types qualified as Curl
import UnliftIO (MonadUnliftIO, finally, liftIO, mask, onException, withRunInIO)

httpStreamingWithMetrics ::
  (MonadResource m, MonadUnliftIO m) =>
  Body.StreamConfig ->
  IO () ->
  (Bool -> ClickhouseTransferMetrics -> IO ()) ->
  Agent ->
  Request.Request ->
  m (Either Curl.CurlCode (StreamingResponse Body.BodyReader), IO ClickhouseTransferMetrics)
httpStreamingWithMetrics streamConfig onStarted onFinished agent request = mask \restore -> do
  liftIO $ Body.validateStreamBufferSize (Body.bufferedChunks streamConfig)
  let !bodyBytes = case Request.body request of
        Curl.Buffer bytes -> fromIntegral (BS.length bytes)
        Curl.Empty -> 0
  transfer <- startTransferWith UseRequestBody agent request \context identifier easy headers metrics -> do
    streamState <- liftIO $ Body.newBodyStreamState streamConfig context identifier
    resources <- Body.installBodyStream easy headers metrics streamState
    pure (streamState, resources, Body.bodyTransferStreams streamState)
  let handler = transferHandler transfer
      streamState = responseTarget handler
      metricsSnapshot = mask_ do
        readMVar (doneRequest handler)
        code <- getCurlCode (easyData handler)
        response <- getHttpParts (requestHeaders handler)
        metrics <- extractMetrics (metricsContext handler)
        pure
          ClickhouseTransferMetrics
            { requestBodyBytes = bodyBytes
            , responseStatus = if statusCode response == 0 then Nothing else Just (statusCode response)
            , transferCode = show code
            , curlMetrics = metrics
            }
  reported <- liftIO $ newIORef False
  let finalize cancelled = mask_ do
        if cancelled
          then Body.markBodyStreamClosed streamState >> closeTransfer transfer
          else finishTransfer transfer
        wasReported <- atomicModifyIORef' reported (\previous -> (True, previous))
        unless wasReported $ metricsSnapshot >>= onFinished cancelled
  cleanupKey <- register (finalize True) `onException` liftIO (finalize True)
  unregisterCleanup <- withRunInIO \runInIO -> pure $ void $ runInIO $ unprotect cleanupKey
  let finishTransferBody = finalize False `finally` unregisterCleanup
      closeTransferBody = finalize True `finally` unregisterCleanup
  reader <- liftIO $ Body.mkBodyReader streamState finishTransferBody closeTransferBody
  response <-
    restore (liftIO $ onStarted >> awaitResponseHead handler)
      `onException` liftIO (Body.closeBody reader)
  outcome <- case response of
    Left code -> liftIO finishTransferBody >> pure (Left code)
    Right info ->
      pure $
        Right
          StreamingResponse
            { info
            , body = reader
            , completion = mask_ do
                readMVar (doneRequest handler)
                result <- getTransferResult (easyData handler) (metricsContext handler)
                finishTransferBody
                pure result
            }
  pure (outcome, metricsSnapshot)

awaitResponseHead :: RequestHandler response -> IO (Either Curl.CurlCode HttpParts)
awaitResponseHead handler = do
  awaitHeaderBlock (requestHeaders handler)
  (response, terminalResult) <- getHttpPartsState (requestHeaders handler)
  case terminalResult of
    Just code | code /= Curl.Ok -> pure (Left code)
    _ | statusCode response /= 0 -> pure (Right response)
    _ -> do
      readMVar (doneRequest handler)
      getCurlCode (easyData handler) >>= \case
        Curl.Ok -> Right <$> getCompletedHttpParts (easyData handler) (requestHeaders handler)
        code -> pure (Left code)
