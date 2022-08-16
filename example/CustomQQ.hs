module CustomQQ where

import Language.Haskell.TH.Quote

import PyF
import PyF.Internal.QQ

customFmt :: QuasiQuoter
customFmt =
    mkFormatter
        "fmtWithDelimiters"
        ( fmtConfig
            { delimiters = Just ('@', '!')
            }
        )