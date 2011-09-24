module Here (here) where

import Language.Haskell.TH.Lib
import Language.Haskell.TH.Quote
import Language.Haskell.TH.Syntax

here :: QuasiQuoter
here = QuasiQuoter (litE . stringL) (litP . stringL)
