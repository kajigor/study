module DnfF where

import Exp
import PushNeg

data MulCtx e = Left e | Right e | No

