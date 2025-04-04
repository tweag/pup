module Combinators where

import Base.Parse (Parse (..))
import Base.Parse qualified as Parse
import Base.Print (Print (..))
import Base.Print qualified as Print
import Control.Monad.Indexed qualified as Indexed

type PUP = Print Indexed.:*: Indexed.IgnoreStack Parse
