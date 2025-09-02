{-# LANGUAGE QualifiedDo #-}

module BestPUP where

import Base.Prettyprinter qualified as Print
import Control.Monad.Indexed qualified as Indexed
import Data.Text (Text)
import Data.Void (Void)
import Prettyprinter qualified
import Text.Megaparsec qualified as Megaparsec
import Text.Megaparsec.Char qualified as Megaparsec
import Prelude hiding (Applicative (..), Monad (..), MonadFail (..))

type PUP = Print.PPrint Void () Indexed.:*: Indexed.IgnoreIndices (Megaparsec.Parsec Void Text)

print :: PUP (a -> Maybe (Prettyprinter.Doc ())) (Maybe (Prettyprinter.Doc ())) b -> a -> Maybe (Prettyprinter.Doc ())
print (prnt Indexed.:*: _) = Print.run prnt

parse :: PUP r r' a -> String -> Text -> Either String a
parse (_ Indexed.:*: Indexed.IgnoreIndices prse) name input =
  case Megaparsec.runParser prse name input of
    (Left err) -> Left $ Megaparsec.errorBundlePretty err
    (Right a) -> Right a

space1 :: PUP r r ()
space1 = (Print.tell Prettyprinter.line) Indexed.:*: Indexed.IgnoreIndices Megaparsec.space1

space :: PUP r r ()
space = Indexed.pure () Indexed.:*: Indexed.IgnoreIndices Megaparsec.space
