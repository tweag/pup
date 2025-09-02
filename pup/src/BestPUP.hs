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

type Pup err ann = Print.PPrint err ann Indexed.:*: Indexed.IgnoreIndices (Megaparsec.Parsec err Text)

type Pup' = Pup Void ()

print :: Pup err ann (a -> Maybe (Prettyprinter.Doc ann)) (Maybe (Prettyprinter.Doc ann)) b -> a -> Maybe (Prettyprinter.Doc ann)
print (prnt Indexed.:*: _) = Print.run prnt

parse :: (Megaparsec.ShowErrorComponent err) => Pup err ann r r' a -> String -> Text -> Either String a
parse (_ Indexed.:*: Indexed.IgnoreIndices prse) name input =
  case Megaparsec.runParser prse name input of
    (Left err) -> Left $ Megaparsec.errorBundlePretty err
    (Right a) -> Right a

space1 :: Pup' r r ()
space1 = (Print.tell Prettyprinter.line) Indexed.:*: Indexed.IgnoreIndices Megaparsec.space1

space :: Pup' r r ()
space = Indexed.pure () Indexed.:*: Indexed.IgnoreIndices Megaparsec.space
