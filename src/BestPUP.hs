{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QualifiedDo #-}
{-# OPTIONS_GHC -Wno-x-partial #-}

module BestPUP where

import Base.Megaparsec
import Base.Prettyprinter qualified as Print
import Control.Additive ((<|>))
import Control.Additive qualified as Additive
import Control.Monad (void)
import Control.Monad.Indexed ((<*))
import Control.Monad.Indexed qualified as Indexed
import Data.Char qualified as Char
import Data.Text (Text)
import Data.Void (Void)
import Prettyprinter qualified
import Text.Megaparsec qualified as Megaparsec
import Text.Megaparsec.Char qualified as Megaparsec
import Prelude hiding (Applicative (..), Monad (..), MonadFail (..))
import Prelude qualified

type PUP = Print.PPrint Void () Indexed.:*: Indexed.IgnoreStack (Megaparsec.Parsec Void Text)

print :: (Indexed.Unroll r (Maybe (Prettyprinter.Doc ()))) => PUP r (Maybe (Prettyprinter.Doc ())) a -> r
print (prnt Indexed.:*: _) = Print.run prnt

parse :: PUP r r' a -> String -> Text -> Either String a
parse (_ Indexed.:*: Indexed.IgnoreStack prse) name input =
  case Megaparsec.runParser prse name input of
    (Left err) -> Left $ Megaparsec.errorBundlePretty err
    (Right a) -> Right a

-- TODO: what should `fail` be more abstractly?
fail :: String -> PUP r r a
fail msg = Additive.empty Indexed.:*: (Indexed.IgnoreStack $ Prelude.fail msg)

-- | Version of `guard` with an error message.
guard :: Bool -> String -> PUP r r ()
guard True _ = Indexed.pure ()
guard False msg = fail msg

anyChar :: PUP (Char -> r) r Char
anyChar = anySingle

char :: Char -> PUP r r ()
char = void . single

group :: PUP r r' a -> PUP r r' a
group (prnt Indexed.:*: prse) = Print.modify Prettyprinter.group prnt Indexed.:*: prse

nest :: Int -> PUP r r' a -> PUP r r' a
nest n (prnt Indexed.:*: prse) = Print.modify (Prettyprinter.nest n) prnt Indexed.:*: prse

space1 :: PUP r r ()
space1 = (Print.tell Prettyprinter.line) Indexed.:*: Indexed.IgnoreStack Megaparsec.space1

space :: PUP r r ()
space = Indexed.pure () Indexed.:*: Indexed.IgnoreStack Megaparsec.space

string :: Text -> PUP r r ()
string s = void $ tokens (==) s Indexed.@ s

digit :: PUP (Int -> r) r Int
digit = Indexed.do
  Indexed.stack (\_fl k i -> k (head (show i))) (\k _ -> k 0)
  c <- anyChar
  guard (Char.isDigit c) ("Expected digit, found: " ++ show c)
  Indexed.pure $ read [c]

int :: PUP (Int -> r) r Int
int = Indexed.do
  Indexed.shift_ $ \k fl -> Indexed.pure (\i -> k (fl . undigitise) (digitise i))
  undigitise <$> Indexed.some (try digit)
  where
    digitise n
      | n < 10 = [n]
      | otherwise = let (q, r) = quotRem n 10 in (digitise q) ++ [r]
    undigitise = foldl (\n d -> 10 * n + d) 0

bool :: PUP (Bool -> r) r Bool
bool =
  trueLead <* string "True"
    <|> falseLead <* string "False"
  where
    trueLead = Indexed.do
      Indexed.stack (\cases _ k True -> k; fl _ b -> fl b) (\k -> k True)
      Indexed.pure True

    falseLead = Indexed.do
      Indexed.stack (\cases _ k False -> k; fl _ b -> fl b) (\k -> k False)
      Indexed.pure False
