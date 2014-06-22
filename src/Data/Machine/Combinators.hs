{-# LANGUAGE Rank2Types #-}

module Data.Machine.Combinators where

import Prelude hiding (any, all)

import Control.Applicative ((<|>))
import Data.Machine


-- | Emit a single `False` as soon as the predicate fails.
--   Otherwise, emit `True` once the input is exhausted.
all :: (a -> Bool) -> Process a Bool
all f = construct $ go <|> yield True where
  go = do
    a <- await
    if not (f a)
      then yield False
      else go

-- | Emit a single `True` as soon as the predicate succeeds.
--   Otherwise, emit `False` once the input is exhausted.
any :: (a -> Bool) -> Process a Bool
any f = auto not <~ all (not . f)
