{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances #-}

import Control.Monad.Trans.Maybe
import Control.Monad.State
import Data.Foldable
import Control.Applicative

type Time = Int -- TODO

foldMonads :: (Foldable f, Monad m) => f (m ()) -> m ()
foldMonads = foldr (>>) (pure ())

lastInFoldable :: (Foldable f) => f a -> Maybe a
lastInFoldable = foldl (const pure) empty

emptyFmap :: (Functor f) => f a -> f ()
emptyFmap = fmap (const ())

class Txn s t where
  txnState :: t -> MaybeT (State s) ()

instance Txn s () where
  txnState () = pure ()

class TimeCheckable b where
  timeCheck :: Time -> b -> Bool

class (Functor b, Foldable b) => Block s b where
  blockStateTxnless :: b t -> MaybeT (State s) ()
  blockStateTxnless = blockStateWithTxns . emptyFmap

  blockStateWithTxns :: (Txn s t) => b t -> MaybeT (State s) ()
  blockStateWithTxns b = foldMonads (fmap txnState b) >> blockStateTxnless b

chainState :: (Block s b, Txn s t, Foldable f, Functor f) => s -> f (b t) -> Maybe s
chainState s = f . flip runState s . runMaybeT . foldMonads . fmap blockStateWithTxns where
  f (m, s) = fmap (const s) m

chainStateCheckingTime :: (TimeCheckable (b ()), Block s b, Txn s t, Foldable f, Functor f) => s -> Time -> f (b t) -> Maybe s
chainStateCheckingTime s t c = do
  last <- lastInFoldable c
  guard $ timeCheck t $ emptyFmap last
  chainState s c
