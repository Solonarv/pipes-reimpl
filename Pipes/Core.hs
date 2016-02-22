{-# LANGUAGE
    RankNTypes,
    LambdaCase
    #-}

module Pipes.Core where

import Control.Applicative
import Control.Monad
import Control.Monad.Trans.Class
import Data.Void (Void, absurd)

-- | Full bi-directional pipe. Parameters:
--   - 'lu' Leftovers from upstream. Either equal to 'iu', or '@Void@'.
--   - 'iu' Input from upstream
--   - 'ou' Output to upstream
--   - 'ru' Result from upstream
--   - 'ld' Leftovers from downstream. Either equal to 'id', or '@Void@'.
--   - 'id' Input from downstream
--   - 'od' Output to downstream
--   - 'rd' Result from downstream
data FullPipe lu iu ou ru ld id od rd m r = SendUp ou (m ()) (Either ru iu -> FullPipe lu iu ou ru ld id od rd m r)
                                          | SendDn od (m ()) (Either rd id -> FullPipe lu iu ou ru ld id od rd m r)
                                          | Monadic (m (FullPipe lu iu ou ru ld id od rd m r))
                                          | LeftoversUp lu (FullPipe lu iu ou ru ld id od rd m r)
                                          | LeftoversDn ld (FullPipe lu iu ou ru ld id od rd m r)
                                          | Done r

instance Functor m => Functor (FullPipe lu iu ou ru ld id od rd m) where
    fmap f = go
      where
        go (SendUp ou fin next)  = SendUp ou fin (go . next)
        go (SendDn od fin next)  = SendDn od fin (go . next)
        go (Monadic mp)          = Monadic (fmap go mp)
        go (LeftoversUp lu next) = LeftoversUp lu (go next)
        go (LeftoversDn ld next) = LeftoversDn ld (go next)
        go (Done r)              = Done (f r)

instance Monad m => Applicative (FullPipe lu iu ou ru ld id od rd m) where
    pure = Done
    pf <*> px = pf >>= flip fmap px
    -- Done f <*> px     = fmap f px
    -- pf     <*> Done x = fmap ($ x) pf
    -- SendUp ouf finf nextf <*> SendUp oux finx nextx = SendUp ouf finf (\case Left ru -> 

instance Monad m => Monad (FullPipe lu iu ou ru ld id od rd m) where
    return = Done
    (>>=) = _bind

_bind :: Monad m => FullPipe lu iu ou ru ld id od rd m a -> (a -> FullPipe lu iu ou ru ld id od rd m b) -> FullPipe lu iu ou ru ld id od rd m b
_bind (Done a)              f = f a
_bind (SendUp ou fin next)  f = SendUp ou fin (\u -> _bind (next u) f)
_bind (SendDn od fin next)  f = SendDn od fin (\d -> _bind (next d) f)
_bind (Monadic mp)          f = Monadic (mp >>= \p -> return (_bind p f))
_bind (LeftoversUp lu next) f = LeftoversUp lu (_bind next f)
_bind (LeftoversDn ld next) f = LeftoversDn ld (_bind next f)

instance MonadTrans (FullPipe lu iu ou ru ld id od rd) where
    lift = Monadic . fmap Done

sendUp :: Monad m => ou -> FullPipe lu iu ou ru ld id od rd m (Either ru iu)
sendUp ou = SendUp ou (return ()) Done
{-# INLINE sendUp #-}

sendDown :: Monad m => od -> FullPipe lu iu ou ru ld id od rd m (Either rd id)
sendDown od = SendDn od (return ()) Done
{-# INLINE sendDown #-}

awaitE :: Monad m => FullPipe lu iu () ru ld id od rd m (Either ru iu)
awaitE = sendUp ()
{-# INLINE awaitE #-}

await :: Monad m => FullPipe lu iu () ru ld id od rd m (Maybe iu)
await = SendUp () (return ()) (\case Left ru -> Nothing; Right iu -> Just iu)
{-# INLINE await #-}

yieldOr :: Monad m => od -> m () -> FullPipe lu iu ou ru ld () od rd m ()
yieldOr od fin = SendDn od fin (\_ -> Done ())
{-# INLINE yieldOr #-}

yield :: Monad m => od -> FullPipe lu iu ou ru ld () od rd m ()
yield od = yieldOr od (return ())
{-# INLINE yield #-}

leftover :: iu -> FullPipe iu iu ou ru ld id od rd m ()
leftover iu = LeftoversUp iu (Done ())
{-# INLINE leftover #-}

leftoverFromDn :: id -> FullPipe lu iu ou ru id id od rd m ()
leftoverFromDn id = LeftoversDn id (Done ())
{-# INLINE leftoverFromDn #-}

-- | Run a fully-closed Pipe
runEffect :: Monad m => Effect m r -> m r
runEffect (SendUp o next)      = absurd o
runEffect (SendDn o next)      = absurd o
runEffect (Monadic mp)         = fmap runEffect mp
runEffect (LeftoversUp l next) = absurd l
runEffect (LeftoversDn l next) = absurd l
runEffect (Done r)             = return r

-- | Fuse two 'FullPipe's together into one, connecting the left pipe's downstream to the right pipe's upstream
(>->) :: FullPipe lu iu ou ru lm im om rm m r -> FullPipe lm im om rm ld id od rd m r -> FullPipe lu iu ou ru ld id od rd m r
SendUp ou fin next >-> d = SendUp ou fin (\r -> 

type Effect         m r = FullPipe Void () Void () Void () Void () m r
type Source l i o d m r = FullPipe Void () Void () l    i  o    d  m r
type Sink   l i o u m r = 
type Pipe l i o u m r = FullPipe l    i  Void u  Void () o    () m r