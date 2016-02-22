{-# LANGUAGE
    RankNTypes,
    LambdaCase
    #-}

module Pipes.Core where

import Control.Applicative
import Control.Monad
import Control.Monad.Trans.Class

import Data.Void (Void, absurd)
import Data.Monoid

-- | Full bi-directional pipe. Parameters:
--   - 'lu' Leftovers from upstream. Either equal to 'iu', or '@Void@'.
--   - 'iu' Input from upstream
--   - 'ou' Output to upstream
--   - 'ru' Result from upstream
--   - 'ld' Leftovers from downstream. Either equal to 'id', or '@Void@'.
--   - 'id' Input from downstream
--   - 'od' Output to downstream
--   - 'rd' Result from downstream
data BiPipe lu iu ou ru ld id od rd m r = SendUp ou (m ()) (Either ru iu -> BiPipe lu iu ou ru ld id od rd m r)
                                          | SendDn od (m ()) (Either rd id -> BiPipe lu iu ou ru ld id od rd m r)
                                          | Monadic (m (BiPipe lu iu ou ru ld id od rd m r))
                                          | LeftoversUp lu (BiPipe lu iu ou ru ld id od rd m r)
                                          | LeftoversDn ld (BiPipe lu iu ou ru ld id od rd m r)
                                          | Done r

instance Functor m => Functor (BiPipe lu iu ou ru ld id od rd m) where
    fmap f = go
      where
        go (SendUp ou fin next)  = SendUp ou fin (go . next)
        go (SendDn od fin next)  = SendDn od fin (go . next)
        go (Monadic mp)          = Monadic (fmap go mp)
        go (LeftoversUp lu next) = LeftoversUp lu (go next)
        go (LeftoversDn ld next) = LeftoversDn ld (go next)
        go (Done r)              = Done (f r)

instance Monad m => Applicative (BiPipe lu iu ou ru ld id od rd m) where
    pure = Done
    pf <*> px = pf >>= flip fmap px
    -- Done f <*> px     = fmap f px
    -- pf     <*> Done x = fmap ($ x) pf
    -- SendUp ouf finf nextf <*> SendUp oux finx nextx = SendUp ouf finf (\case Left ru -> 

instance Monad m => Monad (BiPipe lu iu ou ru ld id od rd m) where
    return = Done
    (>>=) = _bind

_bind :: Monad m => BiPipe lu iu ou ru ld id od rd m a -> (a -> BiPipe lu iu ou ru ld id od rd m b) -> BiPipe lu iu ou ru ld id od rd m b
_bind (Done a)              f = f a
_bind (SendUp ou fin next)  f = SendUp ou fin (\u -> _bind (next u) f)
_bind (SendDn od fin next)  f = SendDn od fin (\d -> _bind (next d) f)
_bind (Monadic mp)          f = Monadic (mp >>= \p -> return (_bind p f))
_bind (LeftoversUp lu next) f = LeftoversUp lu (_bind next f)
_bind (LeftoversDn ld next) f = LeftoversDn ld (_bind next f)

instance MonadTrans (BiPipe lu iu ou ru ld id od rd) where
    lift = Monadic . fmap Done

sendUp :: Monad m => ou -> BiPipe lu iu ou ru ld id od rd m (Either ru iu)
sendUp ou = SendUp ou (return ()) Done
{-# INLINE sendUp #-}

sendDown :: Monad m => od -> BiPipe lu iu ou ru ld id od rd m (Either rd id)
sendDown od = SendDn od (return ()) Done
{-# INLINE sendDown #-}

awaitE :: Monad m => BiPipe lu iu () ru ld id od rd m (Either ru iu)
awaitE = sendUp ()
{-# INLINE awaitE #-}

await :: Monad m => BiPipe lu iu () ru ld id od rd m (Maybe iu)
await = SendUp () (return ()) (\case Left ru -> Done Nothing; Right iu -> Done (Just iu))
{-# INLINE await #-}

yieldOr :: Monad m => od -> m () -> BiPipe lu iu ou ru ld () od rd m ()
yieldOr od fin = SendDn od fin (\_ -> Done ())
{-# INLINE yieldOr #-}

yield :: Monad m => od -> BiPipe lu iu ou ru ld () od rd m ()
yield od = yieldOr od (return ())
{-# INLINE yield #-}

leftover :: iu -> BiPipe iu iu ou ru ld id od rd m ()
leftover iu = LeftoversUp iu (Done ())
{-# INLINE leftover #-}

leftoverFromDn :: id -> BiPipe lu iu ou ru id id od rd m ()
leftoverFromDn id = LeftoversDn id (Done ())
{-# INLINE leftoverFromDn #-}

-- | Run a fully-closed Pipe
runEffect :: Monad m => Effect m r -> m r
runEffect (SendUp o fin next)  = absurd o
runEffect (SendDn o fin next)  = absurd o
runEffect (Monadic mp)         = mp >>= runEffect
runEffect (LeftoversUp l next) = absurd l
runEffect (LeftoversDn l next) = absurd l
runEffect (Done r)             = return r

-- | Fuse two 'BiPipe's together into one, connecting the left pipe's downstream to the right pipe's upstream and using
--   a monoid to combine the results if both finish at the same time. If one finishes early, only that pipe's result is returned.
(>->) :: (Monad m, Monoid r)
         => BiPipe lu iu ou ru Void im om r m r
         -> BiPipe             Void om im r ld id od rd m r
         -> BiPipe lu iu ou ru              ld id od rd m r
SendUp ou fin next  >-> d                = SendUp ou fin (\ru -> next ru >-> d)
Monadic mu          >-> d                = Monadic (mu >>= \u -> return (u >-> d))
LeftoversUp lu next >-> d                = LeftoversUp lu (next >-> d)
u >-> SendDn od fin next                 = SendDn od fin (\rd -> u >-> next rd)
u >-> Monadic md                         = Monadic (md >>= \d -> return (u >-> d))
u >-> LeftoversDn ld next                = LeftoversDn ld (u >-> next)
Done l             >-> Done r            = Done (l <> r)
Done l             >-> SendUp _ fin next = lift fin >> Done l
SendDn _  fin next >-> Done r            = lift fin >> Done r
SendDn od finu nextu >-> SendUp ou find nextd = (nextu (Right ou) >-> nextd (Right od)) <* lift (finu >> find)

-- | This is only useful to make types match. As such, it's simply 'unsafeCoerce'.
--   It's safe, because the change in the type is phantom: the only constructor
--   with a 'lu' parameter is never present in the input.
polyLeftoversUp :: Monad m
                => BiPipe Void ou iu ru ld id od rd m r
                -> BiPipe lu   ou iu ru ld id od rd m r
polyLeftoversUp = unsafeCoerce

-- | This is only useful to make types match. As such, it's simply 'unsafeCoerce'.
--   It's safe, because the change in the type is phantom: the only constructor
--   with a 'ld' parameter is never present in the input.
polyLeftoversDn :: Monad m
                => BiPipe lu ou iu ru Void id od rd m r
                -> BiPipe lu ou iu ru ld   id od rd m r
polyLeftoversDn = unsafeCoerce

-- | Swap a BiPipe's upstream and downstream interfaces
reverseP :: Monad m
         => BiPipe lu iu ru ou ld id od rd m r
         -> BiPipe ld id rd od lu iu ou ru m r
reverseP (SendUp x fin next)  = SendDn x fin (reverseP . next)
reverseP (SendDn x fin next)  = SendUp x fin (reverseP . next)
reverseP (Monadic mp)         = Monadic (fmap reverseP mp)
reverseP (LeftoversUp l next) = LeftoversUp l (reverseP next)
reverseP (LeftoversDn l next) = LeftoversDn l (reverseP next)
reverseP (Done r)             = Done r

type Effect           m r = BiPipe Void () Void () Void () Void () m r -- | A pipe with both interfaces closed off
type BiSource l i o d m r = BiPipe Void () Void () l    i  o    d  m r -- | A pipe with its upstream interface closed off
type BiSink   l i o u m r = BiPipe l    i  o    u  Void () Void () m r -- | A pipe with its downstream interface closed off
type Pipe     l i o u m r = BiPipe l    i  Void u  Void () o    () m r -- | A pipe restricted to unidrectional flow
type Source       o   m r = BiPipe Void () Void () Void () o    () m r -- | A unidrectional pipe with its upstream interface closed off
type Sink     l i   u m r = BiPipe l    i  Void u  Void () Void () m r -- | A unidrectional pipe with its downstream interface closed off