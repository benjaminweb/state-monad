{-# LANGUAGE InstanceSigs #-}
module MyLib (someFunc) where

someFunc :: IO ()
someFunc = putStrLn "someFunc"

-- task 1:
-- [exa] bwe: tbh I'd recommend just biting the bullet and trying to reimplement the thing yourself, it's most effective
-- [exa] bwe: as the initial hint you get the information that stateful functions can be represented as (s -> (result, s)), possibly with
--   more parameters such as (a->b->s->(result, s)), then you add Functor Applicative Monad instances and eventually it just works

-- type State s a = (s -> (a, s))
newtype State s a = MkState (s -> (a, s))

instance Functor (State s) where
-- fmap :: (a -> b) -> f a                     -> f b
   fmap :: (a -> b) -> State s a               -> State s b
-- fmap :: (a -> b) -> (s0 -> (a, s1))         -> (s0 -> (b, s1))
   fmap    f           (MkState g)             =  MkState $ \s0 -> let (a, s1) = g s0 in (f a, s1)

instance Applicative (State s) where
-- (<*>) :: f (a -> b)                   -> f a                     -> f b
   (<*>) :: State s (a -> b)             -> State s a               -> State s b
-- (<*>) :: (s0 -> (a -> b, s1))         -> (s1 -> (a, s2))         -> (s2 -> (b, s3))
   (<*>)    (MkState f)                     (MkState g)             =  MkState $ \s0 -> let (f', s1)   = f s0
                                                                                            (a, s2)    = g s1
                                                                                         in (f' a, s2)
instance Monad (State s) where
-- (>>=) :: m a                   -> (a -> m b)                   -> m b
   (>>=) :: State s a             -> (a -> State s b)             -> State s b
-- (>>=) :: (s -> (a, s))         -> (a -> (s -> (b, s)))         -> (s -> (b, s))
   (>>=)    (MkState f)              g                            =  MkState $ \s0 -> let (a, s1)    = f s0
                                                                                          MkState g' = g a
                                                                                       in g' s1

return :: a    -> State s a
-- return :: a -> (s -> (a, s))
-- return x = \s -> _ :: (a, s)
return x    = MkState $ \s -> (x, s)

-- task 2:
-- [exa]: bwe: as the next task try to think how to implement "set state" function and "get state" function
-- [exa]: (the types of these should be roughly `s -> State s ()` and `State s s`, respectively)
