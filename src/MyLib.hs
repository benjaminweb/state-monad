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
-- fmap :: (a -> b) -> f a                 -> f b
-- fmap :: (a -> b) -> State s a           -> State s b
-- fmap :: (a -> b) -> State (s -> (a, s)) -> State (s -> (b, s))
   fmap    f           (MkState g)         =  MkState $ \s0 -> let (a, s1) = g s0 in (f a, s1)

--instance Monad State s where
--(>>=) :: State s a       -> (a -> State s b)              -> State s b
--       f               ->  g                            -> _
--       f               >>= g                            -> (s -> (b, s))
--   State (s -> (a, s)) >>= (a -> (State (s -> (b, s)))) -> State (s -> (b, s))
--  f                   >>= g                            = State (\s -> let (a, s') = f s in g a)

return :: a    -> State s a
-- return :: a -> (s -> (a, s))
-- return x = \s -> _ :: (a, s)
return x    = MkState $ \s -> (x, s)
