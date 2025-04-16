module MyLib (someFunc) where

someFunc :: IO ()
someFunc = putStrLn "someFunc"

-- [exa] bwe: tbh I'd recommend just biting the bullet and trying to reimplement the thing yourself, it's most effective
-- [exa] bwe: as the initial hint you get the information that stateful functions can be represented as (s -> (result, s)), possibly with
--   more parameters such as (a->b->s->(result, s)), then you add Functor Applicative Monad instances and eventually it just works

type State s a = (s -> (a, s))

(>>=) :: State s a -> (a -> State s b)      -> State s b
--       f         ->  g                    -> _
--       f         >>= g                    -> (s -> (b, s))
--   (s -> (a, s)) >>= (a -> (s -> (b, s))) -> (s -> (b, s))
-- f               >>= g                    = \s -> g _a _s
-- f               >>= g                    = \s -> let (a, s') = f s in g _a _s
-- f               >>= g                    = \s -> let (a, s') = f s in g a s'
f                  >>= g                    = \s -> let (x, s') = f s in g x s'

return :: a    -> State s a
-- return :: a -> (s -> (a, s))
-- return x = \s -> _ :: (a, s)
return x    = \s -> (x, s)
