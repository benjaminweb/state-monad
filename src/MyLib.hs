module MyLib (someFunc) where

someFunc :: IO ()
someFunc = putStrLn "someFunc"

-- [exa] bwe: tbh I'd recommend just biting the bullet and trying to reimplement the thing yourself, it's most effective
-- [exa] bwe: as the initial hint you get the information that stateful functions can be represented as (s -> (result, s)), possibly with
--   more parameters such as (a->b->s->(result, s)), then you add Functor Applicative Monad instances and eventually it just works
