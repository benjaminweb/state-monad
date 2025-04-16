{-# LANGUAGE InstanceSigs #-}
module MyLib where

someFunc :: IO ()
someFunc = putStrLn "someFunc"

-- task 1:
-- [exa] bwe: tbh I'd recommend just biting the bullet and trying to reimplement the thing yourself, it's most effective
-- [exa] bwe: as the initial hint you get the information that stateful functions can be represented as (s -> (result, s)), possibly with
--   more parameters such as (a->b->s->(result, s)), then you add Functor Applicative Monad instances and eventually it just works

-- type State s a = (s -> (a, s))
newtype State s a = MkState {runState :: s -> (a, s)}

instance Functor (State s) where
-- fmap :: (a -> b) -> f a                     -> f b
   fmap :: (a -> b) -> State s a               -> State s b
-- fmap :: (a -> b) -> (s -> (a, s))           -> (s -> (b, s))
   fmap    f           (MkState g)             =  MkState $ \s0 -> let (a, s1) = g s0 in (f a, s1)

instance Applicative (State s) where
-- (<*>) :: f (a -> b)                   ->  f a                     -> f b
   (<*>) :: State s (a -> b)             ->  State s a               -> State s b
-- (<*>) :: (s -> (a -> b, s))           ->  (s -> (a, s))           -> (s -> (b, s))
   MkState f                             <*> MkState g               =  MkState $ \s0 -> let (f', s1)   = f s0
                                                                                             (a, s2)    = g s1
                                                                                         in  (f' a, s2)
-- pure :: a -> f a
   pure :: a -> State s a
   pure    a =  MkState $ \s -> (a, s)
  
instance Monad (State s) where
-- (>>=) :: m a                   ->  (a -> m b)                   -> m b
   (>>=) :: State s a             ->  (a -> State s b)             -> State s b
-- (>>=) :: (s -> (a, s))         ->  (a -> (s -> (b, s)))         -> (s -> (b, s))
   MkState f                      >>= g                            =  MkState $ \s0 -> let (a, s1)     = f s0
                                                                                           MkState g' = g a
                                                                                       in  g' s1

-- task 2:
-- [exa]: bwe: as the next task try to think how to implement "set state" function and "get state" function
-- [exa]: (the types of these should be roughly `s -> State s ()` and `State s s`, respectively)

set :: s -> State s ()
-- set :: s -> (s -> ((), s))
set    s1   =  MkState $ \_s0 -> ((), s1)

get :: State s s
-- get :: (s -> (s, s))
get    =  MkState $ \s -> (s, s)

-- task 3:
-- [exa]: so essentially what you need now is some helper tool for the "users" that allows you to run the state (starting with some initial state),
--        e.g. `evalState :: State s a -> s -> a` or so

evalState :: State s a -> s -> a
-- evalState :: (s -> (a, s)) -> s   ->                        a
evalState       (MkState f)   =  \s0 -> let (a, _s1) = f s0 in a

runState' ::    State s a -> s -> (a, s)
-- runState' :: (s -> (a, s)) -> s  -> (a, s)
runState'       (MkState f)      s0 =  f s0

-- task 4:
-- ski: another exercise, define a reasonable type `Tree' of trees (you should be able to make it a `Functor'), and then define `label :: Tree a
--      -> Tree (Integer,a)', numbering every element with a running index, starting at `0', counting upwards
-- ski: you could make two definitions of `label', one "direct", and one using your `State'
-- ski: (it might help to define a `tick :: State Integer Integer', that yields the current count, while also post-incrementing it)

data Tree a = Leaf a | Node (Tree a) (Tree a) deriving Show

instance Functor Tree where
   fmap :: (a -> b) -> Tree a     -> Tree b
   fmap    f           (Leaf a)   =  Leaf $ f a
   fmap    f           (Node x y) =  Node (f <$> x) (f <$> y)

-- >>> relabel 0 (Node (Node (Leaf "a") (Lead "b")) (Node (Leaf "c") (Leaf "d")))
-- Node (Leaf (1,"a")) (Node (Leaf (3,"b")) (Leaf (4,"c")))
relabel :: Integer   -> Tree a     ->  Tree (Integer, a)
relabel n t = fst $ go n t

go :: Integer -> Tree a     -> (Tree (Integer, a), Integer)
go    n          (Leaf x)   =  (Leaf (n, x), n + 1)
go    n          (Node l r) =  let (l', n1)   = go n l
                                   (r', n2)   = go n1 r
                               in  (Node l' r', n2)

-- :t relabel' (Node (Leaf "a") (Node (Leaf "b") (Leaf "c")))
-- :: State Integer (Tree (Integer, String))
--
-- :t runState
-- runState :: State s a -> s -> (a, s)
-- 
-- >>> flip runState 0 $ relabel' (Node (Node (Leaf "a") (Leaf "b")) (Node (Leaf "c") (Leaf "d")))
-- (Node (Node (Leaf (0,"a")) (Leaf (1,"b"))) (Node (Leaf (2,"c")) (Leaf (3,"d"))),4)
relabel' :: Tree a -> State Integer (Tree (Integer, a))
relabel' (Leaf x) = do
                      n <- get
                      set $ n + 1
                      return $ Leaf (n, x)
relabel' (Node l r) = relabel' l >>= \l' ->
                      relabel' r >>= \r' ->
                      return $ Node l' r'
