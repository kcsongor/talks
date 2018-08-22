{-# LANGUAGE TypeInType #-}
{-# LANGUAGE DataKinds              #-}
{-# LANGUAGE PolyKinds              #-}
{-# LANGUAGE TypeFamilies           #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE UndecidableInstances   #-}

module LHUG where

import GHC.TypeLits
import Data.Kind




























































































































-- Terms and types {{{1
five :: Int
five = 5

-- Type families {{{1
-- Closed type family (2014)
-- Boring :: Type -> Type
type family Boring a where
  Boring String = Char
  Boring Bool   = Int

-- Promotion {{{1

-- Naturals {{{1
data N = Z | S N
  deriving Show

two :: N
two = S (S Z)

--add :: N -> N -> N

-- Types and kinds {{{1
data N_p1 = Z_p1 | S_p1 N_p1
  deriving Show

type Two = 'S ('S 'Z)

-- With a 'CUSK'
type family Add (n :: Nat) (m :: Nat) :: Nat where
-- CUSKs... {{{1

type family Arg1 (x :: Type) :: Type
-- vs
type family Arg0 :: Type -> Type

-- Polymorphism {{{1
type family Id (a :: k) :: k where
  Id a = a
-- Non-linear patterns {{{1
type family Equals (a :: k) (b :: k) :: Bool where
-- Parametricity? {{{1
type family Id' (a :: k) :: k where
-- Parametricity? {{{1
-- How many implementations?
type family What :: k
-- Most general kinds? {{{1
-- What's the inferred kind?
type family What' where
  What' = Int
-- Unapply {{{1


-- Part 1 {{{1
type family UnApply1 (x :: Type) :: Type where
  --

-- Part 2 {{{1
type family UnApply2 (x :: Type) :: Type where
  UnApply2 (f a) = a

-- Part 3 {{{1
type family UnApply3 (x :: k) :: Type where
  UnApply3 (f a) = a

-- :kind! UnApply3 ('S 'Z) ?

-- Part 4 {{{1
type family UnApply4 (x :: k) :: j where
  UnApply4 (f a) = a -- won't reduce...

-- Computing return kinds {{{1
type family UnApply4_p1 (x :: k) :: j where
  UnApply4_p1 (f a) = a

-- Dependent kinds!
type family ArgKind (x :: k) :: Type where
  ArgKind (f (a :: k)) = k

type UnApply4' x = (UnApply4 x :: ArgKind x)

-- Head {{{1
type family Head (x :: k) :: j where
  Head (f a) = Head f
  Head f     = f

type family HeadKind (x :: k) :: Type where
  HeadKind (f a)    = HeadKind f
  HeadKind (f :: k) = k

type Head' x = (Head x :: HeadKind x)

-- But wait a minute... {{{1
type family Head_p1 (x :: k) :: j where
  Head_p1 (f a) = Head f -- pattern match on an application at _any_ kind
  Head_p1 f     = f

-- :kind! Head' 'True

-- Can we "fake" applications? {{{1

type family Boolish :: Nat -> Bool where
-- Boolish is stuck... no inhabitants!

-- How many inhabitants of Bool are there? {{{1
type family W (b :: Bool) :: Nat where
  W 'False = 0
  W 'True  = 1
  W (f n)  = n

-- stuck computation
type family C :: Nat -> k
-- >>> :k C
-- C :: Nat -> k

-- vs
type family C' (n :: Nat) :: k
-- >>> :k C'
-- C' :: Nat -> k



-- Injective type families (2016)
-- Inj2 a ~ Inj2 b => a ~ b
type family Inj1 a = r | r -> a where
  Inj1 Int = Maybe Int
  Inj1 a   = [a]

inj1 :: Inj1 b
inj1 = undefined

-- type family Inj2 a = r | r -> a where
--   Inj2 (Maybe a) = a

-- Inj2 (Maybe a) ~ a
--
--
--
-- a ~ (Inj2 [Char])
-- Inj2 (Maybe (Inj2 [Char])) ~ Inj2 [Char]
-- {injectivity of Inj2}
-- Maybe (Inj2 [Char]) ~ [Char]
-- {decomposition}
-- Maybe ~ [] and (Inj2 [Char] ~ Char)

-- stuck?
type family Stuck a

not_fun :: (Maybe (Stuck Int) ~ f a) => f Int -> Maybe Int
not_fun = id


-- lazy?

type family Loop where
  Loop = [Loop]

type family Const a b where
  Const a _ = a

type Foo = Const [Int] Loop

not_fun_2 :: (f a ~ Foo) => f Int -> [Int]
not_fun_2 = id

test :: Foo
test = [1]
