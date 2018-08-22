{-# LANGUAGE DataKinds              #-}
{-# LANGUAGE PolyKinds              #-}
{-# LANGUAGE TypeFamilies           #-}

module LHUG where

import GHC.TypeLits
import Data.Kind
























































































































































































-- Terms and types {{{1
--




























































-- Naturals {{{1
data N = Z | S N
  deriving Show

two :: N
two = S (S Z)

--add :: N -> N -> N





























































-- Types and kinds {{{1
--
-- >>>> :t two
-- two :: N
-- >>>> :t N
-- ???





























































-- Promotion {{{1





























































-- Types and kinds {{{1
data N_p1 = Z_p1 | S_p1 N_p1
  deriving Show

type Two = 'S ('S 'Z)

-- Closed type family (2014)
type family Add (n :: Nat) (m :: Nat) :: Nat where
--



























































-- Type families {{{1
-- Boring :: Type -> Type
type family Boring a where
  Boring String = Char
  Boring Bool   = Int





























































-- Polymorphism {{{1
type family Id a where
  Id a = a




























































-- Non-linear patterns {{{1
type family Equals (a :: k) (b :: k) :: Bool where




























































-- A note on arity {{{1
--
type family Arg1 (x :: Type) :: Type
-- vs
type family Arg0 :: Type -> Type





























































-- Parametricity? {{{1
type family Id' (a :: k) :: k where





























































-- Parametricity? {{{1
-- How many implementations?
type family What :: k




























































-- Most general kinds? {{{1
-- What's the inferred kind? What's the most general?
type family What' where
  What' = Int





























































-- CUSKs {{{1
--






























































-- Stuck type families {{{1
type family Stuck a where

not_fun :: (Maybe (Stuck Bool) ~ f a) => f Int -> Maybe Int
not_fun = id




























































-- Injective type families (2016) {{{1
type family Inj1 a -- = r | r -> a where





























































-- Shouldn't this be injective? {{{1

-- type family Inj2 a = r | r -> a where
--   Inj2 (Maybe a) = a





























































-- Deconstructing applications {{{1






























































-- UnApply part 1 {{{1
type family UnApply1 (x :: Type) :: Type where
  --





























































-- UnApply part 2 {{{1
type family UnApply2 (x :: Type) :: Type where
  UnApply2 (f a) = a





























































-- UnApply part 3 {{{1
type family UnApply3 (x :: k) :: Type where
  UnApply3 (f a) = a

-- :kind! UnApply3 ('S 'Z) ?





























































-- UnApply part 4 {{{1
type family UnApply4 (x :: k) :: j where
  UnApply4 (f a) = a -- won't reduce...





























































-- Computing return kinds {{{1
type family UnApply4_p1 (x :: k) :: j where
  UnApply4_p1 (f a) = a

-- Dependent kinds!





























































-- Head {{{1
type family Head (x :: k) :: j where
-- :kind! Head (Either Int String)
-- Either :: Type -> Type -> Type






























































-- But wait a minute... {{{1
type family Head_p1 (x :: k) :: j where
--  Head_p1 (f a) = Head f -- pattern match on an application at _any_ kind
--  Head_p1 f     = f

-- :kind! Head' 'True





























































-- Can we "fake" applications? {{{1
-- Recall:
type family Arg1_p1 (x :: Type) :: Type
-- vs
type family Arg0_p1 :: Type -> Type




























































-- Can we "fake" applications? {{{1
type family Boolish :: Nat -> Bool where
-- Boolish is stuck... no inhabitants!





























































-- How many inhabitants of Bool are there? {{{1
type family Wat (b :: Bool) :: Nat where
  Wat 'False = 0
  Wat 'True  = 1
  --




























































-- How many inhabitants of Bool are there? {{{1
type family Boolish_p1 :: Nat -> Bool where





























































-- How many inhabitants of k are there? {{{1
type family Foolish :: Nat -> k where





























































-- How many inhabitants of k are there? {{{1
type family Wat' (b :: k) :: Nat where
  Wat' (f n)  = n
