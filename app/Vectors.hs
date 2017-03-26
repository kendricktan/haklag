{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}

module Vectors where

import Control.Applicative
import Control.Monad
import Data.Monoid


data Vector a = Empty | Vector a (Vector a) deriving Show


instance Monoid (Vector a) where
    mempty = Empty

    mappend Empty v            = v
    mappend (Vector a Empty) v = Vector a v
    mappend (Vector a b) v     = Vector a (b <> v)


instance Functor Vector where
    fmap f Empty        = Empty
    fmap f (Vector a b) = Vector (f a) (fmap f b)

    (<$) c Empty        = Empty
    (<$) c (Vector a b) = Vector c (c <$ b)


instance Applicative Vector where
    pure a = Vector a Empty

    (<*>) (Vector f Empty) = fmap f

    (*>) _ b = b
    (<*) a _ = a


instance Monad Vector where
    (>>=) Empty _        = Empty
    (>>=) (Vector a b) f = f a <> (b >>= f)


instance Foldable Vector where
    foldMap f Empty        = mempty
    foldMap f (Vector a b) = f a <> foldMap f b


instance Num a => Num (Vector a) where
    (*) Empty a                   = a
    (*) a Empty                   = a
    (*) (Vector a b) (Vector c d) = Vector (a * c) (b * d)

    (+) Empty a                   = a
    (+) a Empty                   = a
    (+) (Vector a b) (Vector c d) = Vector (a + c) (b + d)


dotProduct :: Num a => Vector a -> Vector a -> a
dotProduct v1 v2 = sum $ v1 * v2
