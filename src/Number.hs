{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE UnicodeSyntax     #-}

module Number
  ( FromI( fromI, fromI', __fromI, __fromI' )
  , ToNum( toNum, toNumI, toNumi, toNumN, toNumℕ
         , toNumW8, toNumW16, toNumW32, toNumW64 ) )
where

import Prelude  ( Int, Integer, Integral, Num, error )

-- base --------------------------------

import Data.Function  ( ($) )
import Data.Maybe     ( Maybe( Just, Nothing ) )
import Data.Typeable  ( Typeable, typeOf )
import Data.Word      ( Word8, Word16, Word32, Word64 )

-- more-unicode ------------------------

import Data.MoreUnicode.Natural  ( ℕ )

-- tfmt --------------------------------

import Text.Fmt  ( fmt )

--------------------------------------------------------------------------------

class ToNum α where
  toNum   ∷ Num β ⇒ α → β
  toNumI  ∷ α → Integer
  toNumI  = toNum
  toNumi  ∷ α → Int
  toNumi  = toNum
  toNumN  ∷ α → ℕ
  toNumN  = toNum
  toNumℕ  ∷ α → ℕ
  toNumℕ  = toNum
  toNumW8  ∷ α → Word8
  toNumW8  = toNum
  toNumW16 ∷ α → Word16
  toNumW16 = toNum
  toNumW32 ∷ α → Word32
  toNumW32 = toNum
  toNumW64 ∷ α → Word64
  toNumW64 = toNum

class Typeable α ⇒ FromI α where
  fromI  ∷ Integral β ⇒ β → Maybe α

  fromI' ∷ Integer → Maybe α
  fromI' = fromI

  __fromI ∷ Integral β ⇒ β → α
  __fromI i = let result = case fromI i of
                             Just x  → x
                             Nothing → error $ [fmt|value %d out of %w range|]
                                               i (typeOf result)
               in result

  __fromI' ∷ Integer → α
  __fromI' = __fromI


-- that's all, folks! ----------------------------------------------------------
