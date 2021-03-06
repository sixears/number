{-# LANGUAGE InstanceSigs         #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE QuasiQuotes          #-}
{-# LANGUAGE TypeApplications     #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE UnicodeSyntax        #-}

module Number
  ( Absolutable( absT )
  , FromI( fromI, fromI', __fromI, __fromI' )
  , NumSign(..), fromNumSign, toNumSign
  , ToNum( toNum, toNumI, toNumi, toNumN, toNumℕ
         , toNumW8, toNumW16, toNumW32, toNumW64 )
  )
where

import Prelude  ( Int, Integer, Integral, Num, abs, error, fromInteger
                , fromIntegral, maxBound, minBound, toInteger )

-- base --------------------------------

import Data.Bool        ( otherwise )
import Data.Eq          ( Eq )
import Data.Function    ( ($) )
import Data.Int         ( Int8, Int16, Int32, Int64 )
import Data.List        ( and )
import Data.Maybe       ( Maybe( Just, Nothing ) )
import Data.Monoid      ( (<>) )
import Data.Ord         ( Ord, (>), (<) )
import Data.Typeable    ( Typeable, typeOf )
import Data.Word        ( Word, Word8, Word16, Word32, Word64 )
import Numeric.Natural  ( Natural )
import Text.Show        ( Show( show ) )

-- base-unicode-symbols ----------------

import Data.Function.Unicode  ( (∘) )
import Data.Ord.Unicode       ( (≤), (≥) )

--------------------------------------------------------------------------------

{- | Much like `signum`, but using a dedicated, stronger type. -}
data NumSign = MINUS | NOUGHT | PLUS
  deriving (Eq,Show)

toNumSign ∷ (Ord α, Num α) ⇒ α → NumSign
toNumSign a | a < 0     = MINUS
            | a > 0     = PLUS
            | otherwise = NOUGHT

fromNumSign ∷ Num α ⇒ NumSign → α
fromNumSign MINUS  = -1
fromNumSign PLUS   =  1
fromNumSign NOUGHT =  0

------------------------------------------------------------

{- | Basically a superclass of Integral, but without the direct constraints on
     arithmetic operations (so you may, for example, not implement `(+)`.  So
     essentially anything that can be converted to an integral type.  Note that
     conversion to Bounded types is liable to result in wrapping (that is,
     modulo the bounds).  Where negative values are involved in absolute
     conversions, e.g., converting from a negative value of Int* type to Word8,
     representation rather than sign is preserved:

     @
       λ> toNumW8  (-3 :: Int8)
       253
     @
-}
     
class ToNum α where
  toNum   ∷ Num β ⇒ α → β
  toNumI  ∷ α → Integer
  toNumI  = toNum
  toNumi  ∷ α → Int
  toNumi  = toNum
  toNumN  ∷ α → Natural
  toNumN  = toNum
  toNumℕ  ∷ α → Natural
  toNumℕ  = toNum
  toNumW8  ∷ α → Word8
  toNumW8  = toNum
  toNumW16 ∷ α → Word16
  toNumW16 = toNum
  toNumW32 ∷ α → Word32
  toNumW32 = toNum
  toNumW64 ∷ α → Word64
  toNumW64 = toNum

instance ToNum Integer where
  toNum ∷ Num β ⇒ Integer → β
  toNum = fromInteger

instance ToNum Int where
  toNum ∷ Num β ⇒ Int → β
  toNum = fromInteger ∘ toInteger

instance ToNum Natural where
  toNum ∷ Num β ⇒ Natural → β
  toNum = fromInteger ∘ toInteger

instance ToNum Word where
  toNum ∷ Num β ⇒ Word → β
  toNum = fromInteger ∘ toInteger

instance ToNum Word8 where
  toNum ∷ Num β ⇒ Word8 → β
  toNum = fromInteger ∘ toInteger

instance ToNum Word16 where
  toNum ∷ Num β ⇒ Word16 → β
  toNum = fromInteger ∘ toInteger

instance ToNum Word32 where
  toNum ∷ Num β ⇒ Word32 → β
  toNum = fromInteger ∘ toInteger

instance ToNum Word64 where
  toNum ∷ Num β ⇒ Word64 → β
  toNum = fromInteger ∘ toInteger

instance ToNum Int8 where
  toNum ∷ Num β ⇒ Int8 → β
  toNum = fromInteger ∘ toInteger

instance ToNum Int16 where
  toNum ∷ Num β ⇒ Int16 → β
  toNum = fromInteger ∘ toInteger

instance ToNum Int32 where
  toNum ∷ Num β ⇒ Int32 → β
  toNum = fromInteger ∘ toInteger

instance ToNum Int64 where
  toNum ∷ Num β ⇒ Int64 → β
  toNum = fromInteger ∘ toInteger

------------------------------------------------------------

{- | Types that are /potentially/ convertable from an Integral type.  If a
     value cannot safely be converted (would wrap or truncate), return Nothing.
 -}
class Typeable α ⇒ FromI α where
  fromI  ∷ Integral β ⇒ β → Maybe α

  fromI' ∷ Integer → Maybe α
  fromI' = fromI

  __fromI ∷ (Show β, Integral β) ⇒ β → α
  __fromI i = let result = case fromI i of
                             Just x  → x
                             Nothing → error $ "value " <> show i <> " out of "
                                                        <> show (typeOf result)
                                                        <> " range"
               in result

  __fromI' ∷ Integer → α
  __fromI' = __fromI


instance FromI Integer where
  fromI ∷ Integral β ⇒ β → Maybe Integer
  fromI = Just ∘ toInteger

instance FromI Int where
  fromI ∷ Integral β ⇒ β → Maybe Int
  fromI i = if and [ toInteger i ≤ toInteger (maxBound @Int)
                   , toInteger i ≥ toInteger (minBound @Int) ]
            then Just (fromIntegral i)
            else Nothing

instance FromI Natural where
  fromI ∷ Integral β ⇒ β → Maybe Natural
  fromI i = if i ≥ 0
            then Just (fromIntegral i)
            else Nothing

instance FromI Word where
  fromI ∷ Integral β ⇒ β → Maybe Word
  fromI i = if and [ toInteger i ≤ toInteger (maxBound @Word)
                   , toInteger i ≥ toInteger (minBound @Word) ]
            then Just (fromIntegral i)
            else Nothing

instance FromI Word8 where
  fromI ∷ Integral β ⇒ β → Maybe Word8
  fromI i = if and [ toInteger i ≤ toInteger (maxBound @Word8)
                   , toInteger i ≥ toInteger (minBound @Word8) ]
            then Just (fromIntegral i)
            else Nothing

instance FromI Word16 where
  fromI ∷ Integral β ⇒ β → Maybe Word16
  fromI i = if and [ toInteger i ≤ toInteger (maxBound @Word16)
                   , toInteger i ≥ toInteger (minBound @Word16) ]
            then Just (fromIntegral i)
            else Nothing

instance FromI Word32 where
  fromI ∷ Integral β ⇒ β → Maybe Word32
  fromI i = if and [ toInteger i ≤ toInteger (maxBound @Word32)
                   , toInteger i ≥ toInteger (minBound @Word32) ]
            then Just (fromIntegral i)
            else Nothing

instance FromI Word64 where
  fromI ∷ Integral β ⇒ β → Maybe Word64
  fromI i = if and [ toInteger i ≤ toInteger (maxBound @Word64)
                   , toInteger i ≥ toInteger (minBound @Word64) ]
            then Just (fromIntegral i)
            else Nothing

instance FromI Int8 where
  fromI ∷ Integral β ⇒ β → Maybe Int8
  fromI i = if and [ toInteger i ≤ toInteger (maxBound @Int8)
                   , toInteger i ≥ toInteger (minBound @Int8) ]
            then Just (fromIntegral i)
            else Nothing

instance FromI Int16 where
  fromI ∷ Integral β ⇒ β → Maybe Int16
  fromI i = if and [ toInteger i ≤ toInteger (maxBound @Int16)
                   , toInteger i ≥ toInteger (minBound @Int16) ]
            then Just (fromIntegral i)
            else Nothing

instance FromI Int32 where
  fromI ∷ Integral β ⇒ β → Maybe Int32
  fromI i = if and [ toInteger i ≤ toInteger (maxBound @Int32)
                   , toInteger i ≥ toInteger (minBound @Int32) ]
            then Just (fromIntegral i)
            else Nothing

instance FromI Int64 where
  fromI ∷ Integral β ⇒ β → Maybe Int64
  fromI i = if and [ toInteger i ≤ toInteger (maxBound @Int64)
                   , toInteger i ≥ toInteger (minBound @Int64) ]
            then Just (fromIntegral i)
            else Nothing

------------------------------------------------------------

type family   UnsignedType signedType

class Absolutable α where
  absT ∷ α -> (NumSign,UnsignedType α)

----------------------------------------

{- | Simple implementation of `absT` for `FromI` instances. -}
_absT ∷ (FromI β, Show α, Integral α) ⇒ α → (NumSign,β)
_absT i = (toNumSign i, __fromI $ abs i)

type instance UnsignedType Integer = Natural
instance Absolutable Integer where
  absT ∷ Integer → (NumSign,Natural)
  absT = _absT

type instance UnsignedType Int = Word
instance Absolutable Int where
  absT ∷ Int → (NumSign,Word)
  absT = _absT

type instance UnsignedType Int64 = Word64
instance Absolutable Int64 where
  absT ∷ Int64 → (NumSign,Word64)
  absT = _absT

type instance UnsignedType Int32 = Word32
instance Absolutable Int32 where
  absT ∷ Int32 → (NumSign,Word32)
  absT = _absT

type instance UnsignedType Int16 = Word16
instance Absolutable Int16 where
  absT ∷ Int16 → (NumSign,Word16)
  absT = _absT

type instance UnsignedType Int8 = Word8
instance Absolutable Int8 where
  absT ∷ Int8 → (NumSign,Word8)
  absT = _absT


-- that's all, folks! ----------------------------------------------------------
