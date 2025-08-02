module Oath.Uri.Characters where

import Essentials

import Data.Bits (shiftL, shiftR, (.&.))
import Data.ByteString (ByteString)
import Data.Word
import Optics
import Prelude ((+))

import Oath.Abnf
import Oath.Grammar

pcharGrammar ∷ Grammar ByteString Word8
pcharGrammar =
  label "pchar" $
    grammarAlternatives
      [ unreservedGrammar
      , subDelimGrammar
      , tokenEnumeration $ char <$> ":@"
      , pctEncodedGrammar
      ]

pctEncodedGrammar ∷ Grammar ByteString Word8
pctEncodedGrammar =
  label "pct-encoded"
    $ isoGrammar
      ( iso
          (\x → (x `shiftR` 4) :& (x .&. 15))
          (\(a :& b) → (a `shiftL` 4) + b)
      )
    $ constGrammar "%"
      +> hexdigNumGrammar UpperCase
      <+> hexdigNumGrammar UpperCase

unreservedGrammar ∷ Grammar ByteString Word8
unreservedGrammar =
  label "unreserved" $
    grammarAlternatives
      [ alphaGrammar
      , digitCharGrammar
      , tokenEnumeration $ char <$> "-._~"
      ]

reservedGrammar ∷ Grammar ByteString Word8
reservedGrammar = label "reserved" $ tokenEnumeration $ genDelims <> subDelims

genDelimGrammar ∷ Grammar ByteString Word8
genDelimGrammar = label "gen-delims" $ tokenEnumeration genDelims

genDelims ∷ [Word8]
genDelims = char <$> ":/?#[]@"

subDelimGrammar ∷ Grammar ByteString Word8
subDelimGrammar = label "sub-delims" $ tokenEnumeration subDelims

subDelims ∷ [Word8]
subDelims = char <$> "!$&'()*+,;="
