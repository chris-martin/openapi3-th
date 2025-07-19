module Oath.Uri.Rfc3986.Grammar.Characters where

import Essentials

import Data.Bits (shiftL, shiftR, (.&.))
import Data.Word
import Prelude ((+))

import Oath.Abnf.Rfc2234
import Oath.Grammar
import Optics

pcharGrammar ∷ Grammar Word8
pcharGrammar =
  label "pchar" $
    grammarAlternatives
      [ unreservedGrammar
      , subDelimGrammar
      , tokenEnumeration $ char <$> ":@"
      , pctEncodedGrammar
      ]

pctEncodedGrammar ∷ Grammar Word8
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

unreservedGrammar ∷ Grammar Word8
unreservedGrammar =
  label "unreserved" $
    grammarAlternatives
      [ alphaGrammar
      , digitCharGrammar
      , tokenEnumeration $ char <$> "-._~"
      ]

reservedGrammar ∷ Grammar Word8
reservedGrammar = label "reserved" $ tokenEnumeration $ genDelims <> subDelims

genDelimGrammar ∷ Grammar Word8
genDelimGrammar = label "gen-delims" $ tokenEnumeration genDelims

genDelims ∷ [Word8]
genDelims = char <$> ":/?#[]@"

subDelimGrammar ∷ Grammar Word8
subDelimGrammar = label "sub-delims" $ tokenEnumeration subDelims

subDelims ∷ [Word8]
subDelims = char <$> "!$&'()*+,;="
