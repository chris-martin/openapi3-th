module Oath.Uri.Scheme (
  schemeGrammar,
) where

import Essentials

import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Optics

import Oath.Abnf
import Oath.Grammar

schemeGrammar ∷ Grammar ByteString
schemeGrammar =
  label "scheme"
    $ prismGrammar
      ( prism'
          (\(x :& xs) → BS.pack $ x : xs)
          (fmap (\(x, xs) → x :& BS.unpack xs) . BS.uncons)
      )
    $ alphaGrammar
      <+> listGrammar
        ( grammarAlternatives
            [ alphaGrammar
            , digitCharGrammar
            , tokenEnumeration $ char <$> "+-."
            ]
        )
