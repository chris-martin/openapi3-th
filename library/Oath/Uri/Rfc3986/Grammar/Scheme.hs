module Oath.Uri.Rfc3986.Grammar.Scheme (
  schemeGrammar,
) where

import Essentials

import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Optics

import Oath.Abnf.Rfc2234
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
