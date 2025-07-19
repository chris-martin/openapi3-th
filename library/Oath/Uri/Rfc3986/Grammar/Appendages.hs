-- | Note that "appendage" is not a term from the RFC.
module Oath.Uri.Rfc3986.Grammar.Appendages where

import Essentials

import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Optics

import Oath.Abnf.Rfc2234
import Oath.Grammar
import Oath.Uri.Rfc3986.Grammar.Characters

queryGrammar ∷ Grammar ByteString
queryGrammar = label "query" appendageGrammar

fragmentGrammar ∷ Grammar ByteString
fragmentGrammar = label "fragment" appendageGrammar

appendageGrammar ∷ Grammar ByteString
appendageGrammar =
  isoGrammar (iso BS.unpack BS.pack) $
    listGrammar $
      grammarAlternatives
        [ pcharGrammar
        , tokenEnumeration $ char <$> "/?"
        ]
