-- | Note that "appendage" is not a term from the RFC.
module Oath.Uri.Appendages where

import Essentials

import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Optics

import Oath.Abnf
import Oath.Grammar
import Oath.Uri.Characters

queryGrammar ∷ Grammar ByteString ByteString
queryGrammar = label "query" appendageGrammar

fragmentGrammar ∷ Grammar ByteString ByteString
fragmentGrammar = label "fragment" appendageGrammar

appendageGrammar ∷ Grammar ByteString ByteString
appendageGrammar =
  isoGrammar (iso BS.unpack BS.pack) $
    listGrammar $
      grammarAlternatives
        [ pcharGrammar
        , tokenEnumeration $ char <$> "/?"
        ]
