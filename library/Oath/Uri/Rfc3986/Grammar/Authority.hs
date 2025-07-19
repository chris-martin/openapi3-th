module Oath.Uri.Rfc3986.Grammar.Authority where

import Essentials

import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Optics
import Test.QuickCheck.Arbitrary.Generic

import Oath.Abnf.Rfc2234
import Oath.Grammar
import Oath.Uri.Rfc3986.Grammar.Characters
import Oath.Uri.Rfc3986.Grammar.Host

data Authority = Authority
  { userinfo ∷ Maybe ByteString
  , host ∷ Host
  , port ∷ Maybe ByteString
  }

makeFieldLabels ''Authority

instance HasGrammar Authority where
  grammar =
    label "authority"
      $ isoGrammar
        ( iso
            (\Authority {userinfo, host, port} → userinfo :& host :& port)
            (\(userinfo :& host :& port) → Authority {userinfo, host, port})
        )
      $ optionalGrammar (userinfoGrammar <+ constGrammar "@")
        <+> grammar
        <+> optionalGrammar (constGrammar ":" +> portGrammar)

userinfoGrammar ∷ Grammar ByteString
userinfoGrammar =
  label "userinfo" $
    isoGrammar (iso BS.unpack BS.pack) $
      listGrammar $
        grammarAlternatives
          [ unreservedGrammar
          , pctEncodedGrammar
          , subDelimGrammar
          , tokenEnumeration $ char <$> ":"
          ]

portGrammar ∷ Grammar ByteString
portGrammar =
  label "port" $
    isoGrammar (iso BS.unpack BS.pack) $
      listGrammar digitCharGrammar

deriving via
  TheGrammar Authority
  instance
    Arbitrary Authority
