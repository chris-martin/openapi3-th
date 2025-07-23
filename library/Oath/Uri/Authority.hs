module Oath.Uri.Authority where

import Essentials

import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Optics
import Test.QuickCheck.Arbitrary.Generic

import Oath.Abnf
import Oath.Grammar
import Oath.Uri.Characters
import Oath.Uri.Host

data Authority = Authority
  { userinfo ∷ Maybe ByteString
  , host ∷ Host
  , port ∷ Maybe ByteString
  }
  deriving stock (Eq, Show)

makeFieldLabels ''Authority

authorityGrammar ∷ Grammar Authority
authorityGrammar =
  label "authority"
    $ isoGrammar
      ( iso
          (\Authority {userinfo, host, port} → userinfo :& host :& port)
          (\(userinfo :& host :& port) → Authority {userinfo, host, port})
      )
    $ optionalGrammar (userinfoGrammar <+ constGrammar "@")
      <+> hostGrammar
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

instance Arbitrary Authority where
  arbitrary = authorityGrammar.generator
