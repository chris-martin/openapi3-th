-- | Very straightforward translation of ABNF from
--   <https://www.ietf.org/rfc/rfc3986.txt>
module Oath.Uri.Rfc3986.Grammar (
  Uri (..),
  HierPart (..),
  UriReference (..),
  AbsoluteUri (..),
  RelativeRef (..),
  RelativePart (..),

  -- * Scheme
  schemeGrammar,

  -- * Authority
  Authority (..),
  userinfoGrammar,
  portGrammar,

  -- * Host
  Host (..),
  IpLiteral (..),
  IpvFuture (..),
  ipv6AddressGrammar,
  ipv4AddressGrammar,
  regNameGrammar,

  -- * Paths
  pathAbemptyGrammar,
  pathNoschemeGrammar,
  pathRootlessGrammar,
  pathEmptyGrammar,
  segmentGrammar,
  segmentNzGrammar,
  segmentNzNcGrammar,

  -- * Appendages
  queryGrammar,
  fragmentGrammar,

  -- * Characters
  pctEncodedGrammar,
  unreservedGrammar,
  reservedGrammar,
  genDelimGrammar,
  subDelimGrammar,
  decOctetGrammar,
  pcharGrammar,
) where

import Oath.Uri.Rfc3986.Grammar.Appendages
import Oath.Uri.Rfc3986.Grammar.Authority
import Oath.Uri.Rfc3986.Grammar.Characters
import Oath.Uri.Rfc3986.Grammar.Host
import Oath.Uri.Rfc3986.Grammar.Path
import Oath.Uri.Rfc3986.Grammar.RelativeRef
import Oath.Uri.Rfc3986.Grammar.Scheme
import Oath.Uri.Rfc3986.Grammar.Segment
import Oath.Uri.Rfc3986.Grammar.Uri
import Oath.Uri.Rfc3986.Grammar.UriReference
