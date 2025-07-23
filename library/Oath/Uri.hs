-- | <https://www.ietf.org/rfc/rfc3986.txt>
module Oath.Uri (
  -- * Types
  Uri (..),
  AbsoluteUri (..),
  RelativeRef (..),
  HierPart (..),
  Authority (..),
  Host (..),

  -- * Render
  renderHost,

  -- * Logic
  resolveUriReference,
) where

import Oath.Uri.AbsoluteUri
import Oath.Uri.Authority
import Oath.Uri.HierPart
import Oath.Uri.Host
import Oath.Uri.RelativeRef
import Oath.Uri.RelativeResolution
import Oath.Uri.Uri
