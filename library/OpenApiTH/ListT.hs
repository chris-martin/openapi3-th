module OpenApiTH.ListT where

import Essentials

import Data.ByteString (ByteString)
import Data.ByteString.Builder qualified as BSB
import Data.ByteString.Lazy (LazyByteString)
import List.Transformer (ListT)
import List.Transformer qualified as ListT

foldBsList ∷ Monad m ⇒ ListT m ByteString → m LazyByteString
foldBsList = ListT.fold (\x a → x <> BSB.byteString a) mempty BSB.toLazyByteString
