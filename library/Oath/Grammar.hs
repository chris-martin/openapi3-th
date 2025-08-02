module Oath.Grammar where

import Essentials

import Control.Applicative (asum, liftA2)
import Control.Monad (guard)
import Data.ByteString (ByteString)
import Data.ByteString.Builder (Builder)
import Data.ByteString.Builder qualified as BSB
import Data.Either (either)
import Data.Foldable (fold, toList)
import Data.Function (const)
import Data.List qualified as List
import Data.List.NonEmpty (NonEmpty ((:|)), nonEmpty)
import Data.Maybe (mapMaybe)
import Data.Sequence (Seq (..))
import Data.Sequence qualified as Seq
import Data.Word
import Optics
import Test.QuickCheck (Gen)
import Test.QuickCheck qualified as QC
import Text.Megaparsec (Parsec)
import Text.Megaparsec qualified as P
import Prelude (String, error)

infixl 2 :&
infixl 4 <+>
infixl 4 +>
infixl 4 <+

data a :& b = a :& b

(<+>) ∷ Grammar a → Grammar b → Grammar (a :& b)
ga <+> gb =
  Grammar
    { render = \(a :& b) → liftA2 (<>) (ga.render a) (gb.render b)
    , parser = (:&) <$> ga.parser <*> gb.parser
    , generator = (:&) <$> ga.generator <*> gb.generator
    }

(+>) ∷ Grammar () → Grammar b → Grammar b
ga +> gb =
  Grammar
    { render = \b → liftA2 (<>) (ga.render ()) (gb.render b)
    , parser = ga.parser *> gb.parser
    , generator = gb.generator
    }

(<+) ∷ Grammar a → Grammar () → Grammar a
ga <+ gb =
  Grammar
    { render = \a → liftA2 (<>) (ga.render a) (gb.render ())
    , parser = ga.parser <* gb.parser
    , generator = ga.generator
    }

data Grammar a
  = Grammar
  { parser ∷ Parsec Void ByteString a
  , generator ∷ Gen a
  , render ∷ a → Maybe Render
  }

data Render = Render
  { canonical ∷ Builder
  , generator ∷ Gen Builder
  }

instance Semigroup Render where
  a <> b =
    Render
      { canonical = a.canonical <> b.canonical
      , generator = liftA2 (<>) a.generator b.generator
      }

instance Monoid Render where
  mempty = Render {canonical = mempty, generator = pure mempty}

renderConst ∷ Builder → Render
renderConst canonical =
  Render {canonical, generator = pure canonical}

renderChoices ∷ [a → Maybe Render] → a → Maybe Render
renderChoices xs x = do
  os@(o :| _) ← nonEmpty $ mapMaybe ($ x) xs
  pure
    Render
      { canonical = o.canonical
      , generator = QC.oneof $ fmap (.generator) $ toList os
      }

grammarRender ∷ Grammar a → a → Maybe Render
grammarRender = (.render)

grammarParser ∷ Grammar a → Parsec Void ByteString a
grammarParser = (.parser)

grammarGenerator ∷ Grammar a → Gen a
grammarGenerator = (.generator)

renderGenerator ∷ Grammar a → Gen Builder
renderGenerator g = do
  a ← g.generator
  case g.render a of
    Nothing → error "generator should only generate renderable values"
    Just r → r.generator

label ∷ String → Grammar a → Grammar a
label l g = g {parser = P.label l g.parser}

tokenEnumeration ∷ [Word8] → Grammar Word8
tokenEnumeration xs =
  tokenPredicate (`List.elem` xs) (QC.elements xs)

tokenPredicate ∷ (Word8 → Bool) → Gen Word8 → Grammar Word8
tokenPredicate f generator =
  Grammar
    { parser = P.satisfy f
    , generator
    , render = \x → do
        guard $ f x
        Just $ renderConst $ BSB.word8 x
    }

grammarAlternatives ∷ [Grammar a] → Grammar a
grammarAlternatives xs =
  Grammar
    { render = \a → asum @[] $ fmap (\x → x.render a) xs
    , parser = asum @[] $ fmap (.parser) xs
    , generator = QC.oneof $ fmap (.generator) xs
    }

prismGrammar ∷ Prism' b a → Grammar a → Grammar b
prismGrammar p Grammar {render, parser, generator} =
  Grammar
    { render = render <=< preview p
    , parser = review p <$> parser
    , generator = review p <$> generator
    }

isoGrammar ∷ Iso' b a → Grammar a → Grammar b
isoGrammar = prismGrammar . castOptic

constGrammar ∷ ByteString → Grammar ()
constGrammar x =
  Grammar
    { render = \() → Just $ renderConst $ BSB.byteString x
    , parser = void $ P.chunk x
    , generator = pure ()
    }

emptyGrammar ∷ Grammar ()
emptyGrammar =
  Grammar
    { render = \() → Just $ renderConst mempty
    , parser = pure ()
    , generator = pure ()
    }

bracketGrammar ∷ ByteString → ByteString → Grammar a → Grammar a
bracketGrammar open close Grammar {render, parser, generator} =
  Grammar
    { render = \x →
        ( \r →
            fold @[]
              [ renderConst $ BSB.byteString open
              , r
              , renderConst $ BSB.byteString close
              ]
        )
          <$> render x
    , parser = P.chunk open *> parser <* P.chunk close
    , generator
    }

listGrammar ∷ Grammar a → Grammar [a]
listGrammar Grammar {render, parser, generator} =
  Grammar
    { render = fmap (fold @[]) . traverse render
    , parser = P.many $ parser
    , generator = QC.listOf generator
    }

seqGrammar ∷ Grammar a → Grammar (Seq a)
seqGrammar Grammar {render, parser, generator} =
  Grammar
    { render = fmap (fold @Seq) . traverse render
    , parser = fmap Seq.fromList $ P.many $ parser
    , generator = fmap Seq.fromList $ QC.listOf generator
    }

list1Grammar ∷ Grammar a → Grammar (NonEmpty a)
list1Grammar Grammar {render, parser, generator} =
  Grammar
    { render = fmap (fold @NonEmpty) . traverse render
    , parser = fmap f $ P.some $ parser
    , generator = fmap f $ QC.listOf1 generator
    }
 where
  f (x : xs) = x :| xs

optionalGrammar ∷ Grammar a → Grammar (Maybe a)
optionalGrammar Grammar {render, parser, generator} =
  Grammar
    { render = fmap (fold @Maybe) . traverse render
    , parser = P.optional parser
    , generator = QC.liftArbitrary generator
    }

forceRenderCanonical ∷ Grammar a → a → Builder
forceRenderCanonical Grammar {render} x =
  let Just Render {canonical} = render x
   in canonical

readGrammarMaybe ∷ Grammar a → ByteString → Maybe a
readGrammarMaybe Grammar {parser} =
  either (const Nothing) Just . P.parse (parser <* P.eof) ""
