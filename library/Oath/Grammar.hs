module Oath.Grammar where

import Essentials

import Control.Applicative (asum, liftA2)
import Control.Exception (Exception, throw)
import Control.Monad (guard)
import Data.ByteString (ByteString)
import Data.ByteString.Builder ()
import Data.ByteString.Builder qualified as BSB
import Data.Either (either)
import Data.Foldable (fold, toList)
import Data.Function (const)
import Data.List qualified as List
import Data.List.NonEmpty (NonEmpty ((:|)), nonEmpty)
import Data.Maybe (mapMaybe)
import Data.Sequence (Seq (..))
import Data.Sequence qualified as Seq
import Data.Text (Text)
import Data.Text.Lazy.Builder qualified as TB
import Data.Type.Equality
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

class (P.Stream s, P.Tokens s ~ s, Monoid s, Monoid (Builder s)) ⇒ StringLike s where
  type Builder s ∷ Type
  toBuilder ∷ s → Builder s
  tokenBuilder ∷ P.Token s → Builder s

instance StringLike ByteString where
  type Builder ByteString = BSB.Builder
  toBuilder = BSB.byteString
  tokenBuilder = BSB.word8

instance StringLike Text where
  type Builder Text = TB.Builder
  toBuilder = TB.fromText
  tokenBuilder = TB.singleton

(<+>) ∷ StringLike s ⇒ Grammar s a → Grammar s b → Grammar s (a :& b)
ga <+> gb =
  Grammar
    { render = \(a :& b) → liftA2 (<>) (ga.render a) (gb.render b)
    , parser = (:&) <$> ga.parser <*> gb.parser
    , generator = (:&) <$> ga.generator <*> gb.generator
    }

(+>) ∷ StringLike s ⇒ Grammar s () → Grammar s b → Grammar s b
ga +> gb =
  Grammar
    { render = \b → liftA2 (<>) (ga.render ()) (gb.render b)
    , parser = ga.parser *> gb.parser
    , generator = gb.generator
    }

(<+) ∷ StringLike s ⇒ Grammar s a → Grammar s () → Grammar s a
ga <+ gb =
  Grammar
    { render = \a → liftA2 (<>) (ga.render a) (gb.render ())
    , parser = ga.parser <* gb.parser
    , generator = ga.generator
    }

data Grammar s a
  = Grammar
  { parser ∷ Parsec Void s a
  , generator ∷ Gen a
  , render ∷ a → Maybe (Render s)
  }

data Render s = Render
  { canonical ∷ Builder s
  , generator ∷ Gen (Builder s)
  }

instance StringLike s ⇒ Semigroup (Render s) where
  a <> b =
    Render
      { canonical = a.canonical <> b.canonical
      , generator = liftA2 (<>) a.generator b.generator
      }

instance StringLike s ⇒ Monoid (Render s) where
  mempty = Render {canonical = mempty, generator = pure mempty}

renderConst ∷ Builder s → Render s
renderConst canonical =
  Render {canonical, generator = pure canonical}

renderChoices ∷ [a → Maybe (Render s)] → a → Maybe (Render s)
renderChoices xs x = do
  os@(o :| _) ← nonEmpty $ mapMaybe ($ x) xs
  pure
    Render
      { canonical = o.canonical
      , generator = QC.oneof $ fmap (.generator) $ toList os
      }

grammarRender ∷ Grammar s a → a → Maybe (Render s)
grammarRender = (.render)

grammarParser ∷ Grammar s a → Parsec Void s a
grammarParser = (.parser)

grammarGenerator ∷ Grammar s a → Gen a
grammarGenerator = (.generator)

renderGenerator ∷ Grammar s a → Gen (Builder s)
renderGenerator g = do
  a ← g.generator
  case g.render a of
    Nothing → error "generator should only generate renderable values"
    Just r → r.generator

label ∷ StringLike s ⇒ String → Grammar s a → Grammar s a
label l g = g {parser = P.label l g.parser}

tokenEnumeration ∷ StringLike s ⇒ [P.Token s] → Grammar s (P.Token s)
tokenEnumeration xs =
  tokenPredicate (`List.elem` xs) (QC.elements xs)

tokenPredicate ∷ ∀ s. StringLike s ⇒ (P.Token s → Bool) → Gen (P.Token s) → Grammar s (P.Token s)
tokenPredicate f generator =
  Grammar
    { parser = P.satisfy f
    , generator
    , render = \x → do
        guard $ f x
        Just $ renderConst $ tokenBuilder @s x
    }

grammarAlternatives ∷ StringLike s ⇒ [Grammar s a] → Grammar s a
grammarAlternatives xs =
  Grammar
    { render = \a → asum @[] $ fmap (\x → x.render a) xs
    , parser = asum @[] $ fmap (.parser) xs
    , generator = QC.oneof $ fmap (.generator) xs
    }

prismGrammar ∷ Prism' b a → Grammar s a → Grammar s b
prismGrammar p Grammar {render, parser, generator} =
  Grammar
    { render = render <=< preview p
    , parser = review p <$> parser
    , generator = review p <$> generator
    }

isoGrammar ∷ Iso' b a → Grammar s a → Grammar s b
isoGrammar = prismGrammar . castOptic

constGrammar ∷ StringLike s ⇒ s → Grammar s ()
constGrammar x =
  Grammar
    { render = \() → Just $ renderConst $ toBuilder x
    , parser = void $ P.chunk x
    , generator = pure ()
    }

emptyGrammar ∷ StringLike s ⇒ Grammar s ()
emptyGrammar =
  Grammar
    { render = \() → Just $ renderConst mempty
    , parser = pure ()
    , generator = pure ()
    }

bracketGrammar ∷ StringLike s ⇒ s → s → Grammar s a → Grammar s a
bracketGrammar open close Grammar {render, parser, generator} =
  Grammar
    { render = \x →
        ( \r →
            fold @[]
              [ renderConst $ toBuilder open
              , r
              , renderConst $ toBuilder close
              ]
        )
          <$> render x
    , parser = P.chunk open *> parser <* P.chunk close
    , generator
    }

listGrammar ∷ StringLike s ⇒ Grammar s a → Grammar s [a]
listGrammar Grammar {render, parser, generator} =
  Grammar
    { render = fmap (fold @[]) . traverse render
    , parser = P.many $ parser
    , generator = QC.listOf generator
    }

seqGrammar ∷ StringLike s ⇒ Grammar s a → Grammar s (Seq a)
seqGrammar Grammar {render, parser, generator} =
  Grammar
    { render = fmap (fold @Seq) . traverse render
    , parser = fmap Seq.fromList $ P.many $ parser
    , generator = fmap Seq.fromList $ QC.listOf generator
    }

list1Grammar ∷ StringLike s ⇒ Grammar s a → Grammar s (NonEmpty a)
list1Grammar Grammar {render, parser, generator} =
  Grammar
    { render = fmap (fold @NonEmpty) . traverse render
    , parser = fmap f $ P.some $ parser
    , generator = fmap f $ QC.listOf1 generator
    }
 where
  f (x : xs) = x :| xs
  f [] = undefined

optionalGrammar ∷ StringLike s ⇒ Grammar s a → Grammar s (Maybe a)
optionalGrammar Grammar {render, parser, generator} =
  Grammar
    { render = fmap (fold @Maybe) . traverse render
    , parser = P.optional parser
    , generator = QC.liftArbitrary generator
    }

forceRenderCanonical ∷ Grammar s a → a → Builder s
forceRenderCanonical Grammar {render} x =
  case render x of
    Just Render {canonical} → canonical
    Nothing → throw Unrenderable

data Unrenderable = Unrenderable
  deriving stock (Eq, Show)
  deriving anyclass Exception

readGrammarMaybe ∷ StringLike s ⇒ Grammar s a → s → Maybe a
readGrammarMaybe Grammar {parser} =
  either (const Nothing) Just . P.parse (parser <* P.eof) ""
