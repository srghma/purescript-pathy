-- https://github.com/mxswd/data-filepath/blob/master/Data/FilePath.hs
module Pathy.PathWithoutParent where

import Prelude

import Data.Char (toCharCode)
import Data.Either (Either(..))
import Data.Foldable (any)
import Data.Identity (Identity(..))
import Data.Maybe (Maybe(..))
import Data.Maybe (Maybe(..), maybe)
import Data.Newtype (un)
import Data.String.NonEmpty (NonEmptyString)
import Data.String.NonEmpty as NES
import Data.String.NonEmpty as NonEmptyString
import Data.String.NonEmpty.CodeUnits as NonEmptyString
import Data.Symbol (class IsSymbol)
import Data.Tuple (Tuple(..), fst, snd)
import Data.Variant (Variant)
import Partial.Unsafe (unsafeCrashWith)
import Prim.Boolean as Symbol
import Type.Data.Symbol as Symbol
import Type.Prelude (Proxy(..))
import Unsafe.Coerce (unsafeCoerce)

-- :heart: emoji
-- https://hgiasac.github.io/posts/2018-12-18-PureScript-GADTs-Alternatives---Recap.html
-- https://github.com/Kamirus/purescript-functional-concepts/blob/master/src/GADTs/ExprLeibnizTagless.purs

newtype PathSegment = PathSegment NonEmptyString

derive newtype instance Eq PathSegment
derive newtype instance Ord PathSegment
derive newtype instance Show PathSegment

instance semigroupPathSegment :: Semigroup PathSegment where
  append (PathSegment a) (PathSegment b) = PathSegment $ a <> b

segString :: PathSegment -> NonEmptyString
segString (PathSegment s) = s

isControl :: Char -> Boolean
isControl x =
  let
    code = toCharCode x
  in
    code <= 31 || code == 127

isValidPathSegmentChar :: Char -> Boolean
isValidPathSegmentChar x =
  x == '/' || isControl x

mkPathSegment :: String -> Maybe PathSegment
mkPathSegment s = do
  s' <- NonEmptyString.fromString s
  if any isValidPathSegmentChar (NonEmptyString.toNonEmptyCharArray s') then Nothing else Just $ PathSegment s'

eitherPathSegment :: String -> Either String PathSegment
eitherPathSegment s = case mkPathSegment s of
  Just ps -> Right ps
  Nothing -> Left s

-- Phantom types for From and Path
foreign import data Root :: Type
foreign import data Relative :: Type
foreign import data File :: Type
foreign import data Directory :: Type

-- Tagless final encoding
class FilePathF rep where
  rootPath :: rep Root Directory
  relativePath :: rep Relative Directory
  filePath :: forall a. rep a Directory -> PathSegment -> rep a File
  directoryPath :: forall a. rep a Directory -> PathSegment -> rep a Directory

-- Example implementation
newtype FilePathImpl a b = FilePathImpl (Array PathSegment)

instance filePathFImpl :: FilePathF FilePathImpl where
  rootPath = FilePathImpl []
  relativePath = FilePathImpl []
  filePath (FilePathImpl segments) segment = FilePathImpl (segments <> [ segment ])
  directoryPath (FilePathImpl segments) segment = FilePathImpl (segments <> [ segment ])

class IsPathSegment :: Symbol -> Constraint
class IsPathSegment sym where
  reflectPathSegment :: forall proxy. proxy sym -> PathSegment

instance isPathSegmentNESymbol :: (IsSymbol s, Symbol.Equals s "" Symbol.False) => IsPathSegment s where
  reflectPathSegment _ = asNonEmpty $ Symbol.reflectSymbol (Proxy :: Proxy s)
    where
    asNonEmpty :: forall d. String -> PathSegment
    asNonEmpty = unsafeCoerce

-- Usage example
example :: forall rep. FilePathF rep => rep Root File
example = filePath (directoryPath rootPath (reflectPathSegment (Proxy :: Proxy "home"))) (reflectPathSegment (Proxy :: Proxy "file.txt"))
