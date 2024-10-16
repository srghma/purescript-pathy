module Pathy.Sandboxed
  ( SandboxedPath
  , sandbox
  , sandboxAny
  , sandboxRoot
  , unsandbox
  , class SafeAppend
  , safeAppendPath
  , safeAppendPath'
  , (<///>)
  ) where

import Prelude

import Data.Maybe (Maybe(..))
import Data.Maybe as Maybe
import Pathy.Name (class IsName, Name, reflectName)
import Pathy.Path (Path, appendPath, dir', file', foldPath, relativeTo, rootDir, (</>))
import Pathy.Phantom (class IsDirOrFile, class IsRelOrAbs, Abs, Dir, File, Rel, onRelOrAbs)
import Unsafe.Coerce (unsafeCoerce)

-- | The type for paths that have been sandboxed.
data SandboxedPath dirOrFile = SandboxedPath (Path Abs Dir) (Path Abs dirOrFile)

derive instance (IsDirOrFile dirOrFile) => Eq (SandboxedPath dirOrFile)
derive instance (IsDirOrFile dirOrFile) => Ord (SandboxedPath dirOrFile)
instance (IsDirOrFile dirOrFile) => Show (SandboxedPath dirOrFile) where
  show (SandboxedPath root path) = "(SandboxedPath " <> show root <> " " <> show path <> ")"

-- | Attempts to sandbox a path relative to an absolute directory ("sandbox
-- | root"). If the `Path relOrAbs dirOrFile` escapes the sandbox root `Nothing` will be
-- | returned.
sandbox
  :: forall dirOrFile relOrAbs
   . IsDirOrFile dirOrFile
  => IsRelOrAbs relOrAbs
  => Path Abs Dir
  -> Path relOrAbs dirOrFile
  -> Maybe (SandboxedPath dirOrFile)
sandbox root = map (SandboxedPath root) <<< onRelOrAbs onRel onAbs
  where
  onRel :: (Path Rel dirOrFile -> Path relOrAbs dirOrFile) -> Path Rel dirOrFile -> Maybe (Path Abs dirOrFile)
  onRel _coercePath relPath = onAbs' (root </> relPath)

  onAbs :: (Path Abs dirOrFile -> Path relOrAbs dirOrFile) -> Path Abs dirOrFile -> Maybe (Path Abs dirOrFile)
  onAbs _coercePath = onAbs'

  onAbs' :: Path Abs dirOrFile -> Maybe (Path Abs dirOrFile)
  onAbs' absPath = if goesUp (absPath `relativeTo` root) then Nothing else Just absPath

  goesUp :: forall x y. Path x y -> Boolean
  goesUp = foldPath false (const true) (\p _ -> goesUp p)

-- | Sandboxes any path to `/`.
-- |
-- | This should only be used for situations where a path is already constrained
-- | within a system so that access to `/` is safe - for instance, in URIs.
-- |
-- | If the path escapes the sandbox root - rootDir will be returned.
sandboxAny :: forall dirOrFile relOrAbs. IsDirOrFile dirOrFile => IsRelOrAbs relOrAbs => Path relOrAbs dirOrFile -> SandboxedPath dirOrFile
sandboxAny p = onRelOrAbs onRel onAbs p
  where
  onRel :: (Path Rel dirOrFile -> Path relOrAbs dirOrFile) -> Path Rel dirOrFile -> SandboxedPath dirOrFile
  onRel _coercePath relPath = SandboxedPath rootDir (rootDir </> relPath)

  onAbs :: (Path Abs dirOrFile -> Path relOrAbs dirOrFile) -> Path Abs dirOrFile -> SandboxedPath dirOrFile
  onAbs _coercePath = SandboxedPath rootDir

-- | Returns the location a `SandboxedPath` was sandboxed to.
sandboxRoot :: forall dirOrFile. SandboxedPath dirOrFile -> Path Abs Dir
sandboxRoot (SandboxedPath root _) = root

-- | Extracts the original path from a `SandboxedPath`.
unsandbox :: forall dirOrFile. SandboxedPath dirOrFile -> Path Abs dirOrFile
unsandbox (SandboxedPath _ p) = p

------

class SafeAppend dirOrFile where
  safeAppendPath :: SandboxedPath Dir -> Name dirOrFile -> SandboxedPath dirOrFile

instance safeAppendFile :: SafeAppend File where
  safeAppendPath (SandboxedPath root baseDir) name =
    SandboxedPath root (appendPath baseDir (file' name))

-- | Instance for appending a relative directory path
instance safeAppendDir :: SafeAppend Dir where
  safeAppendPath (SandboxedPath root baseDir) name =
    SandboxedPath root (appendPath baseDir (dir' name))

safeAppendPath' :: forall dirOrFile s proxy. SafeAppend dirOrFile => IsName s => SandboxedPath Dir -> proxy s -> SandboxedPath dirOrFile
safeAppendPath' d n = safeAppendPath d (reflectName n)

infixl 6 safeAppendPath' as <///>

-- outerTmpDir :: SandboxedPath Rel Dir
-- outerTmpDir = sandboxAny (currentDir </> dir (Proxy :: _ "tmp") </> dir (Proxy :: _ "dir-entries-test"))
--
-- foo :: SandboxedPath Rel File
-- foo = outerTmpDir <///> (Proxy :: _ "asdf")
--
-- bar :: SandboxedPath Rel File
-- bar = outerTmpDir <///> (Proxy :: _ "asdf") <///> (Proxy :: _ "asdf")
--
-- q :: SandboxedPath Rel Dir
-- q = outerTmpDir <///> (Proxy :: _ "asdf") <///> (Proxy :: _ "asdf")
