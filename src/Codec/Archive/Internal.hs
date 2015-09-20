{-# LANGUAGE Safe #-}
{-# OPTIONS_HADDOCK show-extensions #-}

{-|
Module      : $Header$
Copyright   : (c) 2015 Swinburne Software Innovation Lab
License     : Proprietary

Maintainer  : Rhys Adams <rhysadams@swin.edu.au>
Stability   : unstable
Portability : unportable (POSIX)

Functions for working with archives, intended for use by other libraries.
-}

module Codec.Archive.Internal
    ( defaultBuffer, ensureSuccess, ensuringSuccess, isEof
    , setFormat, setCompression, setFiletype, setPathname
    , writeFileData
    , openReadOnlyFdNoFollow
    , openDiskArchive
    , readEntry
    , setEntry
    ) where

import Codec.Archive.FFI
import Codec.Archive.Types

import Control.Exception (throwIO)
import Control.Monad ((>=>), void, when)
import Control.Monad.Loops (iterateUntilM)
import Data.Word (Word8)
import Foreign (Ptr, castPtr)
import Foreign.C.String (CString, withCString, peekCString)
import Foreign.C.Types (CInt, CSize)
import System.Posix.Internals (c_open)
import System.Posix.IO (fdReadBuf)
import System.Posix.Types (Fd (..))

-- | 64k is enough for anyone.
defaultBuffer :: CSize
defaultBuffer = 64 * 1024

-- | Call the correct @archiveWriteSetFormat*@ function.
-- Throws 'ArchiveException' if this fails... somehow.
setFormat :: Format -> Ptr PArchive -> IO ()
setFormat PaxRestricted ar = ensureSuccess ar =<< archiveWriteSetFormatPaxRestricted ar

-- | Call the correct @archiveWriteAddFilter*@ function.
-- Throws 'ArchiveException' if this fails... somehow.
setCompression :: Compression -> Ptr PArchive -> IO ()
setCompression Uncompressed _  = pure ()
setCompression Gzip         ar = ensureSuccess ar =<< archiveWriteAddFilterGzip ar

-- | @
-- setFiletype t p = 'archiveEntrySetFiletype' p $ 'fromFiletype' t
-- @
setFiletype :: Filetype -> Ptr PEntry -> IO ()
setFiletype t p = archiveEntrySetFiletype p $ fromFiletype t

-- | @
-- setPathname ap = 'withCString' ap . 'archiveEntryCopyPathname'
-- @
setPathname :: FilePath -> Ptr PEntry -> IO ()
setPathname ap = withCString ap . archiveEntryCopyPathname

-- | 'archiveWriteData' the contents of an 'Fd' via the provided buffer.
-- Throws 'ArchiveException' on archive write failure, 'IOException' on
-- read failure.
writeFileData :: Ptr PArchive -> Fd -> Ptr Word8 -> CSize -> IO ()
writeFileData archive fd buf bufSize = void $ iterateUntilM (== 0) (doWrite >=> const doRead) =<< doRead
  where
    doWrite bytesRead = do
        bytesWritten <- ensuringSuccess archive =<< archiveWriteData archive buf bytesRead
        -- TODO: this is probably not the correct response
        when (fromIntegral bytesRead /= bytesWritten) $ error "writeFileData: bytesRead /= bytesWritten"
    doRead            = fdReadBuf fd (castPtr buf) bufSize

-- | Open a file for reading, but fail if it's a symlink (or for the usual
-- reasons). Returns 'Nothing' on failure.
openReadOnlyFdNoFollow :: CString -> IO (Maybe Fd)
openReadOnlyFdNoFollow str = do
    res <- c_open str flags mode_w
    if res > 0
        then pure . Just $ Fd res
        else pure Nothing
  where
    flags  = o_nofollow
    mode_w = 0

-- | Open an on-disk archive with any support compression or format using
-- 'defaultBuffer'. Throws 'ArchiveException' on open failure.
openDiskArchive :: Ptr PArchive -> FilePath -> IO ()
openDiskArchive ar fp = do
    archiveReadSupportFilterAll ar
    archiveReadSupportFormatAll ar
    withCString fp $ \cfp ->
        ensureSuccess ar =<< archiveReadOpenFilename ar cfp (64 * 1024)

-- | Populate an 'EntryStat' with metadata from a 'PEntry'.
readEntry :: Ptr PEntry -> IO (EntryStat ())
readEntry ent = do
    ap     <- peekCString =<< archiveEntryPathname ent
    len    <- archiveEntrySize ent
    uid'   <- archiveEntryUid ent
    gid'   <- archiveEntryGid ent
    perm'  <- archiveEntryPerm ent
    mtime' <- (,) <$> archiveEntryMtime ent <*> archiveEntryMtimeNsec ent
    ftype  <- toFiletype <$> archiveEntryFiletype ent
    pure $ EntryStat ap ftype uid' gid' perm' mtime' len ()

-- | Set metadata from the 'EntryStat' to the 'PEntry'.
setEntry :: Ptr PEntry -> EntryStat a -> IO ()
setEntry entry stat = do
    setPathname (archivePath stat) entry
    archiveEntrySetSize entry (entryLength stat)
    setFiletype RegularFile entry
    archiveEntrySetPerm entry $ perm stat
    archiveEntrySetUid  entry $ uid  stat
    archiveEntrySetGid  entry $ gid  stat
    uncurry (archiveEntrySetMtime entry) $ mtime stat

-- | @
-- isEof = (== 'archive_eof')
-- @
isEof :: CInt -> Bool
isEof = (== archive_eof)

-- | @
-- ensureSuccess a = 'void' . 'ensuringSuccess' a
-- @
ensureSuccess :: Ptr PArchive -> CInt -> IO ()
ensureSuccess a = void . ensuringSuccess a

-- | Throw an 'ArchiveException' with libarchive error details if the result
-- was negative. Use like @ensuringSuccess archive =<< archiveFFICall archive foo@.
ensuringSuccess :: (Num r, Ord r) => Ptr PArchive -> r -> IO r
ensuringSuccess a v
    | v >= 0    = pure v
    | otherwise = throwArchiveException a

throwArchiveException :: Ptr PArchive -> IO a
throwArchiveException archive = do
    pstr <- archiveErrorString archive
    str <- peekCString pstr
    throwIO $ ArchiveException str
