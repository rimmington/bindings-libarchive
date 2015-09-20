{-# LANGUAGE Trustworthy #-}
{-# OPTIONS_HADDOCK show-extensions #-}

{-|
Module      : $Header$
Copyright   : (c) 2015 Swinburne Software Innovation Lab
License     : Proprietary

Maintainer  : Rhys Adams <rhysadams@swin.edu.au>
Stability   : unstable
Portability : unportable (POSIX)

Simple archive writing.
-}

module Codec.Archive.Write ( Compression (..), Format (..), Filetype (..)
                           , ArchivePtr
                           , bytesStat
                           , withWriteArchive, addFromDisk, addRegularBytes ) where

-- TODO: rename to Simple, add reading

import Codec.Archive.FFI
import Codec.Archive.Internal
import Codec.Archive.Types

import Control.Exception (bracket)
import Control.Monad ((<=<), when)
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.ByteString.Unsafe (unsafeUseAsCString)
import Data.Foldable (forM_)
import Foreign (Ptr, nullPtr, allocaBytes, castPtr)
import Foreign.C.String (withCString)
import Foreign.C.Types (CSize)
import System.Posix.IO (closeFd)
import System.Posix.Types (Fd (Fd))

-- TODO: move to Simple, use Free?
-- | The tiniest abstraction.
newtype ArchivePtr = AP (Ptr Archive)

-- | Run some action with a new archive on disk.
withWriteArchive :: Format -> Compression -> FilePath -> (ArchivePtr -> IO a) -> IO a
withWriteArchive format comp path f = bracket archiveWriteNew archiveWriteFree go
  where
    go archive = do
        setFormat format archive
        setCompression comp archive
        withCString path $ ensureSuccess archive <=< archiveWriteOpenFilename archive
        r <- f $ AP archive
        ensureSuccess archive =<< archiveWriteClose archive
        pure r

-- TODO: writeArchiveFiles?
-- TODO: do sockets need to be handled specially? actool does so.
-- | Add an on-disk file to the archive.
addFromDisk :: ArchivePtr
            -> FilePath   -- ^ The on-disk path
            -> FilePath   -- ^ The archive path
            -> IO ()
addFromDisk (AP archive) fp ap = bracket archiveReadDiskNew archiveReadFree $ \ard ->
    bracket archiveEntryNew archiveEntryFree $ \entry -> do
        ensureSuccess ard =<< archiveReadDiskSetStandardLookup ard
        withCString fp $ \cfp -> do
            archiveEntryCopyPathname entry cfp
            bracket (openReadOnlyFdNoFollow cfp) (mapM_ closeFd) $ \mfd -> do
                let fdi = case mfd of
                        Just (Fd i) -> i
                        Nothing     -> -1
                ensureSuccess archive =<< archiveReadDiskEntryFromFile ard entry fdi nullPtr
                setPathname ap entry
                ensureSuccess archive =<< archiveWriteHeader archive entry
                ftype <- toFiletype <$> archiveEntryFiletype entry
                forM_ mfd $ \fd -> when (ftype == RegularFile) .
                    allocaBytes (fromIntegral defaultBuffer) $ \buf ->
                        writeFileData archive fd buf defaultBuffer

-- | 'defEntryStat' for 'ByteString'.
bytesStat :: FilePath -> ByteString -> EntryStat ByteString
bytesStat ap bs = defEntryStat ap (fromIntegral $ BS.length bs) bs

-- | Add a regular file to the archive. The 'entryLength' is ignored and
-- 'BS.length' used instead.
addRegularBytes :: ArchivePtr -> EntryStat ByteString -> IO ()
addRegularBytes (AP archive) stat = bracket archiveEntryNew archiveEntryFree $ \entry -> do
    let len   = BS.length bs
        bs    = content stat
    setEntry entry $ stat { entryLength = (fromIntegral len) }
    ensureSuccess archive =<< archiveWriteHeader archive entry
    bytesWritten <- ensuringSuccess archive <=< unsafeUseAsCString bs $ \buf ->
        archiveWriteData archive (castPtr buf) (fromIntegral len)
    when (fromIntegral len /= bytesWritten) $ error "addRegularBytes: did not write all bytes"

-- TODO: addSymlink
-- TODO: addDirectory
