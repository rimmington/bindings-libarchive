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

module Codec.Archive.Simple ( Archive, Reading, Writing
                            , withWriteArchive, addFromDisk, addRegularBytes
                            , withReadArchive, readNextEntry ) where

import Codec.Archive.FFI
import Codec.Archive.Internal
import Codec.Archive.Types

import Control.Exception (bracket, bracketOnError)
import Control.Monad ((<=<), when)
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.ByteString.Unsafe (unsafeUseAsCString, unsafePackMallocCStringLen)
import Data.Foldable (forM_)
import Data.Word (Word8)
import Foreign (Ptr, nullPtr, allocaBytes, castPtr, free, mallocBytes)
import Foreign.C.String (withCString)
import Foreign.C.Types (CSize)
import System.Posix.IO (closeFd)
import System.Posix.Types (Fd (Fd))

data Reading
data Writing

-- | A mutable archive.
newtype Archive s = AP (Ptr PArchive)

-- | Run some action with a new archive on disk.
withWriteArchive :: Format -> Compression -> FilePath -> (Archive Writing -> IO a) -> IO a
withWriteArchive format comp path f = bracket archiveWriteNew archiveWriteFree go
  where
    go archive = do
        setFormat format archive
        setCompression comp archive
        withCString path $ ensureSuccess archive <=< archiveWriteOpenFilename archive
        r <- f $ AP archive
        ensureSuccess archive =<< archiveWriteClose archive
        pure r

-- | Add an on-disk file to the archive. Doesn't check if you're adding eg. a socket.
addFromDisk :: Archive Writing
            -> FilePath   -- ^ The on-disk path
            -> FilePath   -- ^ The archive path
            -> IO ()
addFromDisk (AP archive) fp ap = bracket archiveReadDiskNew archiveReadFree $ \ard ->
    withEntry $ \entry -> do
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

-- | Add a regular file to the archive. The 'entryLength' is ignored and
-- 'BS.length' used instead.
addRegularBytes :: Archive Writing -> EntryStat -> ByteString -> IO ()
addRegularBytes (AP archive) stat bs = bracket archiveEntryNew archiveEntryFree $ \entry -> do
    let len   = BS.length bs
    setEntry entry $ stat { entryLength = (fromIntegral len) }
    ensureSuccess archive =<< archiveWriteHeader archive entry
    bytesWritten <- ensuringSuccess archive <=< unsafeUseAsCString bs $ \buf ->
        archiveWriteData archive (castPtr buf) (fromIntegral len)
    when (fromIntegral len /= bytesWritten) $ error "addRegularBytes: did not write all bytes"

-- TODO: addSymlink
-- TODO: addDirectory

withReadArchive :: FilePath -> (Archive Reading -> IO a) -> IO a
withReadArchive fp f = bracket
    archiveReadNew
    (\ar -> ensureSuccess ar =<< archiveReadFree ar)
    $ \ar -> do
        openDiskArchive ar fp
        f (AP ar)

readNextEntry :: Archive Reading -> IO (Maybe (EntryStat, ByteString))
readNextEntry (AP ar) = withEntry $ \entry -> do
    eof <- fmap isEof . ensuringSuccess ar =<< archiveReadNextHeader2 ar entry
    if eof
        then pure Nothing
        else do
            stat <- readEntry entry
            let len = entryLength stat
            bs <- mkNewByteString (fromIntegral len) $ \ptr -> do
                bytesRead <- ensuringSuccess ar =<< archiveReadData ar ptr (fromIntegral len)
                when (bytesRead /= fromIntegral len) $ error "readNextEntry: did not read all bytes"
            pure $ Just (stat, bs)

withEntry :: (Ptr PEntry -> IO a) -> IO a
withEntry = bracket archiveEntryNew archiveEntryFree

mkNewByteString :: Int -> (Ptr Word8 -> IO ()) -> IO ByteString
mkNewByteString len f = bracketOnError
    (mallocBytes len)
    free
    (\ptr -> f ptr *> unsafePackMallocCStringLen (castPtr ptr, len))
