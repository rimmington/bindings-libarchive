{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE Safe #-}
{-# OPTIONS_HADDOCK show-extensions #-}

{-|
Module      : $Header$
Copyright   : (c) 2015 Swinburne Software Innovation Lab
License     : Proprietary

Maintainer  : Rhys Adams <rhysadams@swin.edu.au>
Stability   : unstable
Portability : unportable (POSIX)

FFI to libarchive.
-}

module Codec.Archive.FFI where

-- TODO: look into which calls to mark unsafe

import Data.Word (Word8)
import Foreign (Ptr)
import Foreign.C.String (CString)
import Foreign.C.Types (CInt (..), CSize (..), CLong (..), CLLong (..), CTime (..))
import System.Posix.Types ( FileMode, EpochTime, UserID, GroupID
                          , CUid (..), CGid (..), CMode (..), CSsize (..) )

#include "archive.h"
#include "archive_entry.h"
#include "fcntl.h"


-- * Types


-- | Phantom type for archive 'Ptr's.
data PArchive
-- | Phantom type for entry 'Ptr's.
data PEntry
-- | Phantom type for POSIX stat 'Ptr's.
data CStat'
-- | The base package doesn't contain an explicit int64_t.
type CInt64 = CLLong

-- * 'PEntry creation and access


-- | <https://github.com/libarchive/libarchive/wiki/ManPageArchiveEntry3>
foreign import ccall "archive.h archive_entry_new"
    archiveEntryNew :: IO (Ptr PEntry)

-- | <https://github.com/libarchive/libarchive/wiki/ManPageArchiveEntry3>
foreign import ccall "archive.h archive_entry_free"
    archiveEntryFree :: Ptr PEntry -> IO ()

-- | <https://github.com/libarchive/libarchive/wiki/ManPageArchiveEntry3>
foreign import ccall "archive.h archive_entry_clear"
    archiveEntryClear :: Ptr PEntry -> IO (Ptr PEntry)

-- | <https://github.com/libarchive/libarchive/wiki/ManPageArchiveEntryStat3>
foreign import ccall "archive.h archive_entry_filetype"
    archiveEntryFiletype :: Ptr PEntry -> IO CMode

-- | <https://github.com/libarchive/libarchive/wiki/ManPageArchiveEntryStat3>
foreign import ccall "archive.h archive_entry_set_filetype"
    archiveEntrySetFiletype :: Ptr PEntry -> FileMode -> IO ()

-- | https://github.com/libarchive/libarchive/wiki/ManPageArchiveEntryPaths3
foreign import ccall "archive.h archive_entry_pathname"
    archiveEntryPathname :: Ptr PEntry -> IO CString

-- | <https://github.com/libarchive/libarchive/wiki/ManPageArchiveEntryPaths3>
foreign import ccall "archive.h archive_entry_copy_pathname"
    archiveEntryCopyPathname :: Ptr PEntry -> CString -> IO ()

-- | <https://github.com/libarchive/libarchive/wiki/ManPageArchiveEntryStat3>
foreign import ccall "archive.h archive_entry_size"
    archiveEntrySize :: Ptr PEntry -> IO CInt64

-- | <https://github.com/libarchive/libarchive/wiki/ManPageArchiveEntryStat3>
foreign import ccall "archive.h archive_entry_set_size"
    archiveEntrySetSize :: Ptr PEntry -> CInt64 -> IO ()

-- | <https://github.com/libarchive/libarchive/wiki/ManPageArchiveEntryPerms3>
foreign import ccall "archive.h archive_entry_perm"
    archiveEntryPerm :: Ptr PEntry -> IO FileMode

-- | <https://github.com/libarchive/libarchive/wiki/ManPageArchiveEntryPerms3>
foreign import ccall "archive.h archive_entry_set_perm"
    archiveEntrySetPerm :: Ptr PEntry -> FileMode -> IO ()

-- | <https://github.com/libarchive/libarchive/wiki/ManPageArchiveEntryTime3>
foreign import ccall "archive.h archive_entry_mtime"
    archiveEntryMtime :: Ptr PEntry -> IO EpochTime

-- | <https://github.com/libarchive/libarchive/wiki/ManPageArchiveEntryTime3>
foreign import ccall "archive.h archive_entry_mtime_nsec"
    archiveEntryMtimeNsec :: Ptr PEntry -> IO CLong

-- | <https://github.com/libarchive/libarchive/wiki/ManPageArchiveEntryTime3>
foreign import ccall "archive.h archive_entry_set_mtime"
    archiveEntrySetMtime :: Ptr PEntry -> EpochTime -> CLong -> IO ()

-- | <https://github.com/libarchive/libarchive/wiki/ManPageArchiveEntryPerms3>
foreign import ccall "archive.h archive_entry_uid"
    archiveEntryUid :: Ptr PEntry -> IO UserID

-- | <https://github.com/libarchive/libarchive/wiki/ManPageArchiveEntryPerms3>
foreign import ccall "archive.h archive_entry_set_uid"
    archiveEntrySetUid :: Ptr PEntry -> UserID -> IO ()

-- | <https://github.com/libarchive/libarchive/wiki/ManPageArchiveEntryPerms3>
foreign import ccall "archive.h archive_entry_gid"
    archiveEntryGid :: Ptr PEntry -> IO GroupID

-- | <https://github.com/libarchive/libarchive/wiki/ManPageArchiveEntryPerms3>
foreign import ccall "archive.h archive_entry_set_gid"
    archiveEntrySetGid :: Ptr PEntry -> GroupID -> IO ()


-- * Archive reading


-- | <https://github.com/libarchive/libarchive/wiki/ManPageArchiveReadNew3>
foreign import ccall "archive.h archive_read_new"
    archiveReadNew :: IO (Ptr PArchive)

-- | <https://github.com/libarchive/libarchive/wiki/ManPageArchiveReadFree3>
foreign import ccall "archive.h archive_read_free"
    archiveReadFree :: Ptr PArchive -> IO CInt

-- | <https://github.com/libarchive/libarchive/wiki/ManPageArchiveReadFilter3>
foreign import ccall "archive.h archive_read_support_filter_all"
    archiveReadSupportFilterAll :: Ptr PArchive -> IO ()

-- | <https://github.com/libarchive/libarchive/wiki/ManPageArchiveReadFormat3>
foreign import ccall "archive.h archive_read_support_format_all"
    archiveReadSupportFormatAll :: Ptr PArchive -> IO ()

-- | <https://github.com/libarchive/libarchive/wiki/ManPageArchiveReadOpen3>
foreign import ccall "archive.h archive_read_open_filename"
    archiveReadOpenFilename :: Ptr PArchive -> CString -> CSize -> IO CInt

-- | <https://github.com/libarchive/libarchive/wiki/ManPageArchiveReadHeader3>
foreign import ccall "archive.h archive_read_next_header2"
    archiveReadNextHeader2 :: Ptr PArchive -> Ptr PEntry -> IO CInt

-- | <https://github.com/libarchive/libarchive/wiki/ManPageArchiveReadData3>
foreign import ccall "archive.h archive_read_data"
    archiveReadData :: Ptr PArchive -> Ptr Word8 -> CSize -> IO CSsize

-- | 'archive_eof' is returned only from 'archiveReadData' when you reach the
-- end of the data in an entry or from 'archiveReadNextHeader2' when you
-- reach the end of the archive.
-- <https://github.com/libarchive/libarchive/wiki/Examples>
archive_eof :: CInt
archive_eof = #const ARCHIVE_EOF


-- * Archive writing


-- | <https://github.com/libarchive/libarchive/wiki/ManPageArchiveWriteNew3>
foreign import ccall "archive.h archive_write_new"
    archiveWriteNew :: IO (Ptr PArchive)

-- | <https://github.com/libarchive/libarchive/wiki/ManPageArchiveWriteFilter3>
foreign import ccall "archive.h archive_write_add_filter_gzip"
    archiveWriteAddFilterGzip :: Ptr PArchive -> IO CInt

-- | <https://github.com/libarchive/libarchive/wiki/ManPageArchiveWriteFormat3>
foreign import ccall "archive.h archive_write_set_format_pax_restricted"
    archiveWriteSetFormatPaxRestricted :: Ptr PArchive -> IO CInt

-- | <https://github.com/libarchive/libarchive/wiki/ManPageArchiveWriteOpen3>
foreign import ccall "archive.h archive_write_open_filename"
    archiveWriteOpenFilename :: Ptr PArchive -> CString -> IO CInt

-- | <https://github.com/libarchive/libarchive/wiki/ManPageArchiveWriteHeader3>
foreign import ccall "archive.h archive_write_header"
    archiveWriteHeader :: Ptr PArchive -> Ptr PEntry -> IO CInt

-- | <https://github.com/libarchive/libarchive/wiki/ManPageArchiveWriteData3>
foreign import ccall "archive.h archive_write_data"
    archiveWriteData :: Ptr PArchive -> Ptr Word8 -> CSize -> IO CSsize

-- | <https://github.com/libarchive/libarchive/wiki/ManPageArchiveWriteFree3>
foreign import ccall "archive.h archive_write_close"
    archiveWriteClose :: Ptr PArchive -> IO CInt

-- | <https://github.com/libarchive/libarchive/wiki/ManPageArchiveWriteFree3>
foreign import ccall "archive.h archive_write_free"
    archiveWriteFree :: Ptr PArchive -> IO CInt

-- | <https://github.com/libarchive/libarchive/wiki/ManPageArchiveReadDisk3>
foreign import ccall "archive.h archive_read_disk_new"
    archiveReadDiskNew :: IO (Ptr PArchive)

-- | <https://github.com/libarchive/libarchive/wiki/ManPageArchiveReadDisk3>
foreign import ccall "archive.h archive_read_disk_set_standard_lookup"
    archiveReadDiskSetStandardLookup :: Ptr PArchive -> IO CInt

-- | <https://github.com/libarchive/libarchive/wiki/ManPageArchiveReadDisk3>
foreign import ccall "archive.h archive_read_disk_entry_from_file"
    archiveReadDiskEntryFromFile :: Ptr PArchive -> Ptr PEntry -> CInt -> Ptr CStat' -> IO CInt


-- * Errors


-- | <https://github.com/libarchive/libarchive/wiki/ManPageArchiveUtil3>
foreign import ccall "archive.h archive_error_string"
    archiveErrorString :: Ptr PArchive -> IO CString


-- * Constants


-- | Regular file. <https://github.com/libarchive/libarchive/wiki/ManPageArchiveEntryStat3#general-accessor-functions>
ae_ifreg :: CMode
ae_ifreg = #const AE_IFREG

-- | Symbolic link. <https://github.com/libarchive/libarchive/wiki/ManPageArchiveEntryStat3#general-accessor-functions>
ae_iflnk :: CMode
ae_iflnk = #const AE_IFLNK

-- | Socket. <https://github.com/libarchive/libarchive/wiki/ManPageArchiveEntryStat3#general-accessor-functions>
ae_ifsock :: CMode
ae_ifsock = #const AE_IFSOCK

-- | Character device. <https://github.com/libarchive/libarchive/wiki/ManPageArchiveEntryStat3#general-accessor-functions>
ae_ifchr :: CMode
ae_ifchr = #const AE_IFCHR

-- | Block device. <https://github.com/libarchive/libarchive/wiki/ManPageArchiveEntryStat3#general-accessor-functions>
ae_ifblk :: CMode
ae_ifblk = #const AE_IFBLK

-- | Directory. <https://github.com/libarchive/libarchive/wiki/ManPageArchiveEntryStat3#general-accessor-functions>
ae_ifdir :: CMode
ae_ifdir = #const AE_IFDIR

-- | Named pipe (fifo). <https://github.com/libarchive/libarchive/wiki/ManPageArchiveEntryStat3#general-accessor-functions>
ae_ififo :: CMode
ae_ififo = #const AE_IFIFO

-- | From <http://man7.org/linux/man-pages/man2/open.2.html>:
-- If pathname is a symbolic link, then the open fails.  This is
-- a FreeBSD extension, which was added to Linux in version
-- 2.1.126.  Symbolic links in earlier components of the pathname
-- will still be followed.
o_nofollow :: CInt
o_nofollow = #const O_NOFOLLOW
