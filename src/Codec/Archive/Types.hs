{-# LANGUAGE Safe #-}
{-# OPTIONS_HADDOCK show-extensions #-}

{-|
Module      : $Header$
Copyright   : (c) 2015 Swinburne Software Innovation Lab
License     : Proprietary

Maintainer  : Rhys Adams <rhysadams@swin.edu.au>
Stability   : unstable
Portability : unportable (POSIX)

Types for working with archives.
-}

module Codec.Archive.Types (
    -- * Archive parameters
      Compression (..), Format (..)
    -- * 'EntryStat' and friends
    , EntryStat (..)
    , CInt64
    , Filetype (..)
    , toFiletype, fromFiletype
    -- * Alas, an exception
    , ArchiveException (..)
    ) where

import Codec.Archive.FFI ( CInt64, ae_ifsock, ae_iflnk, ae_ifreg, ae_ifblk
                         , ae_ifdir, ae_ifchr, ae_ififo )

import Control.Exception (Exception)
import Foreign.C.Types (CLong)
import System.Posix.Types (CMode, UserID, GroupID, EpochTime, FileMode)

-- | An error from libarchive, typically because an input archive is invalid.
data ArchiveException = ArchiveException String deriving (Show)

instance Exception ArchiveException

-- | Supported (de)compression filters.
data Compression = Uncompressed | Gzip
                   deriving (Show, Eq)

-- | Supported archive formats.
data Format = PaxRestricted deriving (Show, Eq)

-- | Types of files that could be archived.
data Filetype = Socket | SymbolicLink | RegularFile | BlockDevice | Directory
              | CharacterDevice | FIFO | Unknown CMode
              deriving (Show, Eq)

-- TODO: handle hard links: https://github.com/libarchive/libarchive/wiki/ManPageArchiveEntryLinkify3
-- TODO: expose symlink targets
-- TODO: handle device numbers
-- TODO: mark Entry(Content) fields strict?
-- | Details about an archive entry.
data EntryStat = EntryStat { archivePath :: FilePath
                           , filetype    :: Filetype
                           , uid         :: UserID
                           , gid         :: GroupID
                           , perm        :: FileMode
                           , mtime       :: (EpochTime, CLong)
                           , entryLength :: CInt64 }
                           deriving (Show, Eq)

-- | Convert the output of 'Codec.Archive.FFI.archiveEntryFiletype' to a
-- 'Filetype'. No bitmasking is done, so this isn't suitable to use directly
-- with a stat call (plus, the libarchive constants may not match your C libs).
toFiletype :: CMode -> Filetype
toFiletype mode
    | mode == ae_ifsock = Socket
    | mode == ae_iflnk  = SymbolicLink
    | mode == ae_ifreg  = RegularFile
    | mode == ae_ifblk  = BlockDevice
    | mode == ae_ifdir  = Directory
    | mode == ae_ifchr  = CharacterDevice
    | mode == ae_ififo  = FIFO
    | otherwise         = Unknown mode

-- | Turn a 'Filetype' into a value ready for writing to an archive.
fromFiletype :: Filetype -> CMode
fromFiletype Socket          = ae_ifsock
fromFiletype SymbolicLink    = ae_iflnk
fromFiletype RegularFile     = ae_ifreg
fromFiletype BlockDevice     = ae_ifblk
fromFiletype Directory       = ae_ifdir
fromFiletype CharacterDevice = ae_ifchr
fromFiletype FIFO            = ae_ififo
fromFiletype (Unknown mode)  = mode
