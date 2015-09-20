{-# LANGUAGE OverloadedStrings #-}

module Codec.Archive.WriteSpec where

import Codec.Archive.Types ( Compression (Gzip), Format (PaxRestricted)
                           , Filetype (RegularFile), EntryStat (..) )
import Codec.Archive.Write (withWriteArchive, addRegularBytes, addFromDisk)

import Codec.Archive (sourceArchive)
import Codec.Archive.Read (readArchive, getNextEntry, archiveReadFree)
import Conduit (($$), findC, runResourceT)
import Control.Exception (bracket)
import qualified Data.ByteString as BS
import Data.Maybe (isJust)
import System.Directory (createDirectory)
import System.FilePath ((</>))
import System.IO.Temp (withSystemTempDirectory)

import Test.Hspec

findFile :: FilePath -> FilePath -> IO (Maybe BS.ByteString)
findFile tar ap = runResourceT $ fmap snd <$> (sourceArchive tar $$ findC ((== ap) . fst))

spec :: Spec
spec = do
    describe "addRegularBytes" $
        it "writes a regular file to an archive" $
            withSystemTempDirectory "writespec" $ \tmp -> do
                let bs  = "{\"something\":1}"
                    tar = tmp </> "test.tar"
                    ap  = "manifest"
                    st  = EntryStat ap RegularFile 0 0 0o644 (0, 0) 0
                withWriteArchive PaxRestricted Gzip tar $ \archive ->
                    addRegularBytes archive st bs
                mbs <- findFile tar ap
                mbs `shouldBe` Just bs

    describe "addFromDisk" $ do
        it "writes a file from disk to an archive" $
            withSystemTempDirectory "writespec" $ \tmp -> do
                let ap  = "AnotherName.hs"
                    fp  = "test/Codec/Archive/WriteSpec.hs"
                    tar = tmp </> "test.tar"
                bs <- BS.readFile fp
                withWriteArchive PaxRestricted Gzip tar $ \archive ->
                    addFromDisk archive fp ap
                mbs <- findFile tar ap
                mbs `shouldBe` Just bs

        it "creates directories" $
            withSystemTempDirectory "writespec" $ \tmp -> do
                let fp  = tmp </> "testdir"
                    ap  = "testdir"
                    tar = tmp </> "test.tar"
                createDirectory fp
                withWriteArchive PaxRestricted Gzip tar $ \archive ->
                    addFromDisk archive fp ap
                bracket (readArchive tar) archiveReadFree $ \ptr -> do
                    ent <- getNextEntry ptr
                    ent `shouldSatisfy` isJust
                    let (Just (path, _)) = ent
                    path `shouldBe` "testdir/"
