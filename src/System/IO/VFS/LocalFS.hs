{-# LANGUAGE TypeFamilies #-}
module System.IO.VFS.LocalFS
(
    LocalFS(..)
)
where

import Control.Monad
import System.IO.VFS
import System.Directory
import System.FilePath
import Control.Monad.Trans.Except

data LocalFS = LocalFS
    deriving (Show)

instance VFS LocalFS where
    data File LocalFS = FileLFS FilePath deriving (Show)
    data Directory LocalFS = DirectoryLFS FilePath deriving (Show)

    files (DirectoryLFS fp) = do
        content <- io $ listDirectory fp
        filelist <- io $ filterM doesFileExist content
        return $ FileLFS <$> filelist

    dirs (DirectoryLFS fp) = do
        content <- io $ listDirectory fp
        filelist <- io $ filterM doesDirectoryExist content
        return $ DirectoryLFS <$> filelist

    mkdir (DirectoryLFS fp) name = do
        io $ createDirectory newDir
        return $ DirectoryLFS newDir
        where
            newDir = fp </> name

    file fp = do
        ex <- io $ doesFileExist fp
        if ex then
            return $ FileLFS fp
            else
            throwE NOT_A_FILE

    dir fp = do
        ex <- io $ doesDirectoryExist fp
        if ex then
            return $ DirectoryLFS fp
            else
            throwE NOT_A_DIRECTORY
