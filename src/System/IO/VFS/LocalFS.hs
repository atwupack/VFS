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
import Control.Monad.IO.Class

data LocalFS = LocalFS deriving (Show)
data FileLFS = FileLFS FilePath deriving (Show)
data DirectoryLFS = DirectoryLFS FilePath deriving (Show)

instance VFS LocalFS where
    type File LocalFS = FileLFS
    type Directory LocalFS = DirectoryLFS
    lsFiles (DirectoryLFS fp) = do
        content <- liftIO $ listDirectory fp
        filelist <- liftIO $ filterM doesFileExist content
        return $ FileLFS <$> filelist
    lsDirs (DirectoryLFS fp) = do
        content <- liftIO $ listDirectory fp
        filelist <- liftIO $ filterM doesDirectoryExist content
        return $ DirectoryLFS <$> filelist
    rmFile (FileLFS fp) = do
        liftIO $ removeFile fp
    mkDir (DirectoryLFS fp) name = do
        liftIO $ createDirectory newDir
        return $ DirectoryLFS newDir
        where
            newDir = fp </> name
    rmDir (DirectoryLFS fp) = do
        liftIO $ removeDirectory fp
    file fp = do
        ex <- liftIO $ doesFileExist fp
        if ex then
            return $ FileLFS fp
            else
            throwE NOT_A_FILE
    dir fp = do
        ex <- liftIO $ doesDirectoryExist fp
        if ex then
            return $ DirectoryLFS fp
            else
            throwE NOT_A_DIRECTORY
