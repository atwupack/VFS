{-# LANGUAGE TypeFamilies, FlexibleContexts #-}
module System.IO.VFS
(
    VFS(..), VFSExcept(..), runVFS,  config, WrappingVFS(..), runInnerFS,
    rmTree
)
where

import Control.Monad.Trans.Except
import Control.Monad.Trans.Reader
import Control.Monad.Trans.Class
import Control.Monad.IO.Class

data VFSExcept = FILE_NOT_FOUND | NOT_A_FILE | NOT_A_DIRECTORY | UNSUPPORTED_OP
    deriving (Eq, Show)

class VFS a where
    type File a :: *
    type Directory a :: *
    lsFiles :: Directory a -> VFSM a [File a]
    lsDirs :: Directory a -> VFSM a [Directory a]
    mkDir :: Directory a -> String -> VFSM a (Directory a)
    rmDir :: Directory a -> VFSM a ()
    rmFile :: File a -> VFSM a ()
    file :: FilePath -> VFSM a (File a)
    dir :: FilePath -> VFSM a (Directory a)

rmTree :: (VFS a) => Directory a -> VFSM a ()
rmTree dir = do
    dirs <- lsDirs dir
    mapM_ rmTree dirs
    files <- lsFiles dir
    mapM_ rmFile files
    rmDir dir

type VFSM f a = ExceptT VFSExcept (ReaderT f IO) a

config :: (VFS f) => VFSM f f
config = lift ask

runVFS :: (VFS f) => f -> VFSM f a -> IO (Either VFSExcept a)
runVFS fs d = runReaderT (runExceptT d) fs

class (VFS (Inner a), VFS a) => WrappingVFS a where
    type Inner a :: *
    innerFS :: a -> Inner a

runInnerFS :: (WrappingVFS f) => f -> VFSM (Inner f) a -> VFSM f a
runInnerFS ofs op = do
    result <- liftIO $ runVFS ifs op
    case result of
        Left e -> throwE e
        Right r -> return r
    where
        ifs = innerFS ofs
