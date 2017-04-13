{-# LANGUAGE TypeFamilies, FlexibleContexts #-}
module System.IO.VFS
(
    VFS(..), io, VFSExcept(..), runVFS,  config, WrappingVFS(..), runInnerFS
)
where

import Control.Monad.Trans.Except
import Control.Monad.Trans.Reader
import Control.Monad.Trans.Class

data VFSExcept = FILE_NOT_FOUND | NOT_A_FILE | NOT_A_DIRECTORY | UNSUPPORTED_OP
    deriving (Eq, Show)

class VFS a where
    data File a :: *
    data Directory a :: *
    files :: Directory a -> VFSM a [File a]
    dirs :: Directory a -> VFSM a [Directory a]
    mkdir :: Directory a -> String -> VFSM a (Directory a)
    file :: FilePath -> VFSM a (File a)
    dir :: FilePath -> VFSM a (Directory a)

type VFSM f a = ExceptT VFSExcept (ReaderT f IO) a

io :: (VFS f) => IO a -> VFSM f a
io = lift . lift

config :: (VFS f) => VFSM f f
config = lift ask

runVFS :: (VFS f) => f -> VFSM f a -> IO (Either VFSExcept a)
runVFS fs d = runReaderT (runExceptT d) fs

class (VFS (Inner a), VFS a) => WrappingVFS a where
    type Inner a :: *
    innerFS :: a -> Inner a

runInnerFS :: (WrappingVFS f) => f -> VFSM (Inner f) a -> VFSM f a
runInnerFS ofs op = do
    result <- io $ runVFS ifs op
    case result of
        Left e -> throwE e
        Right r -> return r
    where
        ifs = innerFS ofs
