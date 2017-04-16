{-# LANGUAGE TypeFamilies, FlexibleContexts #-}
module System.IO.VFS.ReadOnlyFS
(
    ReadOnlyFS, readOnly
)
where

import System.IO.VFS
import Control.Monad.Trans.Except

data ReadOnlyFS a = ReadOnlyFS a
data ReadOnlyFile a = ReadOnlyFile a
data ReadOnlyDirectory a = ReadOnlyDirectory a

readOnly :: (VFS a) => a -> ReadOnlyFS a
readOnly = ReadOnlyFS

instance (VFS a) => WrappingVFS (ReadOnlyFS a) where
    type Inner (ReadOnlyFS a) = a
    innerFS (ReadOnlyFS ifs) = ifs

instance (VFS a) => VFS (ReadOnlyFS a) where
    type File (ReadOnlyFS a) = ReadOnlyFile (File a)
    type Directory (ReadOnlyFS a) = ReadOnlyDirectory (Directory a)
    lsFiles (ReadOnlyDirectory child) = do
        ofs <- config
        result <- runInnerFS ofs $ lsFiles child
        return $ ReadOnlyFile <$> result
    lsDirs (ReadOnlyDirectory child) = do
        ofs <- config
        result <- runInnerFS ofs $ lsDirs child
        return $ ReadOnlyDirectory <$> result
    mkDir d s = throwE UNSUPPORTED_OP
    rmDir d = throwE UNSUPPORTED_OP
    rmFile f = throwE UNSUPPORTED_OP
    file fp = do
        ofs <- config
        result <- runInnerFS ofs $ file fp
        return $ ReadOnlyFile result
    dir fp = do
        ofs <- config
        result <- runInnerFS ofs $ dir fp
        return $ ReadOnlyDirectory result
