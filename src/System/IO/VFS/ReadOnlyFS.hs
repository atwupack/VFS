{-# LANGUAGE TypeFamilies, FlexibleContexts #-}
module System.IO.VFS.ReadOnlyFS
(
    ReadOnlyFS, readOnly
)
where

import System.IO.VFS
import Control.Monad.Trans.Except

data ReadOnlyFS a = ReadOnlyFS a

readOnly :: (VFS a) => a -> ReadOnlyFS a
readOnly = ReadOnlyFS

instance (VFS a) => WrappingVFS (ReadOnlyFS a) where
    type Inner (ReadOnlyFS a) = a
    innerFS (ReadOnlyFS ifs) = ifs

instance (VFS a) => VFS (ReadOnlyFS a) where
    data File (ReadOnlyFS a) = ReadOnlyFile (File a)
    data Directory (ReadOnlyFS a) = ReadOnlyDirectory (Directory a)
    files (ReadOnlyDirectory child) = do
        ofs <- config
        result <- runInnerFS ofs $ files child
        return $ ReadOnlyFile <$> result
    dirs (ReadOnlyDirectory child) = do
        ofs <- config
        result <- runInnerFS ofs $ dirs child
        return $ ReadOnlyDirectory <$> result
    mkdir d s = throwE UNSUPPORTED_OP
    file fp = do
        ofs <- config
        result <- runInnerFS ofs $ file fp
        return $ ReadOnlyFile result
    dir fp = do
        ofs <- config
        result <- runInnerFS ofs $ dir fp
        return $ ReadOnlyDirectory result
