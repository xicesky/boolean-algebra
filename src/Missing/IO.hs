
{- |
Description     : IO routines
Stability       : experimental

Various helper functions for dealing with IO.
You probably want to import this module qualified or use

>>> import Prelude hiding (writeFile, readFile)

-}
module Missing.IO
    (   writeFile
    ,   readFile
    ,   exitCodeToInt
    ,   readProcessWithExitCodeInt
    ) where

import Prelude hiding (writeFile, readFile)

import Control.Exception (IOException, handle)
import System.IO (withFile, IOMode(..))
import System.Exit (ExitCode(..))
import System.Process (readProcessWithExitCode)

import qualified Data.ByteString as B
import Data.ByteString.Builder (Builder, hPutBuilder)

import Control.Monad.IO.Class

{- FIXME: Instead of introducing another incompatibility,
    make a class of "stuff that can be read and written from files".
    And use Missing.Textual.
-}

-- | Write a ByteString 'Builder' to a file
writeFile :: MonadIO m => FilePath -> Builder -> m ()
writeFile fp content = liftIO $ withFile fp WriteMode $
    \fh -> hPutBuilder fh content

-- | Read a ByteString from a file
readFile :: MonadIO m => FilePath -> m (Either String B.ByteString)
readFile fp = liftIO $ handle errHandler (Right <$> B.readFile fp) where
    errHandler :: IOException -> IO (Either String a)
    errHandler e = return $ Left $ show e

-- | Interpret 'ExitCode' as 'Int'
exitCodeToInt :: ExitCode -> Int
exitCodeToInt = \case
    ExitSuccess -> 0
    ExitFailure x -> x

-- | 'readProcessWithExitCode' without the weird 'ExitCode'
readProcessWithExitCodeInt :: MonadIO m =>
    FilePath -> [String] -> String -> m (Int, String, String)
readProcessWithExitCodeInt file args stdin = do
    (exitcode, stdOut, stdErr) <- liftIO $ readProcessWithExitCode file args stdin
    return (exitCodeToInt exitcode, stdOut, stdErr)
