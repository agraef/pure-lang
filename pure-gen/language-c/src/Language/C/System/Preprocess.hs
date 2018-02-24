-----------------------------------------------------------------------------
-- |
-- Module      :  Language.C.Wrapper.Preprocess
-- Copyright   :  (c) 2008 Benedikt Huber
-- License     :  BSD-style
-- Maintainer  :  benedikt.huber@gmail.com
-- Stability   :  experimental
-- Portability :  portable
--
-- Invoking external preprocessors.
-----------------------------------------------------------------------------
module Language.C.System.Preprocess (
    Preprocessor(..),
    CppOption(..),
    CppArgs(..),rawCppArgs,addCppOption,addExtraOption,
    runPreprocessor,
    isPreprocessed,
)
where
import Language.C.Data.InputStream
import System.Exit
import System.Directory
import System.FilePath
import System.Environment
import System.IO
import Control.Exception
import Control.Monad
import Data.List

-- | 'Preprocessor' encapsulates the abstract interface for invoking C preprocessors
class Preprocessor cpp where
    -- | parse the given command line arguments, and return a pair of parsed and ignored arguments
    parseCPPArgs :: cpp -> [String] -> Either String (CppArgs, [String])
    -- | run the preprocessor
    runCPP :: cpp -> CppArgs -> IO ExitCode

-- | file extension of a preprocessed file
preprocessedExt :: String
preprocessedExt = ".i"

-- | Generic Options for the preprocessor
data CppOption =
        IncludeDir FilePath
      | Define String String
      | Undefine String
      | IncludeFile FilePath

-- | Generic arguments for the preprocessor
data CppArgs = CppArgs {
        cppOptions :: [CppOption],
        extraOptions :: [String],
        cppTmpDir  :: Maybe FilePath,
        inputFile  :: FilePath,
        outputFile :: Maybe FilePath
    }

-- | Cpp arguments that only specify the input file name.
cppFile :: FilePath -> CppArgs
cppFile input_file = CppArgs { cppOptions = [], extraOptions = [], cppTmpDir = Nothing, inputFile = input_file, outputFile = Nothing }

-- | use the given preprocessor arguments without analyzing them
rawCppArgs :: [String] -> FilePath -> CppArgs
rawCppArgs opts input_file =
    CppArgs { inputFile = input_file, cppOptions = [], extraOptions = opts, outputFile = Nothing, cppTmpDir = Nothing }

-- | add a typed option to the given preprocessor arguments
addCppOption :: CppArgs -> CppOption -> CppArgs
addCppOption cpp_args opt =
    cpp_args { cppOptions = opt : (cppOptions cpp_args) }

-- | add a string option to the given preprocessor arguments
addExtraOption :: CppArgs -> String -> CppArgs
addExtraOption cpp_args extra =
    cpp_args { extraOptions = extra : (extraOptions cpp_args) }

-- | run the preprocessor and return an 'InputStream' if preprocesssing succeeded
runPreprocessor :: (Preprocessor cpp) => cpp -> CppArgs -> IO (Either ExitCode InputStream)
runPreprocessor cpp cpp_args = do
    bracket
        getActualOutFile
        -- remove outfile if it was temporary
        removeTmpOutFile
        -- invoke preprocessor
        invokeCpp
    where
    getActualOutFile :: IO FilePath
    getActualOutFile = maybe (mkOutputFile (cppTmpDir cpp_args) (inputFile cpp_args)) return (outputFile cpp_args)
    invokeCpp actual_out_file = do
        exit_code <- runCPP cpp (cpp_args { outputFile = Just actual_out_file})
        case exit_code of
            ExitSuccess   -> liftM Right (readInputStream actual_out_file)
            ExitFailure _ -> return $ Left exit_code
    removeTmpOutFile out_file = maybe (removeFile out_file) (\_ -> return ()) (outputFile cpp_args)

-- | create an output file, given  @Maybe tmpdir@ and @inputfile@
mkOutputFile :: Maybe FilePath -> FilePath -> IO FilePath
mkOutputFile tmp_dir_opt input_file =
    do tmpDir <- getTempDir tmp_dir_opt
       mkTmpFile tmpDir (getOutputFileName input_file)
    where
    getTempDir (Just tmpdir) = return tmpdir
    getTempDir Nothing       = getTemporaryDirectory

-- | compute output file name from input file name
getOutputFileName :: FilePath -> FilePath
getOutputFileName fp | hasExtension fp = replaceExtension filename preprocessedExt
                     | otherwise       = addExtension filename preprocessedExt
    where
    filename = takeFileName fp

-- | create a temporary file
mkTmpFile :: FilePath -> FilePath -> IO FilePath
mkTmpFile tmp_dir file_templ = do
    -- putStrLn $ "TmpDir: "++tmp_dir
    -- putStrLn $ "FileTempl: "++file_templ
    (path,file_handle) <- openTempFile tmp_dir file_templ
    hClose file_handle
    return path

-- | guess whether a file is preprocessed (file end with .i)
isPreprocessed :: FilePath -> Bool
isPreprocessed = (".i" `isSuffixOf`)

