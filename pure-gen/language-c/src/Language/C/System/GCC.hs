{-# LANGUAGE PatternGuards #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Language.C.System.Gcc
-- Copyright   :  (c) 2008 Benedikt Huber
-- License     :  BSD-style
-- Maintainer  :  benedikt.huber@gmail.com
-- Stability   :  experimental
-- Portability :  portable
--
-- Invoking gcc for preprocessing and compiling.
-----------------------------------------------------------------------------
module Language.C.System.GCC (
    GCC,newGCC,
)
where
import Language.C.Data.RList as RList
import Language.C.System.Preprocess
import Data.Maybe
import System.Process
import System.Directory
import Data.List

-- | @GCC@ represents a reference to the gcc compiler
newtype GCC = GCC { gccPath :: FilePath }

-- | create a reference to @gcc@
newGCC :: FilePath -> GCC
newGCC = GCC

instance Preprocessor GCC where
    parseCPPArgs _ = gccParseCPPArgs
    runCPP gcc cpp_args =
        do  -- copy the input to the outputfile, because in case the input is preprocessed,
            -- gcc -E will do nothing.
            maybe (return()) (copyWritable (inputFile cpp_args)) (outputFile cpp_args)
            rawSystem (gccPath gcc) (buildCppArgs cpp_args)
                where copyWritable source target = do copyFile source target
                                                      p <- getPermissions target
                                                      setPermissions target p{writable=True}

-- | Parse arguments for preprocessing via GCC.
--   At least one .c, .hc or .h file has to be present.
--   For now we only support the most important gcc options.
--
--   1) Parse all flags relevant to CppArgs
--   2) Move -c,-S,-M? to other_args
--   3) Strip -E
--   4) The rest goes into extra_args
gccParseCPPArgs :: [String] -> Either String (CppArgs, [String])
gccParseCPPArgs args =
    case mungeArgs ((Nothing,Nothing,RList.empty),(RList.empty,RList.empty)) args of
        Left err                   -> Left err
        Right ((Nothing,_,_),_)  -> Left "No .c / .hc / .h source file given"
        Right ((Just input_file,output_file_opt,cpp_opts),(extra_args,other_args))
            -> Right ((rawCppArgs (RList.reverse extra_args) input_file)
                      { outputFile = output_file_opt, cppOptions = RList.reverse cpp_opts },
                      RList.reverse other_args)
    where
    mungeArgs :: ParseArgsState -> [String] -> Either String ParseArgsState
    mungeArgs parsed@( cpp_args@(inp,out,cpp_opts),
                          unparsed@(extra,other))
              unparsed_args =
        case unparsed_args of
            ("-E":rest) -> mungeArgs parsed rest

            (flag:rest) |  flag == "-c"
                        || flag == "-S"
                        || "-M" `isPrefixOf` flag
                        -> mungeArgs (cpp_args,(extra,other `snoc` flag)) rest

            ("-o":file:rest)   | isJust out -> Left "two output files given"
                               | otherwise          -> mungeArgs ((inp,Just file,cpp_opts),unparsed) rest

            (cpp_opt:rest)     | Just (opt,rest') <- getArgOpt cpp_opt rest
                               -> mungeArgs ((inp,out,cpp_opts `snoc` opt),unparsed) rest'

            (cfile:rest)       | any (flip isSuffixOf cfile) (words ".c .hc .h")
                               -> if isJust inp
                                   then Left "two input files given"
                                   else mungeArgs ((Just cfile,out,cpp_opts),unparsed) rest

            (unknown:rest)     -> mungeArgs (cpp_args,(extra `snoc` unknown,other)) rest

            []                 -> Right parsed

    getArgOpt cpp_opt rest | "-I" `isPrefixOf` cpp_opt = Just (IncludeDir (drop 2 cpp_opt),rest)
                           | "-U" `isPrefixOf` cpp_opt = Just (Undefine (drop 2 cpp_opt),rest)
                           | "-D" `isPrefixOf` cpp_opt = Just (getDefine (drop 2 cpp_opt),rest)
    getArgOpt "-include" (f:rest')                     = Just (IncludeFile f, rest')
    getArgOpt _ _ = Nothing
    getDefine opt = let (key,val) = break (== '=') opt in Define key (if null val then "" else tail val)

type ParseArgsState = ((Maybe FilePath, Maybe FilePath, RList CppOption), (RList String, RList String))


buildCppArgs :: CppArgs -> [String]
buildCppArgs (CppArgs options extra_args _tmpdir input_file output_file_opt) = do
       (concatMap tOption options)
    ++ outputFileOpt
    ++ ["-E", input_file]
    ++ extra_args
    where
    tOption (IncludeDir incl)  = ["-I",incl]
    tOption (Define key value) = [ "-D" ++ key ++ (if null value then "" else "=" ++ value) ]
    tOption (Undefine key)     = [ "-U" ++ key ]
    tOption (IncludeFile f)    = [ "-include", f]
    outputFileOpt = concat [ ["-o",output_file] | output_file <- maybeToList output_file_opt ]

