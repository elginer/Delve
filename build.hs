-- I declare this build script to be in the public domain!
-- John Morrice 2009

import System.Directory
import System.Cmd
import System.Exit

-- Some OS' which will remain NAMELESS insist on pointless file extensions for their executable formats.
-- Which is not at all like people who use with formats like .dui and .dcode - Not at all!
#if windows_HOST_OS==1
edc_name = "edc.exe"
edvm_name = "edvm.exe"
edc_run   = ""
#else
edc_name = "edc"
edc_run  = "./"
edvm_name = "edvm"
#endif

mk_build_dir = do
   createDirectoryIfMissing False "build"
   es <- fmap ( filter $ \ e -> not $ e == "." || e == ".." ) $ getDirectoryContents "src"
   mapM_ ( \ e -> copyFile ( "src/" ++ e ) ( "build/" ++ e ) ) es
   

main = do
  mk_build_dir
  setCurrentDirectory "build"
  ghc <- system "ghc --make -O2 -fforce-recomp edc"
  if_good ghc $ do
        edc <- system $ edc_run ++ edc_name ++ " extend ../delve/Object.edelv ../delve/Symbol.edelv ../delve/Bool.delv ../delve/Int.edelv ../delve/CallCC.edelv -g -fforce-recomp"
        if_good edc $ do
           setCurrentDirectory ".."
           d <- getCurrentDirectory
           catch ( do copyFile ( "build/" ++ edc_name ) $ d ++ "/" ++ edc_name
                      copyFile ( "build/" ++ edvm_name ) $ d ++ "/" ++ edvm_name
                      copyFile "build/edvm.dui" $ d ++ "/edvm.dui" )
                 ( \ e -> error $ "Error: Could not copy files into root directory:\n" ++ show e )

if_good e f =
   case e of
     ExitSuccess -> f
     e -> exitWith e
