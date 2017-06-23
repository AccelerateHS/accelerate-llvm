{-# LANGUAGE CPP #-}
-- |
-- Module      : Data.Array.Accelerate.LLVM.Plugin
-- Copyright   : [2017] Trevor L. McDonell
-- License     : BSD3
--
-- Maintainer  : Trevor L. McDonell <tmcdonell@cse.unsw.edu.au>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--

module Data.Array.Accelerate.LLVM.Plugin (

  plugin

) where

import GhcPlugins
import Linker
import SysTools

import Data.IORef
import Data.Array.Accelerate.LLVM.Plugin.Annotation


plugin :: Plugin
plugin = defaultPlugin
  { installCoreToDos = install
  }

install :: [CommandLineOption] -> [CoreToDo] -> CoreM [CoreToDo]
install args rest = do
#if __GLASGOW_HASKELL__ < 802
  reinitializeGlobals
#endif
  let interactive = "interactive" `elem` args
      --
      this (CoreDoPluginPass "acc-llvm" _) = True
      this _                               = False
  --
  return $ CoreDoPluginPass "acc-llvm" (pass interactive) : filter (not . this) rest

pass :: Bool -> ModGuts -> CoreM ModGuts
pass interactive guts = do
  paths   <- concat <$> mapM (objectPaths guts) (mg_binds guts)
  objects <- return $ map (FileOption []) paths

  debugTraceMsg $ vcat $ map text $ "Data.Array.Accelerate.LLVM.Plugin linking with:" : paths

  -- We need to pass the extra object files along differently depending on
  -- whether we are in interactive (ghci) or normal mode.
  if interactive
    then do
      hsc_env <- getHscEnv
      let dyn_flags = hsc_dflags hsc_env
          ld_inputs = ldInputs dyn_flags
      liftIO $ linkCmdLineLibs
             $ hsc_env { hsc_dflags = dyn_flags { ldInputs = ld_inputs ++ objects }}

    else do
      dyn_flags   <- getDynFlags
      linker_info <- liftIO $ getLinkerInfo dyn_flags
      liftIO $ writeIORef (rtldInfo dyn_flags)
             $ Just
             $ case linker_info of
                 GnuLD     opts -> GnuLD     (opts ++ objects)
                 GnuGold   opts -> GnuGold   (opts ++ objects)
                 DarwinLD  opts -> DarwinLD  (opts ++ objects)
                 SolarisLD opts -> SolarisLD (opts ++ objects)
                 AixLD     opts -> AixLD     (opts ++ objects)
                 UnknownLD      -> UnknownLD  -- no linking performed?

  return guts

objectPaths :: ModGuts -> CoreBind -> CoreM [FilePath]
objectPaths guts (NonRec b _) = objectAnns guts b
objectPaths guts (Rec bs)     = concat <$> mapM (objectAnns guts) (map fst bs)

objectAnns :: ModGuts -> CoreBndr -> CoreM [FilePath]
objectAnns guts bndr = do
  anns  <- getAnnotations deserializeWithData guts
  return [ path | Object path <- lookupWithDefaultUFM anns [] (varUnique bndr) ]

