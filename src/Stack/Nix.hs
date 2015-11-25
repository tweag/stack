{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveDataTypeable #-}

-- | Run commands in a nix-shell
module Stack.Nix
  (execWithOptionalShell
  ,reexecWithOptionalShell
  ,nixCmdName
  ,StackNixException(..)
  ) where

import           Control.Applicative
import           Control.Exception.Lifted
import           Control.Monad
import           Control.Monad.Catch (throwM,MonadCatch,MonadMask)
import           Control.Monad.IO.Class (MonadIO,liftIO)
import           Control.Monad.Logger (MonadLogger,logDebug)
import           Control.Monad.Reader (MonadReader,asks)
import           Control.Monad.Trans.Control (MonadBaseControl)
import           Data.Char (toUpper)
import           Data.List (intercalate)
import           Data.Maybe
import           Data.Streaming.Process (ProcessExitedUnsuccessfully(..))
import qualified Data.Text as T
import           Data.Typeable
import           Data.Version (showVersion)
import           Network.HTTP.Client.Conduit (HasHttpManager)
import qualified Paths_stack as Meta
import           Prelude -- Fix redundant import warnings
import           Stack.Constants (stackProgName)
import           Stack.Docker (StackDockerException(OnlyOnHostException), reExecArgName)
import           Stack.Types
import           Stack.Types.Internal
import           System.Environment (lookupEnv,getArgs,getExecutablePath)
import           System.Exit (exitSuccess, exitWith)
import           System.IO (stderr,stdin,hIsTerminalDevice)
import           System.Process.Read
import           System.Process.Run
import           System.Process (CreateProcess(delegate_ctlc))


-- | If Nix is enabled, re-runs the currently running OS command in a Nix container.
-- Otherwise, runs the inner action.
--
-- This takes an optional release action which should be taken IFF control is
-- transfering away from the current process to the intra-container one.  The main use
-- for this is releasing a lock.  After launching reexecution, the host process becomes
-- nothing but an manager for the call into docker and thus may not hold the lock.
reexecWithOptionalShell
    :: M env m
    => IO ()
    -> m ()
reexecWithOptionalShell =
    execWithOptionalShell getCmdArgs
  where
    getCmdArgs = do
        args <-
            fmap
                (("--" ++ reExecArgName ++ "=" ++ showVersion Meta.version) :)
                (liftIO getArgs)
        exePath <- liftIO getExecutablePath
        return (exePath, args)

-- | If Nix is enabled, re-runs the OS command returned by the second argument in a
-- Nix container.  Otherwise, runs the inner action.
--
-- This takes an optional release action just like `reexecWithOptionalShell`.
execWithOptionalShell
    :: M env m
    => m (FilePath,[String])
    -> IO ()
    -> m ()
execWithOptionalShell getCmdArgs inner =
  do config <- asks getConfig
     inShell <- getInShell
     isReExec <- asks getReExec
     if | inShell && not isReExec ->
            throwM OnlyOnHostException
        | inShell ->
            liftIO (do inner
                       exitSuccess)
        | not (nixEnable (configNix config)) ->
            do liftIO inner
               liftIO exitSuccess
        | otherwise ->
            do runShellAndExit
                 getCmdArgs

runShellAndExit :: M env m
                => m (String, [String])
                -> m ()
runShellAndExit getCmdArgs = do
     config <- asks getConfig
     envOverride <- getEnvOverride (configPlatform config)
     (cmnd,args) <- getCmdArgs
     isStdoutTerminal <- asks getTerminal
     (isStdinTerminal,isStderrTerminal) <-
       liftIO ((,) <$> hIsTerminalDevice stdin
                   <*> hIsTerminalDevice stderr)
     let mshellFile = nixInitFile (configNix config)
         pkgsInConfig = nixPackages (configNix config)
     if not (null pkgsInConfig) && isJust mshellFile then
       throwM NixCannotUseShellFileAndPackagesException
       else return ()
     let isTerm = isStdinTerminal && isStdoutTerminal && isStderrTerminal
         nixopts = case mshellFile of
           Just filePath -> [filePath]
           Nothing -> ["-E", intercalate " " $ concat
                              [["with (import <nixpkgs> {});"
                               ,"runCommand \"myEnv\" {"
                               ,"buildInputs=lib.optional stdenv.isLinux glibcLocales ++ lib.optionals stdenv.isDarwin [darwin.security_tool] ++ ["],pkgsInConfig,["];"
                               ,"shellHook=''"
                               ,   "STACK_IN_NIX_EXTRA_ARGS='"]
                               ,      (map (\p -> concat ["--extra-lib-dirs=", "${"++p++"}/lib"
                                                         ," --extra-include-dirs=", "${"++p++"}/include"])
                                           pkgsInConfig), ["' ;"
                               ,"'';"
                               ,"} \"\""]]]
                    -- glibcLocales is necessary on Linux to avoid warnings about GHC being incapable to set the locale.
         fullArgs = concat [["--pure"],
                            map T.unpack (nixShellOptions (configNix config))
                           ,nixopts
                           ,["--command", ("export " ++ inShellEnvVar ++ "=1 ; ")
                                          ++ intercalate " " (cmnd:args)
                                          ++ " $STACK_IN_NIX_EXTRA_ARGS"]
                           ]
     $logDebug $ T.pack $
         "Using a nix-shell environment " ++ (case mshellFile of
            Just filePath -> "from file: " ++ filePath
            Nothing -> "with nix packages: " ++ (intercalate ", " pkgsInConfig))
     e <- try (callProcess'
                 (if isTerm then id else \cp -> cp { delegate_ctlc = False })
                 Nothing
                 envOverride
                 "nix-shell"
                 fullArgs)
     case e of
       Left (ProcessExitedUnsuccessfully _ ec) -> liftIO (exitWith ec)

       Right () -> liftIO exitSuccess

-- | 'True' if we are currently running inside a Nix.
getInShell :: (MonadIO m) => m Bool
getInShell = liftIO (isJust <$> lookupEnv inShellEnvVar)

-- | Environment variable used to indicate stack is running in container.
inShellEnvVar :: String
inShellEnvVar = concat [map toUpper stackProgName,"_IN_NIXSHELL"]

-- | Command-line argument for "nix"
nixCmdName :: String
nixCmdName = "nix"

type M env m =
  (MonadIO m
  ,MonadReader env m
  ,MonadLogger m
  ,MonadBaseControl IO m
  ,MonadCatch m
  ,HasConfig env
  ,HasTerminal env
  ,HasReExec env
  ,HasHttpManager env
  ,MonadMask m
  )

-- Exceptions thown specifically by Stack.Nix
data StackNixException
  = NixCannotUseShellFileAndPackagesException
    -- ^ Nix can't be given packages and a shell file at the same time
    deriving (Typeable)

instance Exception StackNixException

instance Show StackNixException where
  show NixCannotUseShellFileAndPackagesException =
    "You cannot have packages and a shell-file filled at the same time in your nix-shell configuration."
