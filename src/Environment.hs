{-# LANGUAGE ImportQualifiedPost #-}

module Environment where

import Control.Monad         ((>=>))
import Data.Functor          ((<&>))
import Data.IORef            (atomicModifyIORef', modifyIORef, newIORef, readIORef)
import Data.List             qualified as List
import Data.List.NonEmpty    (NonEmpty ((:|)), fromList, tail, toList, (<|))
import Data.Map              qualified as Map
import Data.Time.Clock.POSIX (getPOSIXTime)
import Prelude               hiding (tail)

import Error                 (ResolveError (ResolveError), RuntimeError (RuntimeError))
import Syntax                (Callable, Env (Env), Literal (NativeFn, Number'), String')

-- data Env = Env (IORef (Map String Literal)) (Maybe Env)

global :: IO Env
global = newIORef nativeFns <&> Env `flip` Nothing

child :: Env -> IO Env
child env = newIORef Map.empty <&> Env `flip` Just env

parent :: Env -> Env
parent (Env _ (Just enc)) = enc
parent _                  = undefined

ancestor :: Env -> Int -> Env
ancestor env 0                = env
ancestor (Env _ (Just enc)) d = ancestor enc (d - 1)
ancestor _ _                  = undefined

progenitor :: Env -> Env
progenitor (Env _ (Just enc)) = progenitor enc
progenitor env                = env

initialize :: String -> Literal -> Env -> IO Env
initialize name value env@(Env ref _) = modifyIORef ref (Map.insert name value) >> pure env

getHere :: String' -> Env -> IO (Either RuntimeError Literal)
getHere (name, pos) (Env ref _) = do
  vars <- readIORef ref
  pure $ case Map.lookup name vars of
    Just val -> Right val
    Nothing  -> Left $ RuntimeError "Undefined variable" (name, pos)

getAt :: String' -> Distances -> Env -> IO (Either RuntimeError Literal)
getAt name dists env = getHere name $ case getDistance name dists of
  Right dist -> ancestor env dist
  Left _     -> progenitor env

assignAt :: String' -> Literal -> Int -> Env -> IO (Either RuntimeError ())
assignAt name value 0 (Env ref _) =
  atomicModifyIORef' ref $ \vars ->
    if Map.member (fst name) vars
      then (Map.insert (fst name) value vars, Right ())
      else (vars, Left $ RuntimeError "Undefined variable" name)
assignAt name value d (Env _ (Just enc)) = assignAt name value (d - 1) enc
assignAt name _ _ _ = pure . Left $ RuntimeError "Undefined variable" name

nativeFns :: Map.Map String Literal
nativeFns = Map.fromList [("clock", clock)]

clock :: Literal
clock = NativeFn "clock" native 0
 where
  native :: Callable
  native _ env = do
    time <- realToFrac <$> getPOSIXTime
    pure $ pure (Number' time, env)

type Scope = Map.Map String Bool

type Distances = Map.Map String' Int

start :: NonEmpty Scope
start = fromList [Map.empty]

begin :: NonEmpty Scope -> NonEmpty Scope
begin = (Map.empty <|)

end :: NonEmpty Scope -> NonEmpty Scope
end = fromList . tail

declare :: String' -> NonEmpty Scope -> Either ResolveError (NonEmpty Scope)
declare (name, pos) (scope :| scopes) =
  if Map.member name scope
    then Left $ ResolveError "Variable already declared" (name, pos)
    else Right $ Map.insert name False scope :| scopes

define :: String -> NonEmpty Scope -> NonEmpty Scope
define name (scope :| scopes) = Map.insert name True scope :| scopes

declareDefine :: String' -> NonEmpty Scope -> Either ResolveError (NonEmpty Scope)
declareDefine name = declare name >=> pure . (define . fst) name

calcDistance :: String -> NonEmpty Scope -> Maybe Int
calcDistance name = List.findIndex (Map.member name) . toList

getDistance :: String' -> Distances -> Either RuntimeError Int
getDistance name dists = case Map.lookup name dists of
  Just dist -> Right dist
  Nothing   -> Left $ RuntimeError "Unresolved variable" name
