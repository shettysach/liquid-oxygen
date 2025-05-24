{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE TupleSections       #-}
{-# LANGUAGE ViewPatterns        #-}
{-# HLINT ignore "Redundant <&>" #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module Resolver where

import Control.Arrow      ((&&&))
import Control.Monad      ((>=>))
import Data.Foldable      (foldrM)
import Data.Functor       ((<&>))
import Data.List.NonEmpty (NonEmpty ((:|)))
import Data.Map           qualified as Map
import Data.Maybe         (isJust)

import Environment
import Error              (ResolveError (ResolveError))
import Syntax

data FunctionType = NonF | Fun | Mthd | Init

data ClassType = NonC | Sup | Sub

type State = (FunctionType, ClassType, Distances, NonEmpty Scope)

resolve :: [Stmt] -> Either ResolveError ([Stmt], Distances)
resolve stmts = (stmts,) . thd4 <$> resolveStmts stmts (NonF, NonC, Map.empty, start)

replResolve :: [Stmt] -> NonEmpty Scope -> Either ResolveError ([Stmt], Distances, NonEmpty Scope)
replResolve stmts stack = uncurry (stmts,,) . (thd4 &&& fth4) <$> resolveStmts stmts (NonF, NonC, Map.empty, stack)

resolveStmts :: [Stmt] -> State -> Either ResolveError State
resolveStmts [] state = Right state
resolveStmts (stmt : stmts) state@(ftype, ctype, dists, stack) = case stmt of
  Expr expr -> resolveExpr expr state >>= resolveStmts stmts
  Var name (Just expr) ->
    declare name stack
      >>= resolveExpr expr . (ftype,ctype,dists,)
      >>= resolveStmts stmts . fourth4 (define $ fst name)
  Var name Nothing -> declareDefine name stack >>= resolveStmts stmts . (ftype,ctype,dists,)
  Return mExpr | NonF <- ftype -> Left $ ResolveError "Top level return" ("return", snd mExpr)
  Return mExpr | Init <- ftype, isJust $ fst mExpr -> Left $ ResolveError "Can't return value from init" ("return", snd mExpr)
  Return (Just expr, _) -> resolveExpr expr state >>= resolveStmts stmts
  Return (Nothing, _) -> resolveStmts stmts state
  Block stmts' -> resolveStmts stmts' (fourth4 begin state) >>= resolveStmts stmts . fourth4 end
  Print expr -> resolveExpr expr state >>= resolveStmts stmts
  If cond thenStmt elseStmt ->
    resolveExpr cond state
      >>= resolveStmts [thenStmt]
      >>= case elseStmt of
        Just stmt' -> resolveStmts [stmt'] >=> resolveStmts stmts
        Nothing    -> resolveStmts stmts
  While cond stmt' ->
    resolveExpr cond state
      >>= resolveStmts [stmt']
      >>= resolveStmts stmts
  Function{} -> resolveFunction stmt Fun state >>= resolveStmts stmts
  Class name (Just (Variable name')) _ | fst name == fst name' -> Left $ ResolveError "Can't inherit from self" name'
  Class name (Just super) methods ->
    declareDefine name stack
      >>= resolveExpr super . (ftype,Sub,dists,)
      <&> fourth4 (define "this" . begin . define "super" . begin)
      >>= foldrM resolveMethod `flip` methods
      >>= resolveStmts stmts . second4 (const ctype) . fourth4 (end . end)
  Class name Nothing methods ->
    declareDefine name stack
      <&> (ftype,Sup,dists,) . define "this" . begin
      >>= foldrM resolveMethod `flip` methods
      >>= resolveStmts stmts . fourth4 end

resolveFunction :: Stmt -> FunctionType -> State -> Either ResolveError State
resolveFunction (Function name params stmts') ntype (ftype, ctype, dists, stack) =
  declareDefine name stack
    >>= (foldrM declareDefine `flip` params) . begin
    >>= resolveStmts stmts' . (ntype,ctype,dists,)
    <&> first4 (const ftype) . fourth4 end
resolveFunction _ _ _ = undefined

resolveMethod :: Stmt -> State -> Either ResolveError State
resolveMethod mthd@(Function ("init", _) _ _) = resolveFunction mthd Init
resolveMethod mthd                            = resolveFunction mthd Mthd

resolveExpr :: Expr -> State -> Either ResolveError State
resolveExpr (Literal _) state               = Right state
resolveExpr (Grouping expr) state           = resolveExpr expr state
resolveExpr (Variable name) state           = resolveLocal name state
resolveExpr (Assignment name expr) state    = resolveExpr expr state >>= resolveLocal name
resolveExpr (Unary _ right) state           = resolveExpr right state
resolveExpr (Binary _ left right) state     = resolveExpr left state >>= resolveExpr right
resolveExpr (Logical _ left right) state    = resolveExpr left state >>= resolveExpr right
resolveExpr (Call (expr, _) args) state     = resolveExpr expr state >>= foldrM resolveExpr `flip` args
resolveExpr (Get expr _) state              = resolveExpr expr state
resolveExpr (Set expr _ expr') state        = resolveExpr expr state >>= resolveExpr expr'
resolveExpr (This pos) state@(fst4 -> Mthd) = resolveLocal ("this", pos) state
resolveExpr (This pos) state@(fst4 -> Init) = resolveLocal ("this", pos) state
resolveExpr (This pos) _                    = Left $ ResolveError "Used `this` out of class" ("this", pos)
resolveExpr (Super pos _) (snd4 -> NonC)    = Left $ ResolveError "Used `super` out of class" ("super", pos)
resolveExpr (Super pos _) (snd4 -> Sup)     = Left $ ResolveError "Used `super` in class without superclass" ("super", pos)
resolveExpr (Super pos mthd) state          = resolveLocal ("super", pos) state >>= resolveLocal mthd

resolveLocal :: String' -> State -> Either ResolveError State
resolveLocal name state
  | scope :| _ <- fth4 state
  , Just False <- Map.lookup (fst name) scope =
      Left $ ResolveError "Can't read local variable in own init" name
resolveLocal name state = pure $ case calcDistance (fst name) (fth4 state) of
  Just dist -> third4 (Map.insert name dist) state
  Nothing   -> state

-- Quadruples

fst4 :: (a, b, c, d) -> a
fst4 (a, _, _, _) = a

snd4 :: (a, b, c, d) -> b
snd4 (_, b, _, _) = b

thd4 :: (a, b, c, d) -> c
thd4 (_, _, c, _) = c

fth4 :: (a, b, c, d) -> d
fth4 (_, _, _, d) = d

first4 :: (a -> a') -> (a, b, c, d) -> (a', b, c, d)
first4 f (a, b, c, d) = (f a, b, c, d)

second4 :: (b -> b') -> (a, b, c, d) -> (a, b', c, d)
second4 f (a, b, c, d) = (a, f b, c, d)

third4 :: (c -> c') -> (a, b, c, d) -> (a, b, c', d)
third4 f (a, b, c, d) = (a, b, f c, d)

fourth4 :: (d -> d') -> (a, b, c, d) -> (a, b, c, d')
fourth4 f (a, b, c, d) = (a, b, c, f d)
