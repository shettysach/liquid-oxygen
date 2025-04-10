{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE TupleSections       #-}

module Resolver where

import Control.Monad ((>=>))
import Data.Foldable (foldrM)
import Data.Map      qualified as Map

import Data.Maybe    (isJust)
import Environment
import Error         (ResolveError (ResolveError))
import Syntax

-- https://hackage.haskell.org/package/extra-1.8/docs/Data-Tuple-Extra.html

fst3 :: (a, b, c) -> a
fst3 (a, _, _) = a

snd3 :: (a, b, c) -> b
snd3 (_, b, _) = b

thd3 :: (a, b, c) -> c
thd3 (_, _, c) = c

first3 :: (a -> a') -> (a, b, c) -> (a', b, c)
first3 f ~(a, b, c) = (f a, b, c)

second3 :: (b -> b') -> (a, b, c) -> (a, b', c)
second3 f ~(a, b, c) = (a, f b, c)

third3 :: (c -> c') -> (a, b, c) -> (a, b, c')
third3 f ~(a, b, c) = (a, b, f c)

--

data FunctionType = None | Fun | Mthd | Init

type State = (FunctionType, Distances, [Scope])

resolve :: [Stmt] -> Either ResolveError ([Stmt], Distances)
resolve stmts = (stmts,) . snd3 <$> resolveStmts stmts (None, Map.empty, [Map.empty])

resolveStmts :: [Stmt] -> State -> Either ResolveError State
resolveStmts [] state = Right state
resolveStmts (stmt : stmts) state@(ftype, dists, stack) = case stmt of
  Expr expr -> resolveExpr expr state >>= resolveStmts stmts
  Var name (Just expr) -> declare name stack >>= resolveExpr expr . (ftype,dists,) >>= resolveStmts stmts . third3 (define name)
  Var name Nothing -> declareDefine name stack >>= resolveStmts stmts . (ftype,dists,)
  Return mExpr | None <- ftype -> Left $ ResolveError "Top level return" "return" $ snd mExpr
  Return mExpr | Init <- ftype, isJust $ fst mExpr -> Left $ ResolveError "Can't return value from init" "return" $ snd mExpr
  Return mExpr -> case fst mExpr of
    Just expr -> resolveExpr expr state >>= resolveStmts stmts
    Nothing   -> resolveStmts stmts state
  Block stmts' -> resolveStmts stmts' (ftype, dists, begin stack) >>= resolveStmts stmts . third3 tail
  Print expr -> resolveExpr expr state >>= resolveStmts stmts
  If cond thenStmt elseStmt ->
    resolveExpr cond state >>= resolveStmts [thenStmt] >>= case elseStmt of
      Just stmt' -> resolveStmts [stmt'] >=> resolveStmts stmts
      Nothing    -> resolveStmts stmts
  While cond stmt' -> resolveExpr cond state >>= resolveStmts [stmt'] >>= resolveStmts stmts
  Function{} -> resolveFunction stmt Fun state >>= resolveStmts stmts . first3 (const ftype) . third3 tail
  Class name methods ->
    declareDefine name (define ("this", snd name) stack)
      >>= (foldrM resolveMethod `flip` methods) . (ftype,dists,) . begin
      >>= resolveStmts stmts . first3 (const ftype) . third3 tail

resolveFunction :: Stmt -> FunctionType -> State -> Either ResolveError State
resolveFunction (Function name params stmts') ntype (_, dists, stack) =
  foldrM declareDefine (begin $ define name stack) params
    >>= resolveStmts stmts' . (ntype,dists,)
resolveFunction _ _ _ = undefined

resolveMethod :: Stmt -> State -> Either ResolveError State
resolveMethod mthd@(Function ("init", _) _ _) = resolveFunction mthd Init
resolveMethod mthd                            = resolveFunction mthd Mthd

resolveExpr :: Expr -> State -> Either ResolveError State
resolveExpr (Literal _) state             = Right state
resolveExpr (Grouping expr) state         = resolveExpr expr state
resolveExpr (Variable name) state         = resolveLocal name state
resolveExpr (Assignment name expr) state  = resolveExpr expr state >>= resolveLocal name
resolveExpr (Unary _ right) state         = resolveExpr right state
resolveExpr (Binary _ left right) state   = resolveExpr left state >>= resolveExpr right
resolveExpr (Logical _ left right) state  = resolveExpr left state >>= resolveExpr right
resolveExpr (Call (expr, _) args) state   = resolveExpr expr state >>= foldrM resolveExpr `flip` args
resolveExpr (Get expr _) state            = resolveExpr expr state
resolveExpr (Set expr _ expr') state      = resolveExpr expr state >>= resolveExpr expr'
resolveExpr (This pos) state@(Mthd, _, _) = resolveLocal ("this", pos) state
resolveExpr (This pos) state@(Init, _, _) = resolveLocal ("this", pos) state
resolveExpr (This pos) _                  = Left $ ResolveError "Used `this` out of class" "this" pos

resolveLocal :: String' -> State -> Either ResolveError State
resolveLocal name state
  | scope : _ <- thd3 state
  , Map.lookup (fst name) scope == Just False =
      Left $ ResolveError "Can't read local variable in own init" `uncurry` name
resolveLocal name state =
  case calcDistance 0 name (thd3 state) of
    Just dist -> Right $ second3 (Map.insert name dist) state
    Nothing   -> Right state
