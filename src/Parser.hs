module Parser where

import Control.Arrow (first)
import Control.Monad (when)

import AST           as A
import Error         (ParseError (ParseError))
import Token         as T

type Parse a = [Token] -> Either ParseError (a, [Token])

(*>>=) :: (Monad m) => m (a, b) -> (a -> b -> m c) -> m c
x *>>= f = x >>= uncurry f

parse :: [Token] -> Either ParseError [Stmt]
parse = parse' []
 where
  parse' stmts [(T.Eof, _)] = Right (reverse stmts)
  parse' stmts tokens       = declaration tokens *>>= (parse' . (: stmts))

-- Stmts

declaration :: Parse Stmt
declaration (t : ts) = case fst t of
  T.Var       -> varDeclaration ts
  T.Print     -> expression ts *>>= (statement . A.Print)
  T.Fun       -> function ts
  T.Return    -> returnStatement ts
  T.LeftBrace -> first A.Block <$> block [] ts
  T.If        -> ifStatement ts
  T.While     -> while ts
  T.For       -> for ts
  _           -> expression (t : ts) *>>= (statement . A.Expr)
declaration tokens = expression tokens *>>= (statement . A.Expr)

statement :: Stmt -> Parse Stmt
statement stmt (t : ts) | fst t == T.Semicolon = Right (stmt, ts)
statement _ tokens = Left $ ParseError "Expected ';'" (head tokens)

block :: [Stmt] -> Parse [Stmt]
block stmts (t : ts) | fst t == T.RightBrace = Right (reverse stmts, ts)
block stmts tokens = declaration tokens *>>= (block . (: stmts))

varDeclaration :: Parse Stmt
varDeclaration ((T.Identifier name, _) : t : ts)
  | fst t == T.Equal = expression ts *>>= (statement . A.Var name . Just)
  | fst t == T.Semicolon = statement (A.Var name Nothing) (t : ts)
  | otherwise = Left $ ParseError "Expected = after var name" t
varDeclaration tokens = Left $ ParseError "Expected var name" (head tokens)

ifStatement :: Parse Stmt
ifStatement (t : ts) | fst t == T.LeftParen = do
  (condition, afterCondn) <- expression ts
  (thenBranch, afterThen) <- case afterCondn of
    t' : ts' | fst t' == T.RightParen -> declaration ts'
    _ -> Left $ ParseError "Expected ')' after if condition" (head afterCondn)
  (elseBranch, afterElse) <- case afterThen of
    t' : ts' | fst t' == T.Else -> first Just <$> declaration ts'
    _                           -> Right (Nothing, afterThen)
  Right (A.If condition thenBranch elseBranch, afterElse)
ifStatement tokens = Left $ ParseError "Expected '(' after 'if'" (head tokens)

while :: Parse Stmt
while (t : ts) | fst t == T.LeftParen = do
  (condition, afterCondn) <- expression ts
  case afterCondn of
    t' : ts' | fst t' == T.RightParen -> first (A.While condition) <$> declaration ts'
    _ -> Left $ ParseError "Expected ')' after while condition" (head ts)
while tokens = Left $ ParseError "Expected '(' after 'while'" (head tokens)

for :: Parse Stmt
for (t : ts) | fst t == T.LeftParen = do
  (initializer, afterInit) <- case ts of
    t1 : ts1
      | fst t1 == T.Semicolon -> Right (Nothing, ts1)
      | fst t1 == T.Var -> first Just <$> varDeclaration ts1
    _ -> do
      (expr, after) <- expression ts
      case after of
        t2 : ts2 | fst t2 == T.Semicolon -> Right (Just (Expr expr), ts2)
        _ -> Left $ ParseError "Expected ';' after loop initializer" (head after)

  (condition, afterCondn) <- case afterInit of
    t1 : ts1 | fst t1 == T.Semicolon -> Right (Literal (Bool' True), ts1)
    _ -> do
      (expr, after) <- expression afterInit
      case after of
        t2 : ts2 | fst t2 == T.Semicolon -> Right (expr, ts2)
        _ -> Left $ ParseError "Expected ';' after loop condition" (head after)

  (increment, afterInc) <- case afterCondn of
    t1 : ts1 | fst t1 == T.RightParen -> Right (Nothing, ts1)
    _ -> do
      (expr, after) <- expression afterCondn
      case after of
        t2 : ts2 | fst t2 == T.RightParen -> Right (Just expr, ts2)
        _ -> Left $ ParseError "Expected ')' after increment" (head after)

  (stmt, afterStmt) <- declaration afterInc

  let body = case increment of
        Nothing  -> stmt
        Just inc -> Block [stmt, Expr inc]
      stmt' = A.While condition body
      loop = case initializer of
        Nothing    -> stmt'
        Just init' -> Block [init', stmt']
  Right (loop, afterStmt)
for tokens = Left $ ParseError "Expected '(' after 'for'" (head tokens)

-- TODO: Handle kind (method/fn)
function :: Parse Stmt
function (t0 : t1 : ts)
  | T.Identifier name <- fst t0
  , T.LeftParen <- fst t1 = do
      (params, afterParams) <- parameters [] ts
      when
        (length params >= 255)
        (Left $ ParseError ">= 255 params" $ head afterParams)

      (body, afterBody) <- case afterParams of
        (t' : ts') | fst t' == T.LeftBrace -> block [] ts'
        _ -> Left $ ParseError "Expected '{' after params" (head afterParams)

      Right (A.Fun name params body, afterBody)
function tokens = Left $ ParseError "Expected identifier" (head tokens)

parameters :: [String] -> Parse [String]
parameters ps (t : ts)
  | T.RightParen <- fst t = Right (reverse ps, ts)
  | T.Comma <- fst t = parameters ps ts
  | T.Identifier p <- fst t = parameters (p : ps) ts
parameters _ tokens = Left $ ParseError "Expected ')', ',' or identifier" (head tokens)

returnStatement :: Parse Stmt
returnStatement (t : ts) | fst t == T.Semicolon = Right (A.Return Nothing, ts)
returnStatement tokens = do
  (expr, after) <- expression tokens
  case after of
    t : ts | fst t == T.Semicolon -> Right (A.Return (Just expr), ts)
    _                             -> Left $ ParseError "Expected ';'" (head after)

-- Expr

expression :: Parse Expr
expression = assignment

assignment :: Parse Expr
assignment tokens = Parser.or tokens *>>= assignment'
 where
  assignment' expr (t : ts)
    | fst t == T.Equal =
        assignment ts >>= \(value, ts') ->
          case expr of
            Variable var -> Right (A.Assignment var value, ts')
            _            -> Left (ParseError "Invalid target" t)
  assignment' expr ts = Right (expr, ts)

or :: Parse Expr
or tokens =
  Parser.and tokens *>>= or'
 where
  or' expr [] = Right (expr, [])
  or' expr (t : ts) =
    case fst t of
      T.Or -> recurse A.Or
      _    -> Right (expr, t : ts)
   where
    recurse op =
      Parser.and ts *>>= (or' . Logical (op, snd t) expr)

and :: Parse Expr
and tokens =
  equality tokens *>>= and'
 where
  and' expr [] = Right (expr, [])
  and' expr (t : ts) =
    case fst t of
      T.And -> recurse A.And
      _     -> Right (expr, t : ts)
   where
    recurse op =
      equality ts *>>= (and' . Logical (op, snd t) expr)

equality :: Parse Expr
equality tokens = comparison tokens *>>= equality'
 where
  equality' expr [] = Right (expr, [])
  equality' expr (t : ts) = case fst t of
    T.BangEqual  -> recurse A.BangEqual
    T.EqualEqual -> recurse A.EqualEqual
    _            -> Right (expr, t : ts)
   where
    recurse op =
      comparison ts *>>= (equality' . Binary (op, snd t) expr)

comparison :: Parse Expr
comparison tokens = term tokens *>>= comparison'
 where
  comparison' expr [] = Right (expr, [])
  comparison' expr (t : ts) = case fst t of
    T.Greater      -> recurse A.Greater
    T.GreaterEqual -> recurse A.GreaterEqual
    T.Less         -> recurse A.Less
    T.LessEqual    -> recurse A.LessEqual
    _              -> Right (expr, t : ts)
   where
    recurse op =
      term ts *>>= (comparison' . Binary (op, snd t) expr)

term :: Parse Expr
term tokens = factor tokens *>>= term'
 where
  term' expr [] = Right (expr, [])
  term' expr (t : ts) = case fst t of
    T.Minus -> recurse A.Minus
    T.Plus  -> recurse A.Plus
    _       -> Right (expr, t : ts)
   where
    recurse op =
      factor ts *>>= (term' . Binary (op, snd t) expr)

factor :: Parse Expr
factor tokens = unary tokens *>>= factor'
 where
  factor' expr [] = Right (expr, [])
  factor' expr (t : ts) = case fst t of
    T.Slash -> recurse A.Slash
    T.Star  -> recurse A.Star
    _       -> Right (expr, t : ts)
   where
    recurse op =
      unary ts *>>= (factor' . Binary (op, snd t) expr)

unary :: Parse Expr
unary [] = call []
unary (t : ts) =
  case fst t of
    T.Bang  -> (first . Unary) (A.Bang, snd t) <$> unary ts
    T.Minus -> (first . Unary) (A.Minus', snd t) <$> unary ts
    _       -> call (t : ts)

call :: Parse Expr
call tokens = primary tokens *>>= call'
 where
  call' expr [] = Right (expr, [])
  call' expr (t : ts) = case fst t of
    T.LeftParen -> finish expr ts *>>= call'
    _           -> Right (expr, t : ts)

  finish expr' tokens' = do
    (args, rest) <- arguments tokens'
    when
      (length args >= 255)
      (Left $ ParseError ">= 255 args" $ head rest)
    case rest of
      t' : ts' | fst t' == T.RightParen -> Right (Call expr' args, ts')
      _                                 -> Left $ ParseError "Expected ')' after args" (head rest)

  arguments (t' : ts') | fst t' == T.RightParen = Right ([], t' : ts')
  arguments tokens' = do
    (arg, rest) <- expression tokens'
    case rest of
      (t' : ts') | fst t' == T.Comma -> first (arg :) <$> arguments ts'
      _                              -> Right ([arg], rest)

primary :: Parse Expr
primary (t : ts) = case fst t of
  T.False' -> Right (Literal $ Bool' False, ts)
  T.True' -> Right (Literal $ Bool' True, ts)
  T.Nil -> Right (Literal A.Nil, ts)
  T.Number' n -> Right (Literal $ A.Number' n, ts)
  T.String' s -> Right (Literal $ A.String' s, ts)
  T.Identifier i -> Right (Variable (i, snd t), ts)
  T.LeftParen -> do
    (expr, rest) <- expression ts
    case rest of
      t' : ts' | fst t' == T.RightParen -> Right (Grouping expr, ts')
      _                                 -> Left $ ParseError "Expected ')' after expr" t
  _ -> Left $ ParseError "Expected expr" t
primary tokens = Left $ ParseError "Expected expr" (head tokens)
