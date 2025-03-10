module Scanner where

import Data.Char (isAlpha, isAlphaNum, isDigit)
import Token

scan :: String -> Either ScanError [Token]
scan source = scanTokens source (1, 1)

scanTokens :: String -> (Int, Int) -> Either ScanError [Token]
scanTokens [] pos = Right [(Eof, pos)]
scanTokens chars@(c : cs) pos@(line, col)
  | isAlpha c || c == '_' = do
      let (lexeme, cs') = span (\x -> isAlphaNum x || x == '_') chars
      let token = (scanWord lexeme, pos)
      (token :) <$> scanTokens cs' (line, col + length lexeme)
  | isDigit c = do
      let (lexeme, cs') = scanNumber chars
      let token = ((Number' . read) lexeme, pos)
      (token :) <$> scanTokens cs' (line, col + length lexeme)
  | c == '"' = do
      let (lexeme, cs') = span (/= '"') cs
      case cs' of
        '"' : cs'' -> do
          let token = (String' lexeme, pos)
          let line' = line + (length . filter (== '\n')) lexeme
          let col' = col + length lexeme + 2
          (token :) <$> scanTokens cs'' (line', col')
        _ ->
          Left $
            ScanError
              "Unterminated string"
              (takeWhile (/= '\n') lexeme)
              pos
  | c `elem` [' ', '\t', '\r'] = scanTokens cs (line, col + 1)
  | c == '\n' = scanTokens cs (line + 1, 1)
  | c == '/' =
      case cs of
        '/' : cs' ->
          let cs'' = dropWhile (/= '\n') cs'
           in scanTokens cs'' (line + 1, 1)
        _ ->
          let token = (Slash, pos)
           in (token :) <$> scanTokens cs (line, col + 1)
  | otherwise =
      case chars of
        c' : c'' : cs'
          | Just tokenType <- scanDoubleChar c' c'' ->
              let token = (tokenType, pos)
               in (token :) <$> scanTokens cs' (line, col + 2)
        _ : _
          | Just tokenType <- scanSingleChar c ->
              let token = (tokenType, pos)
               in (token :) <$> scanTokens cs (line, col + 1)
        _ -> Left (ScanError "Unidentified token" [c] pos)

scanWord :: String -> TokenType
scanWord lexeme = case lexeme of
  "and"    -> And
  "class"  -> Class
  "else"   -> Else
  "false"  -> False'
  "fun"    -> Fun
  "for"    -> For
  "if"     -> If
  "nil"    -> Nil
  "or"     -> Or
  "print"  -> Print
  "return" -> Return
  "super"  -> Super
  "this"   -> This
  "true"   -> True'
  "var"    -> Var
  "while"  -> While
  _        -> Identifier lexeme

scanNumber :: String -> (String, String)
scanNumber chars = do
  let (intPart, cs) = span isDigit chars
  case cs of
    ('.' : cs') ->
      let (decPart, cs'') = span isDigit cs'
       in (intPart ++ "." ++ decPart, cs'')
    _ -> (intPart, cs)

scanDoubleChar :: Char -> Char -> Maybe TokenType
scanDoubleChar c0 c1 = case [c0, c1] of
  "!=" -> Just BangEqual
  "==" -> Just EqualEqual
  "<=" -> Just LessEqual
  ">=" -> Just GreaterEqual
  _    -> Nothing

scanSingleChar :: Char -> Maybe TokenType
scanSingleChar c = case c of
  '(' -> Just LeftParen
  ')' -> Just RightParen
  '{' -> Just LeftBrace
  '}' -> Just RightBrace
  ',' -> Just Comma
  '.' -> Just Dot
  '-' -> Just Minus
  '+' -> Just Plus
  ';' -> Just Semicolon
  '*' -> Just Star
  '!' -> Just Bang
  '=' -> Just Equal
  '>' -> Just Greater
  '<' -> Just Less
  _   -> Nothing

-- Error

data ScanError = ScanError
  { message  :: String
  , lexeme   :: String
  , position :: (Int, Int)
  }

instance Show ScanError where
  show (ScanError message lexeme position) =
    "\n\ESC[31m"
      ++ "Scan Error - "
      ++ "\ESC[0m"
      ++ message
      ++ "\nLexeme - "
      ++ lexeme
      ++ "\nPosition - "
      ++ show position
