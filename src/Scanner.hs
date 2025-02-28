module Scanner where

import Data.Char (isAlpha, isAlphaNum, isDigit)
import Token

data ScanError = ScanError
  { message :: String,
    lexeme :: String,
    location :: (Int, Int)
  }
  deriving (Show)

scan :: String -> Either ScanError [Token]
scan source = scanTokens source (1, 1)

scanTokens :: String -> (Int, Int) -> Either ScanError [Token]
scanTokens [] location = Right [(Eof, location)]
scanTokens chars@(c : cs) location@(line, col)
  -- Keywords and identifiers
  | isAlpha c || c == '_' = do
      let (lexeme, cs') = span (\v -> isAlphaNum v || v == '_') chars
      let token = (scanWord lexeme, location)
      let col' = col + length lexeme
      (token :) <$> scanTokens cs' (line, col')

  -- Numbers
  | isDigit c = do
      let (lexeme, cs') = scanNumber chars
      let token = (Number' (read lexeme), location)
      let col' = col + length lexeme
      (token :) <$> scanTokens cs' (line, col')

  -- Strings
  | c == '"' = do
      let (lexeme, cs') = span (/= '"') cs
      case cs' of
        _ : cs'' -> do
          let token = (String' lexeme, location)
          let line' = line + length (filter (== '\n') lexeme)
          let col' = col + length lexeme + 2
          (token :) <$> scanTokens cs'' (line', col')
        _ ->
          Left $
            ScanError
              "Unterminated string"
              (takeWhile (/= '\n') lexeme)
              location

  -- Whitespaces
  | c `elem` [' ', '\t', '\r'] = scanTokens cs (line, col + 1)
  -- Newline
  | c == '\n' = scanTokens cs (line + 1, 1)
  --
  -- Division and comments
  | c == '/' =
      case cs of
        '/' : cs' ->
          let cs'' = dropWhile (/= '\n') cs'
           in scanTokens cs'' (line + 1, 1)
        _ ->
          let token = (Slash, location)
           in (token :) <$> scanTokens cs (line, col + 1)
  -- Single and double char tokens
  | otherwise =
      case chars of
        c' : c'' : cs'
          | Just tokenType <- scanDoubleChar c' c'' ->
              let token = (tokenType, location)
               in (token :) <$> scanTokens cs' (line, col + 2)
        _c : _cs
          | Just tokenType <- scanSingleChar c ->
              let token = (tokenType, location)
               in (token :) <$> scanTokens cs (line, col + 1)
        _ -> Left (ScanError "Unidentified token" [c] location)

scanWord :: String -> TokenType
scanWord lexeme = case lexeme of
  "and" -> And
  "class" -> Class
  "else" -> Else
  "false" -> False'
  "fun" -> Fun
  "for" -> For
  "if" -> If
  "nil" -> Nil
  "or" -> Or
  "print" -> Print
  "return" -> Return
  "super" -> Super
  "this" -> This
  "true" -> True'
  "var" -> Var
  "while" -> While
  _ -> Identifier lexeme

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
  _ -> Nothing

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
  _ -> Nothing
