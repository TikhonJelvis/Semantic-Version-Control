-- Parse Scheme code into JSON. w00t
module Parse (parseToJS) where

import Monad
import Text.ParserCombinators.Parsec

data Val = Id String
         | List [Val]
         | Number Float
         | String String
         | Bool Bool
         | Comment String
         | Sequence [Val]

instance Show Val where
  show (String str) = show str
  show (Id str) = str
  show (Number flt) = show flt
  show (Bool bool) = show bool
  show (Comment string) = ";" ++ string
  show (Sequence []) = "[]"
  show (Sequence ls) = "[" ++ (foldl1 ((++) . (++ ", ")) $ map show ls) ++ "]"
  show (List []) = "()"
  show (List ls) = "(" ++ (foldl1 ((++) . (++ " ")) $ map show ls) ++ ")"
           
data JSVal = FullSexp { value :: String
                      , tp    :: String
                      , idNum :: Int
                      , body  :: JSVal
                      }
           | JSList [JSVal]
             
instance Show JSVal where
  show FullSexp {value=value, tp=tp, idNum=idNum, body=body} = 
    "{\"value\" : \"" ++ value ++ "\",\n\"type\" : \"" ++ tp ++ "\",\n\"id\" : " ++
    show idNum ++ ",\n\"body\" : " ++ show body ++ "}"
  show (JSList []) = "[]"
  show (JSList ls) = "[" ++ (foldl1 ((++) . (++ ", ")) $ map show ls) ++ "]"
  

-- Parsing:
specChar :: Parser Char
specChar = fmap spec (oneOf "\"\\nt'"
                      <?> "valid escape character (\", n, t, \\, or ')")
  where spec char = case char of
          '"'       -> '"'
          'n'       -> '\n'
          't'       -> '\t'
          '\\'      -> '\\'
          '\''      -> '\'' 
          otherwise -> char
          
stringLiteral :: Parser Val
stringLiteral = do char '"'
                   contents <- many $ (char '\\' >> specChar)
                                     <|> noneOf "\""
                   char '"'
                   return $ String contents
                   
symbol :: Parser Char
symbol = oneOf "!#$%&|*+-/:<=>?@^_~"

atom :: Parser Val
atom = do first <- letter <|> symbol
          rest <- many (letter <|> symbol <|> digit)
          let atom = first:rest
          return $ case atom of 
            "#t" -> Bool True
            "#f" -> Bool False
            _    -> Id atom
          
number :: Parser Val
number = do sign <- char '-' <|> digit
            rest <- many $ point <|> many1 digit
            return . Number . read $ sign:join rest
  where point = do char '.'
                   num <- digit
                   return $ "." ++ show num

character :: Parser Val
character = do char '#' >> char '\\'
               val <- anyChar
               return . String $ return val
               
whiteSpace :: Parser ()
whiteSpace = skipMany1 space

list :: Parser Val
list = fmap List $ between (char '(') (char ')') $ sepBy expression whiteSpace

comment :: Parser Val
comment = fmap Comment $ between (string ";;") newline (many . noneOf $ "\n")

expression :: Parser Val
expression = atom
         <|> stringLiteral
         <|> number
         <|> character
         <|> list
         <|> comment
         
expressions :: Parser Val
expressions = fmap Sequence $ expression `sepEndBy` whiteSpace

-- TODO: Moar keywordz plz!
isKeyword :: String -> Bool
isKeyword = (`elem` ["define", "if", "cond"])

-- TODO: Id numbers!
jsonify :: Val -> JSVal
jsonify (Id str) = FullSexp { value = str
                            , tp = if isKeyword str
                                   then "keyword"
                                   else "variable"
                            , idNum = 10
                            , body = JSList []}
jsonify (Number n) = FullSexp { value = show n
                              , tp = "number"
                              , idNum = 10
                              , body = JSList []}
jsonify (String str) = FullSexp { value = str
                                , tp = "string"
                                , idNum = 10
                                , body = JSList []}
jsonify (Bool bool) = FullSexp { value = if bool then "true" else "false"
                               , tp = "keyword"
                               , idNum = 10
                               , body = JSList []}
jsonify (Comment str) = FullSexp { value = str
                                 , tp = "comment"
                                 , idNum = 10
                                 , body = JSList []}
jsonify l@(List ls) = FullSexp { value = show l
                               , tp = "list"
                               , idNum = 10
                               , body = JSList $ map jsonify ls}
                      
jsonify (Sequence ls) = JSList $ map jsonify ls

-- I should be using ByteString, but meh (who cares about performance?).
parseToJS :: String -> String
parseToJS code = case parse expressions "TPL" code of
  Left err -> show err
  Right val -> show $ jsonify val