-- Parse Scheme code into JSON. w00t
module Parse where

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
           
data JSVal = FullSexp { value :: String
                      , tp    :: String
                      , idNum :: Int
                      , body  :: JSVal
                      }
           | JSList [JSVal]
             
instance Show JSVal where
  show FullSexp {value=value, tp=tp, idNum=idNum, body=body} = 
    "{\"value\" : " ++ value ++ ",\n\"type\" : " ++ tp ++ ",\n\"id\" : " ++
    show idNum ++ ",\n\"body\" : " ++ show body ++ "}"
  

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

-- I should be using ByteString, but meh (who cares about performance?).
parseToJS :: String -> String
parseToJS code = case parse expressions "TPL" code of
  Left err -> show err
  Right val -> show val