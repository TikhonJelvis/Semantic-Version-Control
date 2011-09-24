-- Parse Scheme code into JSON. w00t
module Parse (parseToJS) where

import Control.Monad
import Control.Monad.State
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

type WithID = Control.Monad.State.State Int

-- TODO: Id numbers!
jsonify :: Val -> WithID JSVal
jsonify (Id str) = tkn (if isKeyword str then "keyword" else "variable") str
jsonify (Number n) = tkn "number" $ show n
jsonify (String str) = tkn "string" str
jsonify (Bool bool) = tkn "keyword" $ if bool then "true" else "false"
jsonify (Comment str) = tkn "comment" str
jsonify l@(List ls) =
  do idn <- get
     let (a, s) = runState (fmap JSList $ mapM jsonify ls) $ idn + 1
     put $ s + 1
     return $ FullSexp { value = show l
                       , tp = "list"
                       , idNum = s
                       , body = a}
                      
jsonify (Sequence ls) =
  do idn <- get
     let (a, s) = runState (fmap JSList (mapM jsonify ls)) idn
     put $ s + 1
     return a

-- jsonify a token of the given type with the given value.
tkn :: String -> String -> WithID JSVal
tkn tp val = do idn <- get
                put $ idn + 1
                return $ FullSexp { value = val
                                  , tp = tp
                                  , idNum = idn
                                  , body = JSList []}

-- I should be using ByteString, but meh (who cares about performance?).
parseToJS :: String -> String
parseToJS code = case parse expressions "TPL" code of
  Left err -> show err
  Right val -> (evalState $ fmap show $ jsonify val) 0
  