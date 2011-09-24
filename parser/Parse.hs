-- Parse Scheme code into JSON. w00t
module Parse (parseToJS) where

import Control.Monad
import Control.Monad.State
import qualified Data.Map as Map
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
  
data ParseState = PS { lid :: Int -- The last id number
                     , info :: TokenInfo
                     }
                  
type TokenInfo = Map.Map TokenSummary Int

data TokenSummary = TKS { tksVal :: String }
instance Eq TokenSummary where
  a == b = tksVal a == tksVal b
instance Ord TokenSummary where
  a `compare` b = tksVal a `compare` tksVal b

-- Parsing:
specChar :: Parser Char
specChar = fmap spec (oneOf "\"\\nt'"
                      <?> "valid escape character (\", n, t, \\, or ')")
  where spec char = case char of
          '"'       -> '"'
          'n'       -> '\n'
          't'       -> '\t'
          '\\'      -> '\\'
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

type WithID = Control.Monad.State.State ParseState

jsonify :: Val -> WithID JSVal
jsonify (Id str)  
  | isKeyword str = tkn "keyword"  str
  | otherwise     = do ps@PS {lid=newId, info=info} <- get
                       let currSummary = TKS {tksVal=str}
                       if Map.member currSummary info
                         then let (Just oldId) = Map.lookup currSummary info in
                           return $ FullSexp { value = str
                                             , tp = "variable"
                                             , idNum = oldId
                                             , body = JSList []
                                             }
                         else do put $ incrementWithToken str ps
                                 return $ FullSexp { value = str
                                                   , tp = "variable"
                                                   , idNum = newId
                                                   , body = JSList []}
jsonify (Number n) = tkn "number" $ show n
jsonify (String str) = tkn "string" str
jsonify (Bool bool) = tkn "keyword" $ if bool then "true" else "false"
jsonify (Comment str) = tkn "comment" str
jsonify l@(List ls) =
  do ps <- get
     let (a, s) = runState (fmap JSList $ mapM jsonify ls) $ incrementPS ps
     put $ incrementPS s
     return $ FullSexp { value = show l
                       , tp = "list"
                       , idNum = lid s
                       , body = a}
jsonify (Sequence ls) =
  do ps <- get
     let (a, s) = runState (fmap JSList (mapM jsonify ls)) $ incrementPS ps
     put $ incrementPS s
     return a

incrementPS :: ParseState -> ParseState
incrementPS PS {lid=idn, info=info} = PS {lid=idn + 1, info=info}

incrementWithToken :: String -> ParseState -> ParseState
incrementWithToken tok PS {lid=idn, info=info} =
  PS { lid=idn + 1
     , info=Map.insert TKS{tksVal=tok} idn info}

-- jsonify a token of the given type with the given value.
tkn :: String -> String -> WithID JSVal
tkn tp val = do ps <- get
                put $ incrementPS ps
                return $ FullSexp { value = val
                                  , tp = tp
                                  , idNum = lid ps
                                  , body = JSList []}

summarize :: String -> TokenSummary
summarize str = TKS {tksVal=str}

-- I should be using ByteString, but meh (who cares about performance?).
parseToJS :: String -> String
parseToJS code = case parse expressions "TPL" code of
  Left err -> show err
  Right val -> (evalState $ fmap show $ jsonify val) $ PS {lid=0, info=Map.empty::TokenInfo}
  