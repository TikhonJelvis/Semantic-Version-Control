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
  show (String str) = init . tail . show $ show str
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
                      , body  :: JSVal}
           | JSList [JSVal]
             
instance Show JSVal where
  show FullSexp {value=value, tp=tp, idNum=idNum, body=body} = 
    " {\"value\" : \"" ++ value ++ "\" , \n\"type\" : \"" ++ tp ++ "\",\n\"id\" : " ++
    show idNum ++ ", \n \"body\" : " ++ show body ++ "} \n"
  show (JSList []) = "[] "
  show (JSList ls) = " [" ++ (foldl1 ((++) . (++ ", ")) $ map show ls) ++ "] "
  
data ParseState = PS { lid :: Int -- The last id number
                     , info :: TokenInfo
                     , currScope :: Int
                     , newScope :: Int
                     , scopeStack :: [Int]
                     , defining :: Bool}
                  
type TokenInfo = Map.Map TokenSummary Int

data TokenSummary = TKS { tksVal :: String 
                        , scope :: Int}
instance Eq TokenSummary where
  a == b = tksVal a == tksVal b && scope a == scope b
instance Ord TokenSummary where
  a `compare` b = (tksVal a ++ "  " ++ (show $ scope a)) `compare`
                  (tksVal b ++ "  " ++ (show $ scope b))

-- Returns whether the first summary with the given value and scope in the stack.
stackSummary :: String -> [Int] -> TokenInfo -> Maybe TokenSummary
stackSummary val [] info = Nothing
stackSummary val (scope:rest) info =
  if Map.member TKS{tksVal=val, scope=scope} info 
  then Just TKS{tksVal=val, scope=scope}
  else stackSummary val rest info

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
expressions = fmap Sequence $ expression `sepEndBy` spaces

isKeyword :: String -> Bool
isKeyword = (`elem` ["define", "if", "cond", "nil", "lambda", "let"])

isNewScope :: String -> Bool
isNewScope = (`elem` ["define", "lambda"])

-- Returns whether the given list starting with define defines a function
isScopeCreator :: Val -> Bool
isScopeCreator v = isFnDefine v || isLambda v || isLet v
  where isFnDefine (List ((Id "define"):(List _):_)) = True
        isFnDefine _ = False
        isLambda (List ((Id "lambda"):_)) = True
        isLambda _ = False
        isLet (List ((Id "let"):_)) = True
        isLet _ = False

type WithID = Control.Monad.State.State ParseState

jsonify :: Val -> WithID JSVal
jsonify (Id str)  
  | isKeyword str = tkn "keyword" str
  | otherwise     = do ps@PS {lid=newId
                             , info=info
                             , currScope=cs
                             , scopeStack=ss
                             , defining=def} <- get
                       let currSummary =
                             if def
                             then TKS { tksVal=str
                                      , scope=cs}
                             else case stackSummary str ss info of
                               Nothing        -> TKS { tksVal=str
                                                     , scope=cs}
                               Just something -> something
                       if Map.member currSummary info
                         then let (Just oldId) = Map.lookup currSummary info in
                           return $ FullSexp { value = str
                                             , tp = "variable"
                                             , idNum = oldId
                                             , body = JSList []}
                         else do put $ incrementWithToken str cs ps
                                 return $ FullSexp { value = str
                                                   , tp = "variable"
                                                   , idNum = newId
                                                   , body = JSList []}
jsonify (Number n)    = tkn "number" $ show n
jsonify (String str)  = tkn "string" str
jsonify (Bool bool)   = tkn "keyword" $ if bool then "true" else "false"
jsonify (Comment str) = tkn "comment" str
jsonify l@(List ((Id "lambda"):((List args):rest))) = 
  do keyword <- jsonify (Id "lambda")
     ps@PS{currScope=cs,newScope=ns} <- get
     let nps = incrementScope . incrementPS $ setDefining True ps
     let (a, s) = runState (fmap JSList $ mapM jsonify args) nps
     let ((JSList a2), s2) = runState (fmap JSList $ mapM jsonify rest) s
     let kexp = FullSexp { value = "lambda"
                         , tp = "keyword"
                         , idNum = lid nps
                         , body = JSList []}
     let argExp = FullSexp { value = ""
                           , tp = "list"
                           , idNum = lid s
                           , body = a}
     put $ decrementScope $ incrementPS s2
     return $ FullSexp { value = show l
                       , tp = "list"
                       , idNum = lid s2
                       , body = JSList $ [kexp,argExp] ++ a2}
jsonify l@(List ((Id "define"):((List args):rest))) = 
  do keyword <- jsonify (Id "lambda")
     ps@PS{currScope=cs,newScope=ns} <- get
     let nps = incrementScope . incrementPS $ setDefining True ps
     let (a, s) = runState (fmap JSList $ mapM jsonify args) nps
     let ((JSList a2), s2) = runState (fmap JSList $ mapM jsonify rest) s
     let kexp = FullSexp { value = "lambda"
                         , tp = "keyword"
                         , idNum = lid nps
                         , body = JSList []}
     let argExp = FullSexp { value = ""
                           , tp = "list"
                           , idNum = lid s
                           , body = a}
     put $ decrementScope $ incrementPS s2
     return $ FullSexp { value = show l
                       , tp = "list"
                       , idNum = lid s2
                       , body = JSList $ [kexp,argExp] ++ a2}
jsonify l@(List ls)   =
  do ps'@PS{currScope=cs,newScope=ns} <- get
     let ps = setDefining False ps'
     let scopeCreator = isScopeCreator l
     let (a, s) = if scopeCreator
                  then runState (fmap JSList $ mapM jsonify ls) $
                       incrementScope $ incrementPS ps
                  else runState (fmap JSList $ mapM jsonify ls) $
                       incrementPS ps
     put $ if scopeCreator then decrementScope $ incrementPS s else incrementPS s
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
incrementPS PS {lid=idn
               , info=info
               , currScope=cs
               , newScope=ns
               , scopeStack=ss
               , defining=def} =
  PS { lid=idn + 1
     , info=info
     , currScope=cs
     , newScope=ns
     , scopeStack=ss
     , defining=def}

incrementScope :: ParseState -> ParseState
incrementScope PS {lid=idn
                  , info=info
                  , currScope=cs
                  , newScope=ns
                  , scopeStack=ss
                  , defining=def} =
  PS { lid=idn
     , info=info
     , currScope=ns + 1
     , newScope=ns + 1
     , scopeStack=cs:ss
     , defining=def}

decrementScope :: ParseState -> ParseState
decrementScope PS { lid=idn
                  , info=info
                  , currScope=cs
                  , newScope=ns
                  , scopeStack=(s:ss)
                  , defining=def} =
  PS { lid=idn
     , info=info
     , currScope=s
     , newScope=ns
     , scopeStack=ss
     , defining=def}

incrementWithToken :: String -> Int -> ParseState -> ParseState
incrementWithToken tok sc PS { lid=idn
                             , info=info
                             , currScope=cs
                             , newScope=ns
                             , scopeStack=ss
                             , defining=def} =
  PS { lid=idn + 1
     , info=Map.insert TKS{tksVal=tok, scope=sc} idn info
     , currScope = cs
     , newScope = ns
     , scopeStack = ss
     , defining=def}
  
setDefining :: Bool -> ParseState -> ParseState
setDefining def PS { lid=idn
                , info=info
                , currScope = cs
                , newScope = ns
                , scopeStack = ss
                , defining=_} =
  PS { lid=idn
     , info=info
     , currScope = cs
     , newScope = ns
     , scopeStack = ss
     , defining=def}

-- jsonify a token of the given type with the given value.
tkn :: String -> String -> WithID JSVal
tkn tp val = do ps <- get
                put $ incrementPS ps
                return $ FullSexp { value = val
                                  , tp = tp
                                  , idNum = lid ps
                                  , body = JSList []}

-- I should be using ByteString, but meh (who cares about performance?).
parseToJS :: String -> String
parseToJS code = case parse expressions "TPL" code of
  Left err -> show err
  Right val -> (evalState $ fmap show $ jsonify val) $
               PS { lid=0
                  , info=Map.empty::TokenInfo
                  , currScope=0
                  , newScope=1
                  , scopeStack=[]
                  , defining=False}
  