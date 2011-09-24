module Send (commit) where

import Network.HTTP
import Network.TCP
import Network.URI

server = "128.32.131.41"
port = 8080
uriStr = "http://128.32.131.41:8080/commit"

uri :: URI
uri = case parseURI uriStr of
  Just result -> result

errorMsg = "Cow: Commit failed :( Here's why:\n"

printError :: String -> IO ()
printError msg = putStrLn $ errorMsg ++ "     " ++ msg

commit :: String -> IO ()
commit body = do putStrLn "Cow: Committing..."
                 con <- openStream server 8080
                 res <- sendHTTP con req
                 case res of
                   Left err  -> printError $ show err
                   Right rsp -> case rspCode rsp of
                     (2,_,_)   -> putStrLn "Cow: Committed successfully!"
                     otherwise -> let (a, b, c) = rspCode rsp in
                                 let n = a * 100 + b * 10 + c in
                                 printError $ show n  ++ ": " ++
                                  rspReason rsp
  where ln  = length body
        req = Request { rqURI     = uri
                      , rqMethod  = POST
                      , rqHeaders = [mkHeader HdrContentType
                                     "application/json",
                                     mkHeader HdrContentLength $ show ln]
                      , rqBody    = body}