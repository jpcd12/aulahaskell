{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

-- Back-end: Servant
module Main where

import Data.Proxy
import Network.Wai.Handler.Warp
import Servant

-- Mude a linha de baixo para cada página
type API = "hello" :> Get '[JSON] String

-- Mude as duas linhas de baixo para cada página
helloFunc :: Server API
helloFunc = pure "Hello, world!"

-- Não alterar
app :: Application
app = serve (Proxy :: Proxy API) helloFunc

-- cabal repl
-- cabal update
main :: IO ()
main = do
    print "Serving on port 8080"
    run 8080 app
