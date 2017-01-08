{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}
import Data.Reflection
import GHC.TypeLits
import Network.Wai.Handler.Warp
import Servant

prefix :: forall s api.
          KnownSymbol s
       => Proxy s
       -> Proxy api
       -> Proxy (s :> api)
prefix Proxy Proxy = Proxy

prefixing :: String
          -> Proxy api
          -> (forall s. KnownSymbol s => Proxy (s :> api) -> r)
          -> r
prefixing s api f = reifySymbol s $ \sProxy -> f (prefix sProxy api)

serveUnder :: HasServer api '[]
           => String
           -> Proxy api
           -> Server api
           -> Application
serveUnder prefix api server = prefixing prefix api $ \apiProxy ->
  serve apiProxy server

serveUnderWithContext :: HasServer api ctx
                      => String
                      -> Proxy api
                      -> Context ctx
                      -> Server api
                      -> Application
serveUnderWithContext prefix api ctx server =
  prefixing prefix api $ \apiProxy -> serveWithContext apiProxy ctx server

-- Example
type API = "foo" :> Get '[JSON] Int

api :: Proxy API
api = Proxy

server :: Server API
server = return 10

main :: IO ()
main = run 8080 (serveUnder "testing" api server)
