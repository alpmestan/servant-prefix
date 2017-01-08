{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeOperators #-}
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

-- Example
type API = "foo" :> Get '[JSON] Int

api :: Proxy API
api = Proxy

server :: Server API
server = return 10

main :: IO ()
main = prefixing "testing" api $ \apiProxy -> run 8080 (serve apiProxy server)
