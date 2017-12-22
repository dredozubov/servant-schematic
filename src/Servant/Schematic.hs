{-# OPTIONS_GHC -fprint-explicit-kinds #-}

{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Servant.Schematic where

import Control.Monad.IO.Class
import Data.Maybe
import Data.Proxy
import Data.Schematic hiding (Schema)
import Data.Schematic.JsonSchema
import Data.Text
import Data.Monoid
import Data.Singletons
import GHC.TypeLits
import JSONSchema.Draft4.Schema as D4
import Servant
import Servant.API.TypeLevel


-- data Query = Query
--   -- { queryParams :: [(Text, Schema)] -- ^ query parameters
--   { request  :: Maybe Schema        -- ^ request
--   , response :: Maybe Schema        -- ^ response
--   }

class HasSchematizedServer api context where
  type SchematizedApi api
  hoistSchematizedServerWithContext
    :: forall m n
     . Applicative m
    => Proxy api
    -> Proxy context
    -> (forall x. m x -> n x)
    -> ServerT api m
    -> ServerT (SchematizedApi api) n

-- instance (HasSchema (SchematizedApi a), HasSchema (SchematizedApi b))
--   => HasSchema (a :<|> b) where
--     type SchematizedApi (a :<|> b)  = SchematizedApi a :<|> SchematizedApi b
--     toApi _ = toApi (Proxy @_) :<|> _

-- | NOOP
-- instance (KnownSymbol sym, HasSchema (SchematizedApi sub)) => HasSchema (sym :> sub) where
--   type SchematizedApi (sym :> sub) = SchematizedApi sub
--   toApi _ = toApi (Proxy @(SchematizedApi sub))

-- | NOOP
-- instance (HasSchema sub) => HasSchema (Vault :> sub) where
--   type SchematizedApi (Vault :> sub) = SchematizedApi sub
--   toApi _ = toApi (Proxy @sub)

-- | NOOP
-- instance (HasSchema sub) => HasSchema (IsSecure :> sub) where
--   type SchematizedApi (IsSecure :> sub) = SchematizedApi sub
--   toApi _ = toApi (Proxy @sub)

-- | NOOP
-- instance (HasSchema sub) => HasSchema (RemoteHost :> sub) where
--   type SchematizedApi (RemoteHost :> sub) = SchematizedApi sub
--   toApi _ = toApi (Proxy @sub)

-- | NOOP
-- instance (HasSchema sub) => HasSchema (HttpVersion :> sub) where
--   type SchematizedApi (HttpVersion :> sub) = SchematizedApi sub
--   toApi _ = toApi (Proxy @sub)

-- -- | NOOP
-- instance (HasSchema sub) => HasSchema (WithNamedContext x c sub) where
--   type SchematizedApi (WithNamedContext x c sub) = SchematizedApi sub
--   toApi _ = toApi (Proxy @sub)

-- -- | NOOP
-- instance (HasSchema sub) => HasSchema (QueryFlag sym :> sub) where
--   type SchematizedApi (QueryFlag sym :> sub) = SchematizedApi sub
--   toApi _ = toApi (Proxy @sub)

-- -- | NOOP
-- instance (HasSchema sub) => HasSchema (Header sym a :> sub) where
--   type SchematizedApi (Header sym a :> sub) = SchematizedApi sub
--   toApi _ = toApi (Proxy @sub)

-- #if MIN_VERSION_servant(0,12,0)
-- -- | NOOP
-- instance (HasSchema sub) => HasSchema (Description sym :> sub) where
--   type SchematizedApi (Description sym :> sub) = SchematizedApi sub
--   toApi _ = toApi (Proxy @sub)

-- -- | NOOP
-- instance (HasSchema sub) => HasSchema (Summary sym :> sub) where
--   type SchematizedApi (Summary sym :> sub) = SchematizedApi sub
--   toApi _ = toApi (Proxy @sub)
-- #endif

-- instance (KnownSymbol sym, HasSchema sub)
--   => HasSchema (Capture sym a :> sub) where
--     type SchematizedApi (Capture sym a :> sub) = SchematizedApi sub
--     toApi _ = toApi (Proxy @sub)

-- instance (KnownSymbol sym, HasSchema sub)
--   => HasSchema (CaptureAll sym a :> sub) where
--     type SchematizedApi (CaptureAll sym a :> sub) = SchematizedApi sub
--     toApi _ = toApi (Proxy @sub)

-- instance (KnownSymbol sym, HasSchema sub)
--   => HasSchema (QueryParam sym a :> sub) where
--     type SchematizedApi (QueryParam sym a :> sub) = SchematizedApi sub
--     toApi _ = toApi (Proxy @sub)

-- instance (KnownSymbol sym, HasSchema sub)
--   => HasSchema (QueryParams sym a :> sub) where
--     type SchematizedApi (QueryParams sym a :> sub) = SchematizedApi sub
--     toApi _ = toApi (Proxy @sub)

-- instance (Elem JSON cs, HasSchema sub, SingI schema)
--   => HasSchema (ReqBody cs (JsonRepr schema) :> sub) where
--     type SchematizedApi (ReqBody cs (JsonRepr schema) :> sub) =
--       Handler Schema :<|> SchematizedApi sub
--     toApi _ = (Handler $ toJsonSchema (Proxy @schema)) :<|> toApi (Proxy @sub)

instance (Elem JSON cs, HasServer (Verb method status ctypes a) context, SingI schema)
  => HasSchematizedServer (Verb method code ctypes (JsonRepr schema)) context where
    type SchematizedApi (Verb method code ctypes (JsonRepr schema)) =
      "response" :> Get '[JSON] D4.Schema
      :<|> Verb method code ctypes (JsonRepr schema)
    -- I don't see any reason for `toJsonSchema` to fail,
    -- I would consider it completely safe.
    hoistSchematizedServerWithContext
      :: forall m n
       . Applicative m
      => Proxy (Verb method code ctypes (JsonRepr schema))
      -> Proxy context
      -> (forall x. m x -> n x)
      -> ServerT (Verb method code ctypes (JsonRepr schema)) m
      -> ServerT (SchematizedApi (Verb method code ctypes (JsonRepr schema))) n
    hoistSchematizedServerWithContext pa pc nt s =
      (nt (pure . fromJust $ toJsonSchema (Proxy @schema) :: m Schema))
      :<|> nt s
