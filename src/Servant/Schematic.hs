{-# OPTIONS_GHC -fprint-explicit-kinds #-}

{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeInType #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Servant.Schematic where

import           Control.Monad.IO.Class
import           Data.Aeson
import           Data.Kind
import           Data.Maybe
import           Data.Monoid
import           Data.Proxy
import           Data.Schematic
import           Data.Schematic.JsonSchema
import           Data.Singletons
import           Data.Singletons.Prelude hiding ((:>))
import           Data.Text
import           Data.Type.Equality
import           GHC.TypeLits
import qualified JSONSchema.Draft4.Schema as D4
import           Servant hiding (Tagged)
import           Servant.API.TypeLevel


data Tagged (t :: k) a = Tagged a deriving (Functor, Show)

instance ToJSON a => ToJSON (Tagged t a) where
  toJSON (Tagged a) = toJSON a

type family ConcatAlternative (l :: [k]) :: k where
  ConcatAlternative (h ': tl) = ConcatAlternative' 'Nothing (h ': tl)

type family ConcatAlternative' (mapi :: Maybe s) (l :: [k]) :: k where
  ConcatAlternative' ('Just api) '[] = api
  ConcatAlternative' 'Nothing (h ': tl) = ConcatAlternative' ('Just h) tl
  ConcatAlternative' ('Just api) (h ': tl) = ConcatAlternative' ('Just (api :<|> h)) tl

type family Expand (req :: Maybe q) (api :: [k]) :: [k] where
  Expand 'Nothing api = api
  Expand ('Just req) api = api :++ '[req]

type family ConcatMaybe (path :: Maybe p) (pathSegment :: q) :: Maybe p where
  ConcatMaybe 'Nothing q = q
  ConcatMaybe ('Just a) q = 'Just (a :> q)

type family PrefixMap (prefix :: k) (paths :: [p]) :: [p] where
  PrefixMap p '[] = '[]
  PrefixMap p (h ': tl) = p :> h ': PrefixMap p tl

type family PrefixFirst (prefix :: k) (paths :: [p]) :: [p] where
  PrefixFirst p (h ': tl) = p :> h ': tl

type family Schematized' (path :: Maybe p) (req :: Maybe q) (api :: k) :: [k] where
  Schematized' mpath mreq (Verb method code ctypes (JsonRepr schema)) =
    Expand mreq
      '[ Verb method code ctypes (JsonRepr schema)
      , "response" :> Get '[JSON] (Tagged schema D4.Schema) ]
  Schematized' mpath mreq (Verb method code ctypes a) =
    Expand mreq '[Verb method code ctypes a]
  Schematized' mpath mreq (ReqBody cs (JsonRepr schema) :> sub) =
    PrefixFirst (ReqBody cs (JsonRepr schema))
      (Schematized'
        mpath
        ('Just ("request" :> Get '[JSON] (Tagged schema D4.Schema)))
        sub)
  Schematized' mpath mreq (ReqBody cs a :> sub) =
    PrefixFirst (ReqBody cs a) (ConcatAlternative (Schematized' mpath mreq sub))
  Schematized' mpath mreq (a :<|> b) = Schematized' mpath mreq a :++ Schematized' mpath mreq b
  Schematized' mpath mreq (a :> sub) = (PrefixMap a (Schematized' (ConcatMaybe mpath a) mreq sub))

type family Schematized (api :: k) :: k where
  Schematized api = ConcatAlternative (Schematized' 'Nothing 'Nothing api)

-- Typechecking tests

type TestApiSchematic1 =
  "api" :> ReqBody '[JSON] (JsonRepr 'SchemaNull) :> Post '[JSON] (JsonRepr 'SchemaNull)

_test1 :: (Schematized TestApiSchematic1) :~~:
  ("api" :> ReqBody '[JSON] (JsonRepr 'SchemaNull) :> Post '[JSON] (JsonRepr 'SchemaNull)
  :<|> "api" :> "response" :> Get '[JSON] (Tagged 'SchemaNull D4.Schema)
  :<|> "api" :> "request"  :> Get '[JSON] (Tagged 'SchemaNull D4.Schema))
_test1 = HRefl

-- Schematic TestApiSchematic
-- ConcatAlternative (PrefixMap "api" (Schematized' ('Just "api") 'Nothing ("api" :> ReqBody '[JSON] (JsonRepr 'SchemaNull) :> Post '[JSON] (JsonRepr 'SchemaNull))))
-- ConcatAlternative' 'Nothing (PrefixMap "api" (Schematized' ('Just "api") 'Nothing ("api" :> ReqBody '[JSON] (JsonRepr 'SchemaNull) :> Post '[JSON] (JsonRepr 'SchemaNull))))
-- ConcatAlternative' 'Nothing (PrefixMap "api" (Schematized' ('Just "api") 'Nothing (ReqBody '[JSON] (JsonRepr 'SchemaNull) :> Post '[JSON] (JsonRepr 'SchemaNull))))
-- ConcatAlternative' 'Nothing (PrefixMap "api" (PrefixFirst (ReqBody '[JSON] (JsonRepr 'SchemaNull)) (Schematized' ('Just "api") ('Just ("request" :> Get '[JSON] (Tagged 'SchemaNull D4.Schema))) (Post '[JSON] (JsonRepr 'SchemaNull)))))
-- ConcatAlternative' 'Nothing (PrefixMap "api" (PrefixFirst (ReqBody '[JSON] (JsonRepr 'SchemaNull)) (Expand ('Just ("request" :> Get '[JSON] (Tagged 'SchemaNull D4.Schema))) '[Post '[JSON] (JsonRepr 'SchemaNull), "response" :> Get '[JSON] (Tagged 'SchemaNull D4.Schema)])))
-- ConcatAlternative' 'Nothing (PrefixMap "api" (PrefixFirst (ReqBody '[JSON] (JsonRepr 'SchemaNull)) '[Post '[JSON] (JsonRepr 'SchemaNull), "response" :> Get '[JSON] (Tagged 'SchemaNull D4.Schema)] :++ ["request" :> Get '[JSON] (Tagged 'SchemaNull D4.Schema]))
-- ConcatAlternative' 'Nothing (PrefixMap "api" (PrefixFirst (ReqBody '[JSON] (JsonRepr 'SchemaNull)) '[Post '[JSON] (JsonRepr 'SchemaNull), "response" :> Get '[JSON] (Tagged 'SchemaNull D4.Schema), "request" :> Get '[JSON] (Tagged 'SchemaNull D4.Schema]))
-- ConcatAlternative' 'Nothing (PrefixMap "api" ('[ReqBody '[JSON] (JsonRepr 'SchemaNull) :> Post '[JSON] (JsonRepr 'SchemaNull), "response" :> Get '[JSON] (Tagged 'SchemaNull D4.Schema), "request" :> Get '[JSON] (Tagged 'SchemaNull D4.Schema]))
-- ConcatAlternative' 'Nothing ("api" :> ReqBody '[JSON] (JsonRepr 'SchemaNull) :> Post '[JSON] (JsonRepr 'SchemaNull) ': PrefixMap "api" '["response" :> Get '[JSON] (Tagged 'SchemaNull D4.Schema), "request" :> Get '[JSON] (Tagged 'SchemaNull D4.Schema]))
-- ConcatAlternative' 'Nothing ("api" :> ReqBody '[JSON] (JsonRepr 'SchemaNull) :> Post '[JSON] (JsonRepr 'SchemaNull) ': "api" :> "response" :> Get '[JSON] (Tagged 'SchemaNull D4.Schema) ': PrefixMap "api" '["request" :> Get '[JSON] (Tagged 'SchemaNull D4.Schema)])
-- ConcatAlternative' 'Nothing ("api" :> ReqBody '[JSON] (JsonRepr 'SchemaNull) :> Post '[JSON] (JsonRepr 'SchemaNull) ': "api" :> "response" :> Get '[JSON] (Tagged 'SchemaNull D4.Schema) ': "api" :> "request" :> Get '[JSON] (Tagged 'SchemaNull D4.Schema) ': PrefixMap "api" '[])
-- ConcatAlternative' 'Nothing ("api" :> ReqBody '[JSON] (JsonRepr 'SchemaNull) :> Post '[JSON] (JsonRepr 'SchemaNull) ': "api" :> "response" :> Get '[JSON] (Tagged 'SchemaNull D4.Schema) ': "api" :> "request" :> Get '[JSON] (Tagged 'SchemaNull D4.Schema) ': '[])
-- ConcatAlternative' 'Nothing '["api" :> ReqBody '[JSON] (JsonRepr 'SchemaNull) :> Post '[JSON] (JsonRepr 'SchemaNull), "api" :> "response" :> Get '[JSON] (Tagged 'SchemaNull D4.Schema), "api" :> "request" :> Get '[JSON] (Tagged 'SchemaNull D4.Schema)])
-- ConcatAlternative' ('Just ("api" :> ReqBody '[JSON] (JsonRepr 'SchemaNull) :> Post '[JSON] (JsonRepr 'SchemaNull))) '["api" :> "response" :> Get '[JSON] (Tagged 'SchemaNull D4.Schema), "api" :> "request" :> Get '[JSON] (Tagged 'SchemaNull D4.Schema)])
-- ConcatAlternative' ('Just ("api" :> ReqBody '[JSON] (JsonRepr 'SchemaNull) :> Post '[JSON] (JsonRepr 'SchemaNull) :<|> "api" :> "response" :> Get '[JSON] (Tagged 'SchemaNull D4.Schema))) '["api" :> "request" :> Get '[JSON] (Tagged 'SchemaNull D4.Schema)])
-- ConcatAlternative' ('Just ("api" :> ReqBody '[JSON] (JsonRepr 'SchemaNull) :> Post '[JSON] (JsonRepr 'SchemaNull) :<|> "api" :> "response" :> Get '[JSON] (Tagged 'SchemaNull D4.Schema) :<|> "api" :> "request" :> Get '[JSON] (Tagged 'SchemaNull D4.Schema))) '[])
-- "api" :> ReqBody '[JSON] (JsonRepr 'SchemaNull) :> Post '[JSON] (JsonRepr 'SchemaNull) :<|> "api" :> "response" :> Get '[JSON] (Tagged 'SchemaNull D4.Schema) :<|> "api" :> "request" :> Get '[JSON] (Tagged 'SchemaNull D4.Schema)

type TestApiSchematic2 =
  "api" :> Post '[JSON] (JsonRepr 'SchemaNull)

_test2 :: Schematized TestApiSchematic2 :~:
  "api" :> Post '[JSON] (JsonRepr 'SchemaNull)
  :<|> "api" :> "response" :> Get '[JSON] (Tagged 'SchemaNull D4.Schema)
_test2 = Refl

type TestApiSchematic3 = "api" :> Post '[JSON] ()

_test3 :: Schematized TestApiSchematic3 :~: TestApiSchematic3
_test3 = Refl

type TestApiSchematic4 =
  "api" :> ReqBody '[JSON] (JsonRepr 'SchemaNull) :> Post '[JSON] ()

_test4 :: Schematized TestApiSchematic4 :~:
  "api" :> ReqBody '[JSON] (JsonRepr 'SchemaNull) :> Post '[JSON] () :<|>
  "api" :> "request" :> Get '[JSON] (Tagged 'SchemaNull D4.Schema)
_test4 = Refl

-- | An implementation for json-schema introspection handlers generated by
-- @Schematized@ type family. It works with requests and response schemas.
schemaHandler
  :: forall m tag (schema :: Schema) a
   . (SingI schema, Monad m)
  => m (Tagged schema D4.Schema)
schemaHandler  = pure . Tagged . fromJust $ toJsonSchema (Proxy @schema)
