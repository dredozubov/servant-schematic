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
import           Data.Text
import           GHC.TypeLits
import qualified JSONSchema.Draft4.Schema as D4
import           Servant hiding (Tagged)
import           Servant.API.TypeLevel


data Tagged (t :: k) a = Tagged a deriving (Functor, Show)

instance ToJSON a => ToJSON (Tagged t a) where
  toJSON (Tagged a) = toJSON a

type family Expand (req :: Maybe q) (api :: k) :: l where
  Expand 'Nothing api = api
  Expand ('Just req) api = req :<|> api

type family Schematize' (req :: Maybe q) (api :: k) :: l where
  Schematize' req (Verb method code ctypes (JsonRepr schema)) =
    Expand req
      ("response" :> Get '[JSON] (Tagged schema D4.Schema) :<|>
      Verb method code ctypes (JsonRepr schema))
  Schematize' req (ReqBody cs (JsonRepr schema) :> sub) =
    Schematize'
      ('Just ("request" :> Get '[JSON] (Tagged schema D4.Schema))) sub
  Schematize' req (a :<|> b) = Schematize' req a :<|> Schematize' req b
  Schematize' req (a :> sub) = a :> Schematize' req sub

type family Schematize (api :: k) :: l where
  Schematize api = Schematize' 'Nothing api

schemaHandler
  :: forall m tag (schema :: Schema) a
   . (SingI schema, Monad m)
  => m (Tagged schema D4.Schema)
schemaHandler = pure . Tagged . fromJust $ toJsonSchema (Proxy @schema)
