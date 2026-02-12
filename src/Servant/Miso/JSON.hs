-----------------------------------------------------------------------------
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RecordWildCards       #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Servant.Miso.JSON
-- Copyright   :  2016-2025 (C) dmjio
-- License     :  BSD3-style (see the file LICENSE)
-- Maintainer  :  David M. Johnson <code@dmj.io>
-- Stability   :  experimental
-- Portability :  non-portable
----------------------------------------------------------------------------
module Servant.Miso.JSON
  ( -- *** Types
    JSON
  ) where
----------------------------------------------------------------------------
import qualified Data.List.NonEmpty as NE
import           Servant.API (Accept (..), MimeRender (..))
import qualified Network.HTTP.Media as M
----------------------------------------------------------------------------
import           Miso.JSON (ToJSON (..), encode)
import           Miso.String (fromMisoString)
----------------------------------------------------------------------------
-- | HTML MimeType used for servant APIs
--
-- > type Home = "home" :> Get '[HTML] (Component model action)
--
data JSON
----------------------------------------------------------------------------
-- | @application/json;charset=utf-8@
instance Accept JSON where
    contentTypes _ =
      "application" M.// "json" M./: ("charset", "utf-8") NE.:|
      [ "application" M.// "json" ]
----------------------------------------------------------------------------
-- | Render JSON from a servant API
instance ToJSON a => MimeRender JSON a where
  mimeRender _ = fromMisoString . encode
----------------------------------------------------------------------------
