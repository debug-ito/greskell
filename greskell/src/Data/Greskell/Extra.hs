-- |
-- Module: Data.Greskell.Extra
-- Description: Extra utility functions implemented by Greskell
-- Maintainer: Toshio Ito <debug.ito@gmail.com>
--
-- 
module Data.Greskell.Extra
  ( writeAllProperties
  ) where

import Data.Aeson (ToJSON)
import Data.Greskell.Binder (Binder, newBind)
import Data.Greskell.Graph
  ( PropertyMap(..), Property(..), Element
  )
import qualified Data.Greskell.Graph as Graph
import Data.Greskell.GTraversal (Walk, SideEffect, gProperty)
import Data.Monoid (mconcat)

-- | Make a series of @.property@ steps to write all properties in the
-- given 'PropertyMap'.
writeAllProperties :: (PropertyMap m, Property p, ToJSON v, Element e)
                   => m p v -> Binder (Walk SideEffect e e)
writeAllProperties ps = fmap mconcat $ mapM toPropStep $ allProperties ps
  where
    toPropStep prop = do
      bval <- newBind $ propertyValue prop
      return $ gProperty (Graph.key $ propertyKey prop) bval
