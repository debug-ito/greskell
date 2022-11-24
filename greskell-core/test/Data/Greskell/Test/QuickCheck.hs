{-# OPTIONS_GHC -fno-warn-orphans #-}
-- | QuickCheck orphan instances and other utility.
module Data.Greskell.Test.QuickCheck
    (
    ) where

import           Data.Text       (Text, pack)
import           Test.QuickCheck (Arbitrary (..))

instance Arbitrary Text where
  arbitrary = fmap pack arbitrary


