module Network.Greskell.WebSocket.ResponseSpec
    ( main
    , spec
    ) where

import           Control.Monad                       (mapM_)
import           Test.Hspec

import           Network.Greskell.WebSocket.Response (ResponseCode (..), isClientSideError,
                                                      isServerSideError, isSuccess)

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "isSuccess" $ do
    mapM_ (uncurry $ makeResponseCodeSpec isSuccess) $ [(Success, True), (Unauthorized, False), (ServerError, False)]
  describe "isClientSideError" $ do
    mapM_ (uncurry $ makeResponseCodeSpec isClientSideError) $ [(Success, False), (Unauthorized, True), (ServerError, False)]
  describe "isServerSideError" $ do
    mapM_ (uncurry $ makeResponseCodeSpec isServerSideError) $ [(Success, False), (Unauthorized, False), (ServerError, True)]

makeResponseCodeSpec :: (ResponseCode -> Bool) -> ResponseCode -> Bool -> Spec
makeResponseCodeSpec testee input want = specify (show input) $ testee input `shouldBe` want
