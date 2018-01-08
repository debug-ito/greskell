{-# LANGUAGE QuasiQuotes #-}
import Distribution.Simple (defaultMain)
import Text.RawString.QQ (r)

main = do
  outputREADMElhs
  defaultMain

outputREADMElhs :: IO ()
outputREADMElhs = do
  body <- readFile "README.md"
  writeFile "README.lhs" (header ++ body ++ footer)

header :: String
header = [r|
```haskell

{-# LANGUAGE OverloadedStrings #-}
import Data.Text (Text)
import Test.Hspec

```
|]

footer :: String
footer = [r|
```haskell

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  specify "The Greskell type" $ do
    spec_toGremlin_Text
    spec_toGremlin_Int

```
|]

loadREADMEmd :: IO String
loadREADMEmd = readFile "README.md"
