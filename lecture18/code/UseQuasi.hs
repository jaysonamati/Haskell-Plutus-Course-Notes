{-# LANGUAGE TemplateHaskellQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
module UseQuasi where



import Quasi (ternary)

fortyTwo10, fortyTwo16, fortyTwo8, fortyTwo2, fortyTwo3 :: Double
fortyTwo10 = 42
fortyTwo16 = 0x2a
fortyTwo8  = 0o52
fortyTwo2  = 0101010
fortyTwo3  = [ternary|
                1120
            |]


weird :: Int -> String
weird 37              = "hmmm -37!"
weird [ternary|1120|] = "42"
weird _               = "42"


myHtml :: Markup
myHtml = [shamlet|
    <html>
        <head>
            <title>Haskell
        <body>
            <h1>Haskell Course
            <p>Haskell is great!
|]