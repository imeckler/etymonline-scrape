module Main where
import Data.Char

import Control.Concurrent.ParallelIO
import qualified Data.ByteString.Lazy.Char8 as B
import Network.HTTP.Conduit
import Text.HTML.TagSoup
import Control.Applicative

dataDir :: String
dataDir = "data/"

downloadLetterPage :: Char -> String -> IO ()
downloadLetterPage c n = do
    pgTxt <- simpleHttp $ "http://www.etymonline.com/index.php?l=" ++ [c] ++ "&p=" ++ n
    B.writeFile (dataDir ++ [c] ++ n) pgTxt

letterDownloads :: Char -> IO [IO ()]
letterDownloads c = do
    fstPage <- simpleHttp $ "http://www.etymonline.com/index.php?l=" ++ [c]
    B.writeFile (dataDir ++ [c] ++ "0") fstPage
    let ts = parseTags $ B.unpack fstPage
    let pgNums = filter (all isNumber) . map fromTagText . filter isTagText . (!! 1) $ partitions  (~== ("<ul>")) ts
    return $ map (downloadLetterPage c) pgNums

main = do
    downloadActions <- concat <$> mapM letterDownloads ['a'..'z']
    parallel_ $ downloadActions
    stopGlobalPool
