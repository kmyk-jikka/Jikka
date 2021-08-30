module Main where

import Asterius.Types
import qualified Data.Text as T
import Jikka.Common.Format.Error
import qualified Jikka.Main.Subcommand.Convert as Convert
import Jikka.Main.Target

convert' :: String -> String
convert' prog = case Convert.run PythonTarget CPlusPlusTarget "<input>" (T.pack prog) of
  Left err -> unlines $ prettyError' err
  Right prog -> T.unpack prog

convert :: JSString -> JSString
convert = toJSString . convert' . fromJSString

foreign export javascript "convert" convert :: JSString -> JSString

main :: IO ()
main = return ()
