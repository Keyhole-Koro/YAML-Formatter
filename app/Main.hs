import System.IO

import YAMLTokenizer
import YAMLValidateSyntax

import qualified Data.Token as Tk

import Error.Error (Err(..))

main :: IO ()
main = do
    --let filePath = "sample/yamlSample1.txt"
    --handle <- openFile filePath ReadMode
    --tokens <- tokenize handle
    --print tokens
    --validateYAMLSyntax tokens
    -- let seq = [Tk.Scalar "test" 0, Tk.Comment "comment", Tk.Space 10, Tk.Comma, Tk.Space 1, Tk.Scalar "wrong one" 0, Tk.Dash, Tk.Colon]
    let map = [Tk.Scalar "key" 0, Tk.Colon, Tk.Scalar "value" 2, Tk.Space 2, Tk.Comma, Tk.Scalar "key2" 0, Tk.Colon, Tk.Scalar "value2" 2, Tk.Space 2]
    let errors = validateMapping map
    print errors
    --mapM_ print errors
    --putStrLn $ "Total errors: " ++ show (length errors)
    --hClose handle