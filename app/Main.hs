import System.IO

import YAMLTokenizer
import YAMLValidateSyntax

import qualified Data.Token as Tk

import Error.Error (Err(..))

main :: IO ()
main = do
    let filePath = "sample/yamlSample1.txt"
    handle <- openFile filePath ReadMode
    tokens <- tokenize handle
    print tokens
    --validateYAMLSyntax tokens
    --let seq = [Tk.Scalar "test" 0, Tk.Comma, Tk.Space 1, Tk.Scalar "wrong one" 0, Tk.Colon]
    --let errors = validateSequence seq
    --putStrLn "Errors:"
    --mapM_ print errors
    --putStrLn $ "Total errors: " ++ show (length errors)
    --hClose handle