alexScanTokens :: String -> [token]

parse :: [token] -> Class

typecheck :: Class -> Class

codegen :: Class -> ClassFile
-- bildet abstrakte Syntax in abstrakten Bytecode ab

compiler :: String -> ClassFile
compiler = codegen . typecheck . parse . alexScanTokens

bytecodegen :: ClassFile -> IO()
-- erzeugt Bytecode--Datei
main = do
s <- readFile "..."
--s <- getContents
bytecodegen (compiler s)