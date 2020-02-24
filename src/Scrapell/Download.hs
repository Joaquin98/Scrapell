module Scrapell.Download (
  downloadPageAs ,
  openItem ,
  openItems
) where
import Control.Exception
import Control.Monad
import Data.List
import System.Process
import System.Directory
import System.Exit
import System.IO

{-
  Descarga una pagina web guardadonla en un archivo con el nombre
  pasado como argumento.
-}
downloadPageAs :: String -> String -> IO String
downloadPageAs url file = do hndl <-(openFile file WriteMode)
                             do hClose hndl
                                putStrLn $ "Descargando: " ++ url
                                res <- system $ "wget " ++ url ++ " -O " ++ file
                                when (res /= ExitSuccess) $ error $ "Fallo al descargar usando wget: " ++ url
                                src <- readFile file
                                length src `seq` return src

{-
  Abre un archivo o descarga una pagina web returnando su contenido.
-}
openItem :: String -> IO String
openItem url
  | not $ "http://" `isPrefixOf` url || "https://" `isPrefixOf` url =
    readFile url
openItem url = bracket
    (openTempFile "." "pagina_temporal.tmp")
    (\(file,hndl) -> removeFile file)
    $ \(file,hndl) -> do
        hClose hndl
        putStrLn $ "Descargando: " ++ url
        res <- system $ "wget -nv " ++ url ++ " -O " ++ file
        when (res /= ExitSuccess) $ error $ "Fallo al descargar usando wget: " ++ url
        src <- readFile file
        length src `seq` return src


{-
  Abre varios archivos o páginas y devuelve la concatenación de sus
  contenidos.
-}
openItems :: [String] -> IO String
openItems urls = fmap concat $ mapM openItem urls
