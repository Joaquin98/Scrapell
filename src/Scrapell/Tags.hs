module Scrapell.Tags (
  cleanScripts ,
  cleanCSS ,
  cleanComments ,
  clean ,
  attrsToString ,
  tagsToString ,
  cmpStrTagName
)
where
import Scrapell.Types
import Text.HTML.TagSoup
{-
  Elimina los scripts.
-}
cleanScripts :: [Tag String] -> Bool -> [Tag String]
cleanScripts [] b = []
cleanScripts ((TagOpen "script" _):tags)  b = cleanScripts tags True
cleanScripts ((TagClose "script"):tags)  b = cleanScripts tags False
cleanScripts (tag:tags) True = cleanScripts tags True
cleanScripts (tag:tags) b = tag : (cleanScripts tags b)

{-
  Elimina las etiquetas relacionadas con el CSS.
-}
cleanCSS :: [Tag String] -> Bool -> [Tag String]
cleanCSS [] b = []
cleanCSS ((TagOpen "style" _):tags)  b = cleanCSS tags True
cleanCSS ((TagClose "style"):tags)  b = cleanCSS tags False
cleanCSS (tag:tags) True = cleanCSS tags True
cleanCSS (tag:tags) b = tag : (cleanCSS tags b)


{-
  Elimina las etiquetas que tienen comentarios.
-}
cleanComments :: [Tag String] -> [Tag String]
cleanComments [] = []
cleanComments (tag:tags)
             | isTagComment tag = cleanComments tags
             | otherwise = tag : (cleanComments tags)

{-
  Limpia lo que no sirva luego del parseo.
-}
clean :: [Tag String] -> [Tag String]
clean tags = cleanScripts (cleanCSS (cleanComments tags) False) False

{-
  Dada una lista de atributos, devuelve como se verían en el HTML de
  una página.
-}
attrsToString :: [Attr] -> String
attrsToString [] = []
attrsToString ((name,val):attrs) = " " ++ name ++ "=\"" ++ val ++ "\"" ++ attrsToString attrs

{-
  Dada una lista de tags, devuelve como se verían en el HTML de
  una página.
-}
tagsToString :: [Tag String] -> String
tagsToString ((TagOpen name attrs):tags) = "<" ++ name ++ (attrsToString attrs) ++ ">"
                ++ (tagsToString tags)
tagsToString ((TagText text):tags) = text ++ (tagsToString tags)
tagsToString ((TagComment comment):tags) = "<!-- " ++ comment ++ "-->" ++ (tagsToString tags)
tagsToString ((TagClose name):tags) = "</" ++ name ++ ">" ++ (tagsToString tags)
tagsToString _ = []

{-
  Compara un string con el nombre de un Tag.
-}
cmpStrTagName :: String -> TagName -> Bool
cmpStrTagName name AnyTag = True
cmpStrTagName name (TagString name') = name == name'
