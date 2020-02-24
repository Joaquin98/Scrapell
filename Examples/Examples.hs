module Examples (
  catComment ,
  wiki ,
  garbarino ,
  imbd
)
where
import Scrapell
import Data.List
import Data.Maybe


exampleHtml = "<html>\
\    <body>\
\        <div class='comments'>\
\            <div class='comment container'>\
\                <span class='comment author'>No tiene la palabra, no aparece</span>\
\                <div class='comment text'>Nada </div>\
\            </div>\
\            <div class='comment container'>\
\                <span class='comment author'>gato si tiene que aparecer</span>\
\                <img class='comment image' src='http://example.com/cat.gif' />\
\            </div>\
\            <div class='comment container'>\
\                <span class='comment author'>gato gato gato</span>\
\                <div class='comment text'>Nada por aquí</div>\
\            </div>\
\            <div class='comment container'>\
\                <span class='comment author'>Gato pero con mayus</span>\
\                <div class='comment text'>Esto no tiene que aparecer gato</div>\
\            </div>\
\        </div>\
\    </body>\
\</html>"

{-
  Ejemplo 1:
   Dado una string la cual tiene html, se obtienen todos los comentarios
   de autor que contienen la palabra gato.
-}

catComment :: IO (Maybe [String])
catComment = do let trees = strToTree exampleHtml in
                    return $ scrapAllForest trees $ textPred (\t -> isInfixOf "gato" t)
                                                    ("span" & ["" === "comment author"] --> selectText)

{-
  Ejemplo 2:
    Se extra de páginas de garbarino, el nombre, el precio y
    las imagenes disponibles de cada producto.
-}

data Product = Product { pName::String, pPrice::Double, pImages::[String] } deriving Show

links = ["https://www.garbarino.com/listado/ofertas-televisores-4274/cbvl?page=2","https://www.garbarino.com/listado/ofertas-televisores-4274/cbvl"]
links_offline = ["Pages/garbarino1.html","Pages/garbarino2.html"]
links2 = ["https://www.garbarino.com/productos/lavarropas/4298"]

garbarino :: IO [Product]
garbarino = do content <- openItems links
               let Just tree = scrapAllForest (strToTree content) $ node $ "div" & ["class" === "row itemList"] --> "div" & [hasAttr "data-product-id"]
               return $ applyToEveryTree prod tree

prod :: Tree -> (Maybe Product)
prod tree = do price <- scrapFst tree $ text ("span"&["id" `startsWith` "price","class" `startsWith` "value-item "])
               name <- scrapFst tree $ text ("h3"&["id" `startsWith` "item-description","itemprop" === "name"])
               images <- cleanMaybe $ [scrapAll tree $ attr "src" ("img"&["itemprop"==="image"])] ++
                                      [scrapAll tree $ attr "data-src" ("img"&["itemprop"==="image"])]
               return $ Product name (priceToDouble price) (removeDups $ addHttps images)

{-
  Ejemplo 3:
    Dada una página en la cual se encuentra un top 10 de peliculas,
    se extrae para cada pelicula de la pagina de la película los
    actores y sus roles.
-}

data Movie = Movie { mName::String, mActors::[(String,String)]} deriving Show

top10 = "https://www.imdb.com/list/ls003992425"
top10_offline = "Pages/imbd.html"

imbd :: IO [Movie]
imbd = do content <- openItem top10
          let Just trees = scrapAllForest (strToTree content) $ node $ "div" & ["class"==="lister-list"] --> "h3" & ["class"==="lister-item-header"]
              Just links = scrapAllForest trees $ attr "href" "a"
          content2 <- openItems (addDomain top10 links)
          let Just trees2 = scrapAllForest (strToTree content2) $ node "html"
          return $ applyToEveryTree movie trees2

movie :: Tree -> (Maybe Movie)
movie t = do name <- scrapFst t $ text ("div"&["class" === "title_wrapper"] --> "h1")
             nameA <- scrapAll t $ text ("div" & ["id" === "titleCast"] --> "td"&[notP (hasAttr "class")] --> "a" & [hasAttr "href"])
             role <- scrapAll t $ text ("div" & ["id" === "titleCast"] --> "td"&["class" === "character"] --> "a" & [hasAttr "href"])
             return $ Movie name (zip nameA role)


{-
  Ejemplo 4:
    Dada una página de wikipedia que muestra los sitios más populares mundialmente
    en una tabla, buscamos las paginas que son de origen chino, extraemos su nombre
    el dominio y el tipo de página.
-}

data Site = Site {sName :: String, sDomain :: String, sType :: String} deriving Show

wiki_websites = "https://en.wikipedia.org/wiki/List_of_most_popular_websites"
wiki_websites_offline = "Pages/wiki.html"

wiki :: IO [Site]
wiki = do content <- openItem wiki_websites
          let Just trees = scrapAllForest (strToTree content) $ node $ "table" & ["class" `startsWith` "wikitable sortable"] -->
                                                                       "tbody" --> "tr"
          return $ applyToEveryTree site trees

site :: Tree -> (Maybe Site)
site t = do coutry <- scrapFstAfter t [6,2] $ textPred (\c -> isInfixOf "China" c) "a"
            name <- scrapFstAfter t [1] $ text "td"
            domain <- scrapFstAfter t [2] $ text "td"
            typeS <- scrapFstAfter t [5] $ text "td"
            return $ Site name domain typeS
