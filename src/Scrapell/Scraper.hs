module Scrapell.Scraper (
  tagsToString ,
  strToTree ,
  scrapFst ,
  scrapAll ,
  text ,
  attr ,
  node ,
  cleanMaybe ,
  scrapFstForest ,
  scrapAllForest ,
  cleanMaybe' ,
  applyToEveryTree ,
  childreN ,
  scrapFstAfter ,
  scrapAllAfter ,
  textPred
) where
import Scrapell.Types
import Scrapell.Attributes
import Scrapell.Selection
import Scrapell.Download
import Scrapell.Tags
import Control.Applicative ((<$>),(<|>))
import Text.HTML.TagSoup
import Text.HTML.TagSoup.Tree
import Data.Maybe


{-
  Dado un selector verifica que un nodo cumpla con lo que el
  selector pide.
-}
checkSelNode :: Tree -> SelectNode -> Bool
checkSelNode (TagBranch name attrs trees) (SelectNode tagName attrPreds) =
              (cmpStrTagName name tagName) && checkPredicates attrPreds attrs
checkSelNode (TagLeaf (TagText _)) SelectText = True
checkSelNode _ _ = False

{-
  Dado una etiqueta de texto o de inicio y una seleccion de nodo, dice si
  el nodo cumple con la seleccion.
-}
checkSelTag :: Tag String -> SelectNode -> Bool
checkSelTag (TagText _) SelectText = True
checkSelTag (TagOpen name attrs) (SelectNode tagName attrPreds) =
            (cmpStrTagName name tagName) && checkPredicates attrPreds attrs
checkSelTag _ _ = False

{-
  Dado un string que puede contener el html de una o varias páginas
  devuelve una lista de arboles que se obtienen de parsear el html.
-}
strToTree :: String -> [Tree]
strToTree str = tagTree $ clean $ parseTags str


{-
  Dada una selección retorna un Scraper que al encontrar el nodo
  buscado extrae todo el texto de él y sus hijos.
-}
text :: SelectionRep s => s -> Scraper (Maybe String)
text ss = Scraper (toSelection ss) (\node -> case innerText $ flattenTree [node] of
                                    "" -> Nothing
                                    t -> Just t)
{-
  Similar al anterior solo que en este caso se pasa como parametro
  un predicado que el texto debe verificar.
-}
textPred :: SelectionRep s => (String -> Bool) -> s -> Scraper (Maybe String)
textPred p ss = Scraper (toSelection ss) (\node -> let text = innerText $ flattenTree [node] in
                                  if p text then Just text else Nothing)

{-
  Dada una selección y el nombre de un atributo retorna un Scraper que al
  encontrar el nodo buscado extrae el valor del atributo.
-}
attr :: SelectionRep s => String -> s -> Scraper (Maybe String)
attr name ss = Scraper (toSelection ss) (\(TagBranch tName attrs trees) -> findAttrValue name attrs)

findAttrValue :: String -> [Attr] -> Maybe String
findAttrValue name [] = Nothing
findAttrValue name ((n,v):attrs)
            | name == n = Just v
            | otherwise = findAttrValue name attrs

{-
  Dada una selección y el nombre de un atributo retorna un Scraper que al
  encontrar el nodo lo devuelve.
-}
node :: SelectionRep s => s -> Scraper (Maybe Tree)
node ss = Scraper (toSelection ss) (Just . id)

{-
  Toma una funcion y una lista de arboles, aplica la función a cada arbol
  hasta que uno retorne un resultado distinto a Nothing.
-}
mapLazy :: (Tree -> Maybe a) -> [Tree] -> (Maybe a)
mapLazy f [] = Nothing
mapLazy f (t:ts) = case f t of
                   Nothing -> mapLazy f ts
                   Just res -> Just res



{-
 Igual que scrapAll pero se aplica a una lista de arboles.
-}
scrapAllForest :: [Tree] -> Scraper (Maybe a) -> Maybe [a]
scrapAllForest forest scr = cleanMaybe $
               map ((\scr tree -> (scrapAll tree scr)) scr) forest

{-
  Igual que scrapFst pero se aplica a una lista de arboles.
-}
scrapFstForest :: [Tree] -> Scraper (Maybe a) -> Maybe a
scrapFstForest forest scr = mapLazy ((\scr tree -> (scrapFst tree scr)) scr) forest

{-
  Encuentra el primer nodo que comple con la selección y retorna el
  resultados de aplicar la funcion especificada en el Scraper sobre el nodo.
-}
scrapFst :: Tree -> Scraper (Maybe a) -> Maybe a
scrapFst node@(TagLeaf tag@(TagOpen name attrs)) (Scraper [s] f)
     | (checkSelTag tag s) = f (TagBranch name attrs [])
     | otherwise = Nothing
scrapFst node@(TagLeaf tag) (Scraper [s] f)
     | (checkSelTag tag s) = f node
     | otherwise = Nothing
scrapFst node@(TagLeaf tag) _ = Nothing
scrapFst node@(TagBranch name attrs trees) (Scraper (s:ss) f)
     | (checkSelNode node s) && (length ss == 0) = (f node)
     | (checkSelNode node s) = mapLazy ((\s t -> (scrapFst t s)) (Scraper ss f)) trees
     | otherwise = mapLazy ((\s t -> (scrapFst t s)) (Scraper (s:ss) f)) trees

{-
 Dada una lista de elementos del tipo Maybe que contienen listas,
 se queda con los elementos distintos a Nothing quitandole el constructor Just
 a cada elemento concatenando con los demás resultados y añadiendole Just al
 resultado final en caso que la lista no sea vacía.
-}
cleanMaybe :: [Maybe [a]] -> Maybe [a]
cleanMaybe [] = Nothing
cleanMaybe xs = case [x | (Just x) <- xs ] of
                     [] -> Nothing
                     ys -> Just (concat ys)

{-
  Dada una lista de elementos del tipo Maybe, se queda con los elementos
  distintos a Nothing quitandole el constructor Just a cada elemento y
  añadiendoselo al resultado final en caso que la lista no sea vacía.
-}
cleanMaybe' :: [(Maybe a)] -> Maybe [a]
cleanMaybe' [] = Nothing
cleanMaybe' xs = case [x | (Just x) <- xs ] of
                    [] -> Nothing
                    ys -> Just ys

{-
  Encuentra todos los nodos que complen con la selección y retorna una lista
  de los resultados de aplicar la funcion especificada en el Scraper.
-}
scrapAll :: Tree -> Scraper (Maybe a) -> Maybe [a]
scrapAll node@(TagLeaf tag@(TagOpen name attrs)) (Scraper [s] f)
     | (checkSelTag tag s) = cleanMaybe' $ [f (TagBranch name attrs [])]
     | otherwise = Nothing
scrapAll node@(TagLeaf tag) (Scraper [s] f)
     | (checkSelTag tag s) = cleanMaybe' $ [f node]
     | otherwise = Nothing
scrapAll node@(TagLeaf tag) _ = Nothing
scrapAll node@(TagBranch name attrs trees) (Scraper (s:ss) f)
    | (checkSelNode node s) && (length ss == 0) = cleanMaybe' [(f node)]
    | (checkSelNode node s) = cleanMaybe $ map ((\s t -> (scrapAll t s)) (Scraper ss f)) trees
    | otherwise = cleanMaybe $ map ((\s t -> (scrapAll t s)) (Scraper (s:ss) f)) trees

{-
  Aplica la funcion a cada arbol, retornando para cada uno un resultado
  luego limpia la lista del tipo Maybe para que queden solo los resultados
  exitosos.
-}
applyToEveryTree :: (Tree -> Maybe a) -> [Tree] -> [a]
applyToEveryTree scrapFun trees = fromMaybe [] $ cleanMaybe' $ map scrapFun trees

{-
  Retorna el n-esimo hijo que no es un TagLeaf.
-}
childreN :: Tree -> Int -> Maybe Tree
childreN (TagBranch _ _ ts) n = treeNoLeafN ts n

treeNoLeafN :: [Tree] -> Int -> Maybe Tree
treeNoLeafN [] n = Nothing
treeNoLeafN ((TagLeaf _):ts) n = treeNoLeafN ts n
treeNoLeafN (t@(TagBranch _ _ _):ts) 1 = Just t
treeNoLeafN ((TagBranch _ _ _):ts) n = treeNoLeafN ts (n-1)

scrapFstAfter :: Tree -> [Int] -> Scraper (Maybe a) -> Maybe a
scrapFstAfter t [] scr = scrapFst t scr
scrapFstAfter t (x:xs) scr = do child <- childreN t x
                                scrapFstAfter child xs scr

scrapAllAfter :: Tree -> [Int] -> Scraper (Maybe a) -> Maybe [a]
scrapAllAfter t [] scr = scrapAll t scr
scrapAllAfter t (x:xs) scr =  do child <- childreN t x
                                 scrapAllAfter child xs scr
