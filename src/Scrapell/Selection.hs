{-# LANGUAGE TypeSynonymInstances, FlexibleInstances, PatternGuards #-}

module Scrapell.Selection(
  selectText ,
  (&),(===),(-->)
) where
import Scrapell.Types
import Scrapell.Attributes


selectText :: Selection
selectText = [SelectText]

{-
  Operador que dado un nombre de etiqueta y la lista de
  predicados de atributos genera una seleccion.
-}
(&) :: (TagNameRep t) => t -> [AttrPredicate] -> Selection
(&) tagName attrPredicates = [SelectNode (toTag tagName) attrPredicates]
infixl 8 &
{-
  Operador que dado el nombre del atributo y el valor genera un
  predicado que verifique que exista algún atributo con ese nombre
  y ese valor en el nodo.
-}
(===) :: (AttrRep a) => a -> String -> AttrPredicate
(===) name val = anyAttrPredicate $ \(name',val') -> matchAttrsNames (toAttr name) name'
                                    && val == val'
infixl 9 ===

{-
  Operador que combina dos selecciones.
-}
(-->) :: (SelectionRep a, SelectionRep b) => a -> b -> Selection
(-->) s s' = (toSelection s) ++ (toSelection s')
infixl 7 -->
