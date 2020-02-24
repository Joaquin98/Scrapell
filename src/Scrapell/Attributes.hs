{-# LANGUAGE TypeSynonymInstances, FlexibleInstances, PatternGuards #-}

module Scrapell.Attributes (
  matchAttrsNames ,
  fun ,
  startsWith ,
  hasString ,
  hasAttr ,
  toAttr ,
  anyAttrPredicate ,
  notP ,
  checkPredicates
) where
import Scrapell.Types
import Data.Char (toLower)
import Data.List
import Control.Applicative ((<$>))

matchAttrsNames :: AttrName -> String -> Bool
matchAttrsNames (AttrString name) name' = (toLower <$> name) == (toLower <$> name')
matchAttrsNames _ _ = True

{-
  Dada una función (predicado) crea un predicado que vale si el anterior
  vale para alguno de los atributos.
-}
anyAttrPredicate :: (Attr -> Bool) -> AttrPredicate
anyAttrPredicate p = AttrPredicate $ any p

{-
  Operador que dada una funcion crea un predicado.
-}
fun :: (String -> String -> Bool) -> AttrPredicate
fun f = anyAttrPredicate $ \(name,value) -> f name value
{-
  Dado un predicado crea el predicado negado.
-}
notP :: AttrPredicate -> AttrPredicate
notP attrPred = AttrPredicate (not . (runAttrPredicate attrPred))

{-
  Crea un predicado que se fija si el valor del atributo empieza con
  determinada string.
-}
startsWith :: (AttrRep a) => a -> String -> AttrPredicate
name `startsWith` prefix = fun $ (\ n v -> (matchAttrsNames (toAttr name) n) && isPrefixOf prefix v)

{-
  Crea un predicado que se existe algun atributo con un nombre
  determinado.
-}
hasAttr :: (AttrRep a) => a -> AttrPredicate
hasAttr name = fun $ \n v -> (matchAttrsNames (toAttr name) n)

{-
  Crea un predicado que se fija si el valor del atributo contiene una
  determinada string.
-}
hasString :: (AttrRep a) => a -> String -> AttrPredicate
name `hasString` prefix = fun $ \n v -> (matchAttrsNames (toAttr name) n) && isInfixOf prefix v


{-
  Dada una lista de predicados sobre atributos y una lista de atributos
  verifica que todos se cumplan para esos atributos.
-}
checkPredicates :: [AttrPredicate] -> [Attr] -> Bool
checkPredicates attrPreds attrs = foldr (\p b -> b && (runAttrPredicate p) attrs) True attrPreds


-- (TagString "div") & [(AttrString "pepe")==="juan"] +++ (TagString "div") & [(AttrString "pepe")==="juan"]
