{-# LANGUAGE TypeSynonymInstances, FlexibleInstances, PatternGuards #-}

module Scrapell.Types (
  Tree (..),
  TagName (..),
  Attr,
  AttrName (..),
  AttrPredicate (..),
  SelectNode (..),
  Selection,
  Scraper (..),
  TagNameRep (..),
  AttrRep (..),
  SelectionRep (..)
)
where
import Text.HTML.TagSoup
import Text.HTML.TagSoup.Tree

{-
data Tag str =
     TagOpen str [Attribute str]  -- ^ An open tag with 'Attribute's in their original order
   | TagClose str                 -- ^ A closing tag
   | TagText str                  -- ^ A text node, guaranteed not to be the empty string
   | TagComment str               -- ^ A comment
   | TagWarning str               -- ^ Meta: A syntax error in the input file
   | TagPosition !Row !Column     -- ^ Meta: The position of a parsed element
     deriving (Show, Eq, Ord, Data, Typeable)
-}

{-
  data TagTree str
    = -- | A 'TagOpen'/'TagClose' pair with the 'Tag' values in between.
      TagBranch str [Attribute str] [TagTree str]
    | -- | Any leaf node
      TagLeaf (Tag str)
                   deriving (Eq,Ord,Show)
-}

type Tree = TagTree String

{-
  Tipo de datos que representa el nombre de los tags,
  permitiendo aceptar también cualquier nombre de tag.
-}
data TagName = AnyTag | TagString String

{-
  data Attribute str = (str,str)
-}
type Attr = Attribute String

{-
  Tipo de datos que representa el nombre de los atributos,
  permitiendo aceptar también cualquier nombre de atributo.
-}
data AttrName = AnyAttr | AttrString String deriving Show

{-
  Tipo de dato que representa un predicado sobre un argumento.
-}
data AttrPredicate = AttrPredicate {runAttrPredicate :: ([Attr] -> Bool)}


data SelectNode = SelectNode TagName [AttrPredicate]
                | SelectText
                | SelectAny

type Selection = [SelectNode]


{-
  Un scraper tiene una seleccion que indica como buscar el nodo
  y una funcion que se le debe aplicar una vez encontrado.
-}
data Scraper a = Scraper {sSelection :: Selection , sToDo :: (Tree -> a)}


{-
  Para poder escribir un atributo solo como un string
  creo una clase que sea representacion del tipo atributo.
-}
class AttrRep a where
  toAttr :: a -> AttrName

instance AttrRep AttrName where
  toAttr = id

instance AttrRep String where
  toAttr [] = AnyAttr
  toAttr name = AttrString name

{-
  Para poder escribir un tag solo como un string
  creo una clase que sea representacion del tipo tag.
-}
class TagNameRep a where
  toTag :: a -> TagName

instance TagNameRep TagName where
  toTag = id

instance TagNameRep String where
  toTag [] = AnyTag
  toTag name = TagString name

{-
  Para poder escribir una seleccion solo como un string
  creo una clase que sea representacion del tipo seleccion.
-}
class SelectionRep a where
  toSelection :: a -> Selection

instance SelectionRep Selection where
  toSelection = id

instance SelectionRep String where
  toSelection name = [SelectNode (toTag name) []]
