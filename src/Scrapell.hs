module Scrapell (
 -- Types
 Tree (..), TagName (..),Attr,AttrName (..),AttrPredicate (..),
 SelectNode (..), Selection,Scraper (..),TagNameRep (..),AttrRep (..),
 SelectionRep (..),
 -- Selection
 selectText,(&),(===),(-->),
 -- Scraper
 tagsToString,strToTree,scrapFst,scrapAll,text,attr,node,cleanMaybe,
 scrapFstForest,scrapAllForest,cleanMaybe',applyToEveryTree,childreN,
 scrapFstAfter,scrapAllAfter,textPred,
 -- Download
 downloadPageAs,openItem,openItems,
 -- Attributes
 fun,startsWith,hasString,hasAttr,notP,checkPredicates,
 -- Extra
 removeDups,getDomain,addDomain,addHttps,stringToDouble,priceToDouble
)
where
import Scrapell.Types
import Scrapell.Attributes
import Scrapell.Selection
import Scrapell.Scraper
import Scrapell.Download
import Scrapell.Tags
import Scrapell.Extra
