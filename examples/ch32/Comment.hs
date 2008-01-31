module Comment where

import Control.Concurrent.STM.TVar (TVar)
import qualified Data.Map as M

newtype ElementID = ElementID {
      fromElementID :: String
    } deriving (Eq, Ord, Show)

data Element = Element {
      eltID :: ElementID
    , eltChapter :: String
    , eltSection :: String
    } deriving (Eq, Ord, Show)

data Comment = Comment {
      cmtElement :: Element
    , cmtComment :: String
    , cmtSubmitter :: String
    , cmtURL :: String
    , cmtIP :: String
    , cmtDate :: Int
    , cmtReviewed :: Bool
    , cmtHidden :: Bool
    } deriving (Eq, Ord, Show)

data AppState = AppState {
      appElements :: TVar (M.Map ElementID Element)
    , appComments :: TVar (M.Map ElementID [Comment])
    }
