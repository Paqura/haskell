{-# LANGUAGE DuplicateRecordFields #-}

import qualified Data.Map as Map
import Data.Maybe

type TNode = String
type TText = String

data NodeType = TNode | TText deriving Eq
data NodeState = Unchanged | Changed | Deleted deriving (Enum, Eq)
instance Show NodeType where
  show TNode = "Node"
  show TText = "Text"

type Key = String
type Value = String
-- можно и Props k v = Map.Map k v
type Props = Map.Map Key Value

createProps :: [(Key, Value)] -> Props
createProps = Map.fromList

type Children = [Node]
data Node = Node
  {
      nodeType :: NodeType
    , props :: Props
    , children :: Children
  }

printNode :: Node -> String
printNode (Node nodeType props children) =
  mconcat [
    "{", "\n",
    " nodeType:", show nodeType, "\n",
    " props:", show props, "\n",
    " children:", show children, "\n",
    "}"
  ]

instance Show Node where
  show (Node nodeType props children) = printNode (Node nodeType props children)

createNode :: NodeType -> Props -> Children -> Node
createNode nodeType props children = Node nodeType props allChildren
  where
    childFree = null children
    Node cType cProps cChildren = head children
    allChildren
          | childFree = []
          | otherwise = [createNode cType cProps cChildren]

props1 :: Props
props1 = Map.fromList [("id", "2"), ("class", "test")]

props2 :: Props
props2 = Map.fromList [("class", "test2"), ("id", "3"), ("unexpected", "woooh")]

node1 = createNode TNode (Map.fromList [("class", "test")]) []
node2 = createNode TText (Map.fromList [("class", "test"), ("newProp", "woo")]) [node1]

-- TODO: разобрать, возможно тут можно упростить или найти готовую реализацию
-- insertMaybePair :: Ord k => Map.Map k v -> (k, Maybe v) -> Map.Map k v
-- insertMaybePair myMap (_, Nothing) = myMap
-- insertMaybePair myMap (key, Just value) = Map.insert key value myMap

type NodePatch = Node

checkAndR :: String -> Props -> String
checkAndR a p
        | isJust v = fromJust v
        | otherwise = "empty"
  where
    v = Map.lookup a p

getMaybeValueWithFallback :: Key -> Props -> Value
getMaybeValueWithFallback key props = fromMaybe "" (Map.lookup key props)

vPropsDiff :: Props -> Props -> Props
vPropsDiff p1 p2 = Map.foldl (\k v -> Map.insert v (getMaybeValueWithFallback v p2) k) p1 p2

-- vDiff :: Node -> Node -> NodePatch
-- vDiff (Node n1type n1props n1children) (Node n2type n2props n2children) = patch
--   where
--       isSameType = n1type == n2type
--       propsDiffTuple = vPropsDiff n1props n2props
--       isSameProps = fst propsDiffTuple
--       isSameChildren = length n1children == length n2children
--       patch
--         | not isSameType = vDiff (Node n2type n1props n1children) (Node n2type n2props n2children)
--         | not isSameProps = vDiff (Node n1type n2props n1children) (Node n2type n2props n2children)
--         | not isSameChildren = vDiff (Node n1type n1props n2children) (Node n2type n2props n2children)
--         | otherwise = Node n1type n1props n1children
