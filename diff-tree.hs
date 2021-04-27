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

props3 :: Props
props3 = Map.fromList [("class", "test"), ("id", "2")]

node1 = createNode TNode props1 []
node2 = createNode TText props2 []

type NodePatch = Node

vPropsDiff :: Props -> Props -> Props
-- vPropsDiff oldProps newProps = Map.foldlWithKey (\acc key val -> Map.insert key val acc) oldProps newProps
vPropsDiff = Map.foldlWithKey (\acc key val -> Map.insert key val acc)

vDiff :: Node -> Node -> NodePatch
vDiff (Node n1type n1props n1children) (Node n2type n2props n2children) = patch
  where
      isSameType = n1type == n2type
      isSameProps = n1props == n2props
      propsPatch = vPropsDiff n1props n2props
      isLeftChildrenEmpty = null n1children
      isRightChildrenEmpty = null n2children
      isBothChildrenEmpty = isLeftChildrenEmpty && isRightChildrenEmpty
      patch
        | not isSameType = vDiff (Node n2type n1props n1children) (Node n2type n2props n2children)
        | not isSameProps = vDiff (Node n1type propsPatch n1children) (Node n2type n2props n2children)
        -- | not isBothChildrenEmpty = vDiff (head n1children) (head n2children)
        -- | not isSameChildren = vDiff (Node n1type n1props n2children) (Node n2type n2props n2children)
        | otherwise = Node n1type n1props n1children

