{-# LANGUAGE DuplicateRecordFields #-}

import qualified Data.Map as Map

type TNode = String
type TText = String

data NodeType = TNode | TText deriving Eq
data NodeState = Unchanged | Changed | Deleted deriving (Enum, Eq)
instance Show NodeType where
  show TNode = "Node"
  show TText = "Text"

type Props = Map.Map String String

createProps :: [(String, String)] -> Props
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
props2 = Map.fromList [("class", "test2"), ("id", "3")]

mA = Map.difference props1 props2
node1 = createNode TNode (Map.fromList [("class", "test")]) []
node2 = createNode TText (Map.fromList [("class", "test"), ("newProp", "woo")]) [node1]

type NodePatch = Node

-- vPropsDiff :: Props -> Props -> Props
-- vPropsDiff p1 p2 = Map.map

vDiff :: Node -> Node -> NodePatch
vDiff (Node n1type n1props n1children) (Node n2type n2props n2children) = patch
  where
      isSameType = n1type == n2type
      isSamePropsLen = Map.size n1props == Map.size n2props
      isSameChildren = length n1children == length n2children
      patch
        | not isSameType = vDiff (Node n2type n1props n1children) (Node n2type n2props n2children)
        | not isSamePropsLen = vDiff (Node n1type n2props n1children) (Node n2type n2props n2children)
        | not isSameChildren = vDiff (Node n1type n1props n2children) (Node n2type n2props n2children)
        | otherwise = Node n1type n1props n1children
