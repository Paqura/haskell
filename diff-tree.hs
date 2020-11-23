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
data Node = Node
  {
      nodeType :: NodeType
    , props :: Props
    , children :: [Node]
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

createNode :: NodeType -> Props -> [Node] -> Node
createNode nodeType props children = Node nodeType props allChild
  where
    isLastChild = null children
    Node cType cProps cChildren = head children
    allChild
          | isLastChild = []
          | otherwise = [createNode cType createProps cProps cChildren]

