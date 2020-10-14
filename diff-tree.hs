type Div = String
type Span = String

data NodeType = Div | Span
data NodeState = Unchanged | Changed | Deleted deriving Enum

instance Show NodeType where
  show Div = "div"
  show Span = "span"

instance Show NodeState where
  show Unchanged = "unchanged"
  show Changed = "changed"
  show Deleted = "deleted"

data Node a = Node
  {
      id :: Int
    , nodeType :: NodeType
    , state :: NodeState
    , value :: a
    , children :: [Node a]
  }


printNode :: Show a => Node a -> String
printNode (Node id nodeType state value children) =
  mconcat [
    "{", "\n",
    " id:", show id, "\n",
    " nodeType:", show nodeType, "\n",
    " state:", show state, "\n",
    " value:", show value, "\n",
    " children:", show children, "\n",
    "}"
  ]

instance Show a => Show (Node a) where
  show (Node id nodeType state value children) = printNode (Node id nodeType state value children)

createNode :: Int -> NodeType -> NodeState -> a -> [Node a] -> Node a
createNode id nodeType state value children = Node id nodeType state value allChild
  where
    isLastChild = null children
    Node cId cType cState cValue cChildren = head children
    allChild
          | isLastChild = []
          | otherwise = [createNode cId cType cState cValue cChildren]
