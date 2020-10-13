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
  }


printNode :: Show a => Node a -> String
printNode (Node id nodeType state value) =
  mconcat [
    "{", "\n",
    " id:", show id, "\n",
    " nodeType:", show nodeType, "\n",
    " state:", show state, "\n",
    " value:", show value, "\n",
    "}"
  ]

instance Show a => Show (Node a) where
  show (Node id nodeType state value) = printNode (Node id nodeType state value)

createNode :: Int -> NodeType -> NodeState -> a -> Node a
createNode = Node


