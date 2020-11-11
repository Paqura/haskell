type Div = String
type Span = String

data NodeType = Div | Span deriving Eq
data NodeState = Unchanged | Changed | Deleted deriving (Enum, Eq)

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

type NodeDiff = String

getDiff :: Eq a => Node a -> Node a -> Bool
getDiff (Node aId aType aState aValue aChildren) (Node cId cType cState cValue cChildren)
  | aId /= cId = True
  | aType /= cType = True
  | aState /= cState = True
  | aValue /= cValue = True
  | null aChildren /= null cChildren = True
  | otherwise = getDiff (head aChildren) (head cChildren)

node1 = createNode 0 Div Unchanged 666 []
node2 = createNode 1 Div Unchanged 666 []

data NodePatch a = NodePatch {
      id :: Int
    , nodeType :: NodeType
    , state :: NodeState
    , value :: a
    , children :: [Node a]
}

{-
  Основная идея,которую надо реализовать:


  есть два дерева типа tree

  tree = {
    type: 'div' | 'span' | '...'
    props: // как-то в типе описать
    children: [0..Infinity] - массив деревьев или пустой массив
  }

  их нужно сравнить и сделать патч

  patch = tree

  https://github.com/Matt-Esch/virtual-dom/blob/master/vtree/diff.js

-}