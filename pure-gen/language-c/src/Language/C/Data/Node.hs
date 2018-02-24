{-# LANGUAGE DeriveDataTypeable #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Language.C.Syntax.Attributes
-- Copyright   :  (c) [1995..1999] Manuel M. T. Chakravarty
--                (c) 2008 Benedikt Huber (stripped radically)
-- License     :  BSD-style
-- Maintainer  :  benedikt.huber@gmail.com
-- Stability   :  experimental
-- Portability :  ghc
--
-- source position and unqiue name
-----------------------------------------------------------------------------
module Language.C.Data.Node (
   NodeInfo(..), undefNode, isUndefNode,
   mkNodeInfoOnlyPos,mkNodeInfoPosLen, mkNodeInfo,mkNodeInfo',
   internalNode, -- deprecated, use undefNode
   CNode(nodeInfo), fileOfNode,
   posOfNode, nameOfNode, getLastTokenPos, lengthOfNode,
   eqByName,
) where
import Language.C.Data.Position
import Language.C.Data.Name     (Name)
import Data.Generics

-- | Parsed entity attribute
data NodeInfo = OnlyPos  Position {-# UNPACK #-} !PosLength        -- only pos and last token (for internal stuff only)
              | NodeInfo Position {-# UNPACK #-} !PosLength !Name  -- pos, last token and unique name
           deriving (Data,Typeable,Eq,Ord)

instance Show NodeInfo where
    showsPrec d (OnlyPos p l) =
      (showString "(OnlyPos ") . (showsPrec d p) . (showString " ") . (showsPrec d l) . (showString ")")
    showsPrec d (NodeInfo p l n) =
      (showString "(NodeInfo ") . (showsPrec d p) . (showString " ") . (showsPrec d l) . (showString " ") . (showsPrec d n) . (showString ")")

-- name equality of attributes, used to define (name) equality of objects
--instance Eq NodeInfo where
--  (NodeInfo   _ _ id1) == (NodeInfo   _ _ id2) = id1 == id2
--  _               == _               =
--    error "Attributes: Attempt to compare `OnlyPos' attributes!"

-- attribute ordering
--instance Ord NodeInfo where
--  (NodeInfo   _ _ id1) <= (NodeInfo   _ _ id2) = id1 <= id2
--  _               <= _               =
--    error "Attributes: Attempt to compare `OnlyPos' attributes!"

instance Pos NodeInfo where
  posOf (OnlyPos pos _) = pos
  posOf (NodeInfo pos _ _) = pos

-- | get the number of characters an AST node spans
lengthOfNode :: NodeInfo -> Maybe Int
lengthOfNode ni = len
    where
    len = case ni of NodeInfo firstPos lastTok _ -> computeLength firstPos lastTok
                     OnlyPos firstPos lastTok -> computeLength firstPos lastTok
    computeLength pos (lastPos,len) | len < 0   = Nothing
                                    | otherwise = Just (posOffset lastPos + len - posOffset pos)

-- | get the position and length of the last token
getLastTokenPos :: NodeInfo -> PosLength
getLastTokenPos (NodeInfo _ lastTok _) = lastTok
getLastTokenPos (OnlyPos _ lastTok) = lastTok

-- | a class for convenient access to the attributes of an attributed object
class CNode a where
  nodeInfo :: a -> NodeInfo
instance CNode NodeInfo where
  nodeInfo = id
instance (CNode a, CNode b) => CNode (Either a b) where
  nodeInfo = either nodeInfo nodeInfo

nameOfNode :: NodeInfo -> Maybe Name
nameOfNode (OnlyPos _ _) = Nothing
nameOfNode (NodeInfo _ _ name) = Just name
posOfNode :: NodeInfo -> Position
posOfNode ni = case ni of (OnlyPos pos _) -> pos; (NodeInfo pos _ _) -> pos
fileOfNode :: (CNode a) => a -> Maybe FilePath
fileOfNode = fmap posFile . justIf isSourcePos . posOfNode . nodeInfo where
    justIf predicate x | predicate x = Just x
                       | otherwise   = Nothing

-- | equality by name
eqByName           :: CNode a => a -> a -> Bool
eqByName obj1 obj2  = (nodeInfo obj1) == (nodeInfo obj2)


-- attribute identifier creation
-- -----------------------------
{-# DEPRECATED internalNode "use undefNode instead" #-}
internalNode :: NodeInfo
internalNode = undefNode

-- | create a node with neither name nor positional information
undefNode :: NodeInfo
undefNode = OnlyPos nopos (nopos,-1)

-- | return True if the node carries neither name nor positional information
isUndefNode :: NodeInfo -> Bool
isUndefNode (OnlyPos p _) | isNoPos p = True
                          | otherwise = False
isUndefNode _ = False

-- |
-- | Given only a source position, create a new node attribute
mkNodeInfoOnlyPos :: Position -> NodeInfo
mkNodeInfoOnlyPos pos  = OnlyPos pos (nopos,-1)

-- | Given a source position and the position and length of the last token, create a new node attribute
mkNodeInfoPosLen :: Position -> PosLength -> NodeInfo
mkNodeInfoPosLen = OnlyPos

-- | Given a source position and a unique name, create a new attribute
-- identifier
mkNodeInfo :: Position -> Name -> NodeInfo
mkNodeInfo pos name  = NodeInfo pos (nopos,-1) name

-- | Given a source position, the position and length of the last token and a unique name, create a new attribute
-- identifier. Strict in
mkNodeInfo' :: Position -> PosLength -> Name -> NodeInfo
mkNodeInfo' pos lasttok name = NodeInfo pos lasttok name

