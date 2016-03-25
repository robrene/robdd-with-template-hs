{-# LANGUAGE PatternSynonyms #-}

module BoolExpr.ROBDD where

--
-- Data structure:

type NodeId = Int

-- Ordered binary decision diagram.
data OBDD =
    OBDDZero
  | OBDDOne
  | OBDDNode OBDD Int OBDD NodeId
  | OBDDNRef OBDD Int OBDD NodeId
  deriving (Show, Eq)

data ROBDD = ROBDD OBDD RevMap

data RevMap = EmptyRevMap

--
-- Construction:

fold :: (a -> Int -> a -> a) -> a -> a -> OBDD -> a
fold f ez eo obdd = fst (fold' emptyMemoMap obdd)
  where fold' memomap OBDDZero = (ez, memomap)
        fold' memomap OBDDOne  = (eo, memomap)
        fold' memomap (OBDDNRef _ _ _ nodeId) =
          let Just v = lookupMemoMap nodeId memomap
           in (v, memomap)
        fold' memomap (OBDDNode low var high nodeId) =
          let (lowV, lowMemomap) = fold' memomap low
              (highV, highMemomap) = fold' lowMemomap high
              v = f lowV var highV
           in (v, insertMemoMap nodeId v highMemomap)

rOBDD :: OBDD -> Int -> OBDD -> RevMap -> ROBDD
rOBDD low var high revmap
  | getId low == getId high = ROBDD low revmap
  | otherwise = case lookupRevMap low var high revmap of
      Just obdd -> ROBDD obdd revmap
      Nothing   -> rOBDD2 low var high revmap

rOBDD2 :: OBDD -> Int -> OBDD -> RevMap -> ROBDD
rOBDD2 low var high revmap =
  let obdd = OBDDNode low var high (nextId revmap)
   in ROBDD obdd (insertRevMap low var high (toRef obdd) revmap)

emptyMemoMap = undefined
lookupMemoMap _ _ = undefined
insertMemoMap _ _ _ = undefined

getId :: OBDD -> NodeId
getId _ = undefined

lookupRevMap :: OBDD -> Int -> OBDD -> RevMap -> Maybe OBDD
lookupRevMap _ _ _ _ = undefined

nextId :: RevMap -> NodeId
nextId _ = undefined

insertRevMap :: OBDD -> Int -> OBDD -> OBDD -> RevMap -> RevMap
insertRevMap _ _ _ _ _ = undefined

toRef :: OBDD -> OBDD
toRef (OBDDNode left i right k) = OBDDNRef left i right k
toRef x = x
