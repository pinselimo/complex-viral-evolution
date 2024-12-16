{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric  #-}
{-# LANGUAGE StrictData     #-}
{-# LANGUAGE BangPatterns   #-}
module DynamicMutations.Types.Tree where

import           Prelude hiding  (Foldable, Traversable)
import           Control.Applicative (Applicative(liftA2))
import           Control.Comonad
import           Control.DeepSeq (NFData)
import           Data.Traversable (foldMapDefault, Traversable)
import           Data.List       (sortBy)
import           Data.Foldable
import           Data.IntMap.Lazy (IntMap, findWithDefault)
import qualified Data.Sequence   as Seq
import qualified Data.Vector     as V
import           Data.Tree       (Tree (Node))
import           GHC.Generics    (Generic)

type Forest a = V.Vector (PhyloTree a)
data PhyloTree a = PT a (Forest a)
                    deriving (Show, Generic, NFData, Eq)

instance Functor PhyloTree where
        fmap = relationTreeMap

instance Comonad PhyloTree where
        extract = treeExtract
        extend = treeExtend

instance Foldable PhyloTree where
        foldMap = foldMapDefault

        foldr f z = \t -> go t z  -- Use a lambda to allow inlining with two arguments
          where
            go (PT x ts) = f x . foldr (\t k -> go t . k) id ts
        {-# INLINE foldr #-}

        foldl' f = go
          where go !z (PT x ts) = foldl' go (f z x) ts
        {-# INLINE foldl' #-}

        sum = foldlMap1' id (+)
        {-# INLINABLE sum #-}

        product = foldlMap1' id (*)
        {-# INLINABLE product #-}


foldlMap1' :: (a -> b) -> (b -> a -> b) -> PhyloTree a -> b
foldlMap1' f g =  -- Use a lambda to allow inlining with two arguments
  \(PT x ts) -> foldl' (foldl' g) (f x) ts
{-# INLINE foldlMap1' #-}

instance Traversable PhyloTree where
        traverse f = go
            where go (PT x ts) = liftA2 PT (f x) (traverse go ts)
        {-# INLINE traverse #-}

children :: PhyloTree a -> Forest a
children (PT _ f) = f

withTree :: (a -> Forest a -> b) -> PhyloTree a -> b
withTree f t
  | PT v ts <- t = f v ts
{-# INLINE withTree #-}

treeDo :: (a -> Forest a -> (b, Forest b)) -> PhyloTree a -> PhyloTree b
treeDo f t
  | PT x ts <- t = uncurry PT $ f x ts
{-# INLINE treeDo #-}

treeDoM :: Monad m => (a -> Forest a -> m (b, Forest b)) -> PhyloTree a -> m (PhyloTree b)
treeDoM f t
  | PT x ts <- t = uncurry PT <$> f x ts
{-# INLINE treeDoM #-}

relationTreeMap :: (a -> b) -> PhyloTree a -> PhyloTree b
relationTreeMap f (PT x ts) = PT (f x) (relationTreeMap f <$> ts)

treeExtract :: PhyloTree a -> a
treeExtract (PT x _) = x
{-# INLINE treeExtract #-}

treeExtend :: (PhyloTree a -> b) -> PhyloTree a -> PhyloTree b
treeExtend f t@(PT _ ts) = PT (f t) (treeExtend f <$> ts)
{-# INLINE treeExtend #-}

new :: (a -> Forest a -> PhyloTree a) -> a -> PhyloTree a
new constr x = constr x V.empty

broadcastWith :: a -> (a -> PhyloTree b -> (a, c)) -> PhyloTree b -> PhyloTree c
broadcastWith x f t = treeDo (\_ ts -> (v', broadcastWith x' f <$> ts)) t
     where (x', v') = f x t

-- Tree Compat
toStrTree :: (a -> String) -> PhyloTree a -> Tree String
toStrTree shw (PT v ts) = Node ("[] " <> shw v) $ toStrTree shw <$> foldr (:) [] ts

mapEach :: [a] -> (a -> [a] -> b) -> [b]
mapEach = go []
 where go xs (y:ys) f = f y (xs <> ys) : go (xs <> [y]) ys f
       go _  [] _     = []

mapEachS :: Seq.Seq a -> (a -> Seq.Seq a -> b) -> Seq.Seq b
mapEachS xs f = Seq.mapWithIndex go xs
 where go i x = f x (Seq.take i xs <> Seq.drop (i+1) xs)

mapEachV :: V.Vector a -> (a -> V.Vector a -> b) -> V.Vector b
mapEachV xs f = V.imap go xs
 where go i x = f x (V.take i xs <> V.drop (i+1) xs)

rebase :: PhyloTree a -> PhyloTree (PhyloTree a)
rebase t@(PT v ts) = PT t (mapEachV ts (rebase' (PT v)))
 where rebase' c (PT v' ts') others = let stem = c others
         in PT (PT v' (stem `V.cons` ts')) (mapEachV ts' (go (PT v') stem))
         where go c' st t' ts'' = rebase' c' t' (st `V.cons` ts'')

toVector :: Ord b => (a -> b) -> PhyloTree a -> V.Vector a
toVector f = V.fromList . fmap snd . sortBy (\(x,_) (y,_) -> compare x y) . foldl' (\acc v -> (f v, v) : acc) []

fromVector :: (b -> Int) -> V.Vector a -> PhyloTree b -> PhyloTree a
fromVector f v = fmap ((v V.!) . f)

buildTree :: IntMap (V.Vector Int) -> PhyloTree Int -> PhyloTree Int
buildTree m = treeDo (\i ts -> (i, (buildTree m <$> ts) <> (new PT <$> findWithDefault V.empty i m)))

compressTree :: (PhyloTree a -> Bool) -> PhyloTree a -> PhyloTree a
compressTree p = fmap snd . compress fst . extend go
        where go t = (p t, extract t)

compress :: (a -> Bool) -> PhyloTree a -> PhyloTree a
compress p = treeDo go
        where go x ts = (x, V.filter (p . extract) $ treeDo go <$> ts)

