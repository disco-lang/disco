
-------------------------------------
--Tries
--
-- data VTrie a where
--   SumTrie :: IntMap (VTrie a) -> VTrie a
--   ProdTrie :: VTrie (VTrie a) -> VTrie a
--   Singleton :: [Value] -> a -> VTrie a
--   Empty :: VTrie a
--
-- -- trieLookup :: Value -> VTrie a -> Disco (Maybe a)
-- -- trieLookup _ Empty = return Nothing
-- -- trieLookup (VNum _ r) (SumTrie m) = do
-- --   let maybeTrie = IntMap.lookup (numerator r) m
-- --   case maybeTrie of
-- --     Just (SumTrie m') -> do
-- --        let maybeSingleton = IntMap.lookup (denominator r) m'
-- --        case maybeSingleton of
-- --          Just (Singleton [] a) -> return $ Just a
-- --          _                     -> return Nothing
-- --     Nothing -> return Nothing
--
--
-- class TrieKey k where
--   adjustKey :: k -> (Maybe a -> Maybe a) -> VTrie a -> Disco (Maybe a, VTrie a)
--   buildTrie :: k -> a -> Disco (VTrie a)
--
-- insertKey :: (TrieKey k) => k -> a -> VTrie a -> Disco (VTrie a)
-- insertKey k a t = adjustKey k (const (Just a)) t
--
-- lookupKey :: k -> VTrie a -> Disco (Maybe a, VTrie a)
-- lookupKey k t = adjustKey k id t
--
-- instance TrieKey () where
--   adjustKey () f Empty = return $ (Nothing, maybe Empty Leaf (f Nothing))
--   adjustKey () f (Leaf a) = return $ (Just a, maybe Empty Leaf (f (Just a)))
--
--   buildTrie () = Leaf
--
-- pattern Leaf a = Singleton [] a
--
-- expandSingleton :: Value -> [Value] -> a -> Disco (VTrie a)
-- expandSingleton v vs a = do
--   t <- buildTrie v (Singleton vs a)
--   return $ ProdTrie t
--
-- instance (TrieKey j, TrieKey k) => TrieKey (j,k) where
--   adjustKey (j,k) f Empty = case f Nothing of
--     Nothing -> return (Nothing, Empty)
--     Just a  -> do
--       newTrie <- buildTrie (j,k) a
--       return (Nothing, newTrie)
--   adjustKey (j,k) f (Singleton (v:vs) a) = do
--     newTrie <- expandSingleton v vs a
--     adjustKey (j,k) f newTrie
--   adjustKey (j,k) f (ProdTrie t) = do
--     let g Nothing  = fmap (buildTrie k) (f Nothing)
--         g (Just u) = adjustKey k f u
--     (mTrie, t') <- adjustKey j g t
--     case mTrie of
--       Nothing -> return (Nothing, t')
--       Just u  -> do
--         (kValue, u') <- lookupKey k u
--         updatedTrie <- ProdTrie $ insertKey j u' t'
--         return $ (kValue, updatedTrie)

--adjustKey (j,k)
