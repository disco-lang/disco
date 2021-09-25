-- | Allocate a new memory cell for the given value, and return its
--   location.
allocate :: Members '[Store Cell, Output Debug] r => Value -> Sem r Loc
allocate v = do
  loc <- new (mkCell v)
  debug $ "allocating " ++ show v ++ " at location " ++ show loc
  return loc

-- | Turn a value into a "simple" value which takes up a constant
--   amount of space: some are OK as they are; for others, we turn
--   them into an indirection and allocate a new memory cell for them.
mkSimple :: Members '[Store Cell, Output Debug] r => Value -> Sem r Value
mkSimple v@VNum{}         = return v
mkSimple v@VUnit{}        = return v
mkSimple v@(VInj _ VUnit) = return v
mkSimple v@VConst{}       = return v
mkSimple v@VClos{}        = return v
mkSimple v@VType{}        = return v
mkSimple v@VIndir{}       = return v
mkSimple v                = VIndir <$> allocate v

-- | Delay a computation by packaging it into a @VDelay@ constructor
--   along with the current environment.
delay :: Members EvalEffects r => (forall r'. Members EvalEffects r' => Sem r' Value) -> Sem r Value
delay = delay' []

-- | Like 'delay', but also specify a set of values which will be
--   needed during the delayed computation, to prevent any memory
--   referenced by the values from being garbage collected.
delay' :: Members EvalEffects r => [Value] -> (forall r'. Members EvalEffects r' => Sem r' Value) -> Sem r Value
delay' vs imv = do
  ls <- getReachable vs
  VDelay imv ls <$> getEnv

-- | Turn a Core expression into a value.  Some kinds of expressions
--   can be turned into corresponding values directly; for others we
--   create a thunk by packaging up the @Core@ expression with the
--   current environment.  The thunk is stored in a new location in
--   memory, and the returned value consists of an indirection
--   referring to its location.
mkValue :: Members '[Reader Env, Store Cell, Output Debug] r => Core -> Sem r Value
mkValue (CConst op)   = return $ VConst op
mkValue CUnit         = return VUnit
mkValue (CInj s v)    = VInj s <$> mkValue v
mkValue (CPair v1 v2) = VPair <$> mkValue v1 <*> mkValue v2
mkValue (CNum d r)    = return $ VNum d r
mkValue (CType ty)    = return $ VType ty
mkValue c             = VIndir <$> (allocate . VThunk c =<< getEnv)

-- | Deallocate any memory cells which are no longer recursively
--   referenced by any top-level binding.
garbageCollect :: Members '[State TopInfo, Store Cell] r => Sem r ()
garbageCollect = do
  env  <- gets @TopInfo (view topEnv)
  keep <- getReachable env
  keepKeys keep

-- | Get the set of memory locations reachable from a set of values.
getReachable :: (Reachable v, Members '[Store Cell] r) => v -> Sem r IntSet
getReachable = execState IntSet.empty . mark

class Reachable v where
  -- | @mark v@ marks the memory locations reachable from the values
  --   stored in @v@.
  mark :: Members '[Store Cell, State IntSet] r => v -> Sem r ()

instance Reachable Value where
  mark (VInj _ v)      = mark v
  mark (VPair v1 v2)   = mark v1 >> mark v2
  mark (VClos _ e)     = mark e
  mark (VPAp v vs)     = mark (v:vs)
  mark (VThunk _ e)    = mark e
  mark (VIndir l)      = mark l
  mark (VDelay _ ls e) = (modify @IntSet $ IntSet.union ls) >> mark e
  mark (VBag vs)       = mark (map fst vs)
  mark (VProp p)       = mark p
  mark (VGraph _ adj)  = mark adj
    -- A graph can only contain SimpleValues, which by def contain no indirection.
    -- However its buffered adjacency map can.
  mark (VMap m)        = mark (M.elems m)
  mark _               = return ()

instance Reachable Env where
  mark = mark . M.elems

instance Reachable v => Reachable [v] where
  mark = mapM_ mark

instance Reachable ValProp where
  mark (VPDone (TestResult _ r vs)) = mapM_ mark r >> mark vs
  mark (VPSearch _ _ v vs)          = mark v >> mark vs

instance Reachable TestEnv where
  mark (TestEnv te) = forM_ te $ \(_, _, v) -> mark v

instance Reachable Loc where
  mark l = do
    reach <- get @IntSet
    case IntSet.member l reach of
      True -> return ()
      False -> do
        modify $ IntSet.insert l
        mc <- lookupStore l
        case mc of
          Nothing         -> return ()
          Just (Cell v _) -> mark v

-- | Show the current contents of memory, for debugging purposes.
showMemory :: Members '[Store Cell, Output String] r => Sem r ()
showMemory = assocsStore >>= mapM_ showCell
  where
    showCell :: Member (Output String) r => (Int, Cell) -> Sem r ()
    showCell (i, Cell v b) = output $ printf "%3d%s %s\n" i (if b then "!" else " ") (show v)


------------------------------------------------------------


loadDefs cenv = do

  -- Clear out any leftover memory.
  clearStore

  -- Take the environment mapping names to definitions, and turn
  -- each one into an indirection to a thunk stored in memory.
  env <- traverse mkValue cenv

  -- Top-level definitions are allowed to be recursive, so each
  -- one of those thunks should actually have the environment
  -- 'env' as its environment, so all the top-level definitions
  -- can mutually refer to each other.
  --
  -- For now we know that the only things we have stored in memory
  -- are the thunks we just made, so just iterate through them and
  -- replace their environments.
  mapStore (replaceThunkEnv env)

  -- Finally, set the top-level environment to the one we just
  -- created.
  modify @TopInfo (topEnv .~ env)

  where
    replaceThunkEnv e (Cell (VThunk c _) b) = Cell (VThunk c e) b
    replaceThunkEnv _ c                     = c
