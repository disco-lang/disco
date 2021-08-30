
-- | Pretty-printing of values, with output interleaved lazily with
--   evaluation.
prettyValue :: Members (Input TopInfo ': Output String ': EvalEffects) r => Type -> Value -> Sem r ()
prettyValue ty v = prettyV ty v >> output "\n"

-- | Pretty-printing of values, with output interleaved lazily with
--   evaluation.  Takes a continuation that specifies how the output
--   should be processed (which will be called many times as the
--   output is produced incrementally).
prettyV :: Members (Input TopInfo ': Output String ': EvalEffects) r => Type -> Value -> Sem r ()
prettyV ty = whnfV >=> prettyWHNF ty

-- | Pretty-print a value with guaranteed parentheses.  Do nothing for
--   tuples; add an extra set of parens for other values.
prettyVP :: Members (Input TopInfo ': Output String ': EvalEffects) r => Type -> Value -> Sem r ()
prettyVP ty@(_ :*: _) v = prettyV ty v
prettyVP ty           v = output "(" >> prettyV ty v >> output ")"

-- | Pretty-print a value which is already guaranteed to be in weak
--   head normal form.
prettyWHNF :: Members (Input TopInfo ': Output String ': EvalEffects) r => Type -> Value -> Sem r ()
prettyWHNF (TyUser nm args) v = do
  tymap <- inputs (view topTyDefs)
  case M.lookup nm tymap of
    Just (TyDefBody _ body) -> prettyWHNF (body args) v
    Nothing                 -> error "Impossible! TyDef name does not exist in TyMap"
prettyWHNF TyUnit          VUnit        = output "■"
prettyWHNF TyProp          _            = prettyPlaceholder TyProp
prettyWHNF TyBool          (VInj s _)   = output $ map toLower (show (selectSide s False True))
prettyWHNF TyC             (VNum _ c)   = output (show $ chr (fromIntegral (numerator c)))
prettyWHNF (TyList TyC)    v            = prettyString v
prettyWHNF (TyList ty)     v            = prettyList ty v
prettyWHNF ty@(_ :*: _)    v            = output "(" >> prettyTuple ty v >> output ")"
prettyWHNF (ty1 :+: ty2) (VInj s v)
  = case s of
      L -> output "left"  >> prettyVP ty1 v
      R -> output "right" >> prettyVP ty2 v
prettyWHNF _ (VNum d r)
  | denominator r == 1 = output $ show (numerator r)
  | otherwise          = case d of
      Fraction -> output $ show (numerator r) ++ "/" ++ show (denominator r)
      Decimal  -> output $ prettyDecimal r

prettyWHNF ty@(_ :->: _) _ = prettyPlaceholder ty

prettyWHNF (TySet t) (VBag xs) =
  output "{" >> prettySequence t (map fst xs) ", " >> output "}"
prettyWHNF (TyBag t) (VBag xs) = prettyBag t xs

prettyWHNF (TyGraph a) (VGraph _ adj) = prettyWHNF (TyMap a (TySet a)) =<< rnfV adj
prettyWHNF (TyMap k v) (VMap m)
  | M.null m = output "emptyMap"
  | otherwise = do
      output "map("
      prettyWHNF (TySet (k :*: v)) =<< mapToSet k v (VMap m)
      output ")"

prettyWHNF ty v = error $
  "Impossible! No matching case in prettyWHNF for " ++ show v ++ ": " ++ show ty

prettyPlaceholder :: Member (Output String) r => Type -> Sem r ()
prettyPlaceholder ty = do
  output "<"
  tyStr <- renderDoc (prettyTy ty)
  output tyStr
  output ">"

-- | 'prettySequence' pretty-prints a lists of values separated by a delimiter.
prettySequence :: Members (Input TopInfo ': Output String ': EvalEffects) r => Type -> [Value] -> String -> Sem r ()
prettySequence _ []     _   = output ""
prettySequence t [x]    _   = prettyV t x
prettySequence t (x:xs) del = prettyV t x >> output del >> prettySequence t xs del

prettyBag :: Members (Input TopInfo ': Output String ': EvalEffects) r => Type -> [(Value, Integer)] -> Sem r ()
prettyBag _ []         = output "⟅⟆"
prettyBag t vs
  | all ((==1) . snd) vs   = output "⟅" >> prettySequence t (map fst vs) ", " >> output "⟆"
  | otherwise              = output "⟅" >> prettyCounts vs >> output "⟆"

  where
    prettyCounts []      = error "Impossible! prettyCounts []"
    prettyCounts [v]     = prettyCount v
    prettyCounts (v:vs') = prettyCount v >> output ", " >> prettyCounts vs'

    prettyCount (v,1) = prettyV t v
    prettyCount (v,n) = prettyV t v >> output (" # " ++ show n)

prettyString :: Members (Input TopInfo ': Output String ': EvalEffects) r => Value -> Sem r ()
prettyString str = output "\"" >> go str >> output "\""
  where
    toChar :: Value -> String
    toChar (VNum _ c) = drop 1 . reverse . drop 1 . reverse . show $ [chr (fromIntegral (numerator c))]
    toChar v' = error $ "Impossible! Value that's not a char in prettyString.toChar: " ++ show v'

    go :: Members (Output String ': EvalEffects) r => Value -> Sem r ()
    go v = do
      whnfList v (return ()) $ \hd tl -> do
        hd' <- whnfV hd
        output (toChar hd')
        go tl

-- | Pretty-print a list with elements of a given type, assuming the
--   list has already been reduced to WHNF.
prettyList :: Members (Input TopInfo ': Output String ': EvalEffects) r => Type -> Value -> Sem r ()
prettyList ty v = output "[" >> go v
  where
    go w = whnfList w (output "]") $ \hd tl -> do
      prettyV ty hd
      tlWHNF <- whnfV tl
      case tlWHNF of
        VInj R _ -> output ", "
        _        -> return ()
      go tlWHNF

prettyTuple :: Members (Input TopInfo ': Output String ': EvalEffects) r => Type -> Value -> Sem r ()
prettyTuple (ty1 :*: ty2) (VPair v1 v2) = do
  prettyV ty1 v1
  output ", "
  whnfV v2 >>= prettyTuple ty2
prettyTuple ty v = prettyV ty v
