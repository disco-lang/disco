------------------------------------------------------------
-- Values
------------------------------------------------------------

-- | The type of values produced by the interpreter. The parameter @r@
--   is an effect row listing the effects needed to evaluate such values.
data Value where
  -- | A numeric value, which also carries a flag saying how
  --   fractional values should be diplayed.
  VNum  :: RationalDisplay -> Rational -> Value

  -- | The unit value.
  VUnit :: Value

  -- | An injection into a sum type.
  VInj :: Side -> Value -> Value

  -- | A pair of values.
  VPair :: Value -> Value -> Value

  -- | A built-in function constant.
  VConst :: Op -> Value

  -- | A closure, i.e. a function body together with its
  --   environment.
  VClos  :: Bind [Name Core] Core -> Env -> Value

  -- | A partial application, i.e. an application of a thing to some
  --   arguments which is still waiting for more.  Invariant: the
  --   thing being applied is in WHNF.
  VPAp   :: Value -> [Value] -> Value

  -- | A thunk, i.e. an unevaluated core expression together with
  --   its environment.
  VThunk :: Core -> Env -> Value

  -- | An indirection, i.e. a pointer to an entry in the value table.
  --   This is how we get graph reduction.  When we create a thunk, we
  --   put it in a new entry in the value table, and return a VIndir.
  --   The VIndir can get copied but all the copies refer to the same
  --   thunk, which will only be evaluated once, the first time the
  --   value is demanded.
  VIndir :: Int -> Value

  -- | A literal function value.  @VFun@ is only used when
  --   enumerating function values in order to decide comparisons at
  --   higher-order function types.  For example, in order to
  --   compare two values of type @(Bool -> Bool) -> Bool@ for
  --   equality, we have to enumerate all functions of type @Bool ->
  --   Bool@ as @VFun@ values.
  --
  --   We assume that all @VFun@ values are /strict/, that is, their
  --   arguments should be fully evaluated to RNF before being
  --   passed to the function.
  VFun_   :: ValFun -> Value

  -- | A proposition.
  VProp   :: ValProp -> Value

  -- | A @Value@ computation which can be run later, along with
  --   the environment in which it should run, and a set of referenced
  --   memory locations that should not be garbage collected.
  VDelay_  :: ValDelay -> IntSet -> Env -> Value

  -- | A literal bag, containing a finite list of (perhaps only
  --   partially evaluated) values, each paired with a count.  This is
  --   also used to represent sets (with the invariant that all counts
  --   are equal to 1).
  VBag :: [(Value, Integer)] -> Value

  -- | A Graph in the algebraic repesentation. The stored value is an indirection to the graph's adjacency map representation.
  VGraph :: Graph SimpleValue -> Value -> Value

  -- | A map from keys to values. Differs from functions because we can
  --   actually construct the set of entries, while functions only have this
  --   property when the key type is finite.
  VMap :: Map SimpleValue Value -> Value

  -- | A disco type can be a value.  For now, there are only a very
  --   limited number of places this could ever show up (in
  --   particular, as an argument to @enumerate@ or @count@).
  VType :: Type -> Value
  deriving Show
