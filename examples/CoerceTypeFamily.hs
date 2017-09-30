import Data.Coerce
import Data.Functor.Rep
import Data.Functor.Product
import Data.Functor.Identity
import Data.Distributive
import Data.Kind

-- The phantom `rep` argument specifies which representation to use 
newtype W :: Type -> (Type -> Type) -> (Type -> Type) where
  W :: { unW :: f a } -> W rep f a
  deriving newtype
    Functor

instance Distributive d => Distributive (W rep d) where
  distribute :: Functor f => f (W rep d a) -> W rep d (f a)
  distribute = W . distribute . fmap unW

instance (Representable f, rep `Coercible` Rep f) => Representable (W rep f) where
  -- as seen here:
  type Rep (W rep f) = rep
  -- the reason this works is because of the additional constraint (rep `Coercible` Rep f)

  index :: forall a. W rep f a -> rep -> a
  index = coerce (index @f @a)

  tabulate :: forall a. (rep -> a) -> W rep f a
  tabulate = coerce (tabulate @f @a)

-- This lets us derive `Representable` for `newtype Pair a = Pair_ (Product Identity Identity a)` 
--
-- This normally gives us `Rep Pair = Either () ()` but that is not a pretty representation.
-- 
-- (One way is to derive it from `Bool -> a`, but this means changing representation of `Rep Pair` or of `Pair`)
--
-- This shows how to derive it for a coercible type: `PairIx` the user can specify
newtype PairIx = PairIx (Either () ())

instance Show PairIx where
  show = \case
    Fst -> "fst"
    Snd -> "snd"

pattern Fst, Snd :: PairIx
pattern Fst = PairIx (Left  ())
pattern Snd = PairIx (Right ())

-- Here the user can write
--
-- newtype Pair a = Pair_ (Product Identity Identity a)
--   deriving 
--     (..., Representable)
--     via 
--       Wrap BOOL (Product Identity Identity) a
  
newtype Pair a = Pair (Product Identity Identity a)
  deriving 
    (Functor, Distributive, Representable) -- Distributive doesn't work yet
    via W PairIx Pair a
    
-- Generating . . .

instance Functor Pair where
  fmap :: (a -> a') -> (Pair a -> Pair a')
  fmap = coerce (fmap @(W PairIx Pair) @a @a')

instance Distributive Pair where
  distribute :: Functor f => f (Pair a) -> Pair (f a)
  distribute = coerce (distribute @(W PairIx Pair) @f @a)

instance Representable Pair where
  type Rep Pair = PairIx

  index :: forall a. Pair a -> PairIx -> a
  index = coerce (index @(W PairIx Pair) @a)

  tabulate :: forall a. (PairIx -> a) -> Pair a
  tabulate = coerce (tabulate @(W PairIx Pair) @a)
