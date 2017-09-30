-- This lets us derive `Representable` for `newtype Pair a = Pair_ (Product Identity Identity a)` 
--
-- This normally gives us `Rep Pair = Either () ()` but that is not a pretty representation.
-- 
-- One way is to derive it from `Bool -> a`
--
-- This shows how to derive it for a coercible type: BOOL the user can specify

class R f where                                                                                                                                                                 
  type Re f                                                                                                                                                                     
                                                                                                                                                                                
  (!) :: f a -> Re f -> a                                                                                                                                                       
  pos :: f (Re f)                                                                                                                                                               

-- Here the user can write
--
-- newtype Pair a = Pair_ (Product Identity Identity a)
--   deriving 
--     (..., Representable)
--     via 
--       Wrap BOOL (Product Identity Identity) a
                                              
newtype BOOL = BOOL (Either () ())                                                                                                                                              
                                                                                                                                                                                
instance R (Product Identity Identity) where                                                                                                                                    
  type Re (Product Identity Identity) = Either () ()                                                                                                                            
                                                                                                                                                                                
  (!) :: Product Identity Identity a -> Either () () -> a                                                                                                                       
  (!) (Pair (Identity a) Identity{})   (Left  ()) = a                                                                                                                           
  (!) (Pair Identity{}   (Identity b)) (Right ()) = b                                                                                                                           
                                                                                                                                                                                
  pos :: Product Identity Identity (Either () ())                                                                                                                               
  pos = Pair (Identity (Left ())) (Identity (Right ()))                                                                                                                         
                                                                                                                                                                                
newtype Wrap (e :: Type) f a = Wr (f a)                                                                                                                                         
                                                                                                                                                                                
instance (R f, e `Coercible` Re f) => R (Wrap e f) where                                                                                                                        
  type Re (Wrap e f) = e                                                                                                                                                        
                                                                                                                                                                                
  (!) :: Wrap e f a -> e -> a                                                                                                                                                   
  (!) (Wr fa) e = fa ! coerce e                                                                                                                                                 
                                                                                                                                                                                
  pos :: Wrap e f e                                                                                                                                                             
  pos = Wr (unsafeCoerce foo) where                                                                                                                                             
                                                                                                                                                                                
    foo :: f (Re f)                                                                                                                                                             
    foo = Main.pos                                                                                                                                                              
