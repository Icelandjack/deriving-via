% Stolen Instances Taste Sweet
% Baldur Blöndal
% September, 2017

# The main idea..

Newtypes

#

```
mempty :: Monoid    m => m
(<>)   :: Semigroup s => s -> s -> s

class Functor f => Applicative f where
  pure   :: a             -> f a
  liftA2 :: (a -> b -> c) -> (f a -> f b -> f c)


<!--- pandoc -f markdown -t slidy -i -s --self-contained -o mypresentation.html Presentation.md --->
