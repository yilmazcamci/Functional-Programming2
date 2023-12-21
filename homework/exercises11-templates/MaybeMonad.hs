module MaybeMonad where

maybeMap :: (a -> b) -> Maybe a -> Maybe b
maybeMap f _ = error "TODO: implement me"

--stripMaybe :: Maybe (Maybe a) -> Maybe a
--stripMaybe ...

--applyMaybe:: (a -> Maybe b) -> Maybe a -> Maybe b
--applyMaybe f ... = ...
