module Helper (definitely, definitelyString) where

definitely :: Maybe a -> a -> a
definitely (Just a) _ = a
definitely Nothing def = def

definitelyString :: Maybe String -> String
definitelyString string = definitely string "" 