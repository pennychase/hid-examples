{-# LANGUAGE NamedFieldPuns #-}

import Control.Monad.Reader

data Config = Config {
    verbose :: Bool
    {- other parameters -}
  }

type ConfigM = Reader Config

getConfiguration :: IO Config
getConfiguration = pure Config { verbose = True {- ... -} }

-- Originally all the returrn types were ConfigM (). But there was no way
-- to see if the configuration was being used since () was always returned.
-- Changed the types of all the functions to ConfigM String. Had to change
-- doSomething to use if-then-else instead of when, both to create an alternative
-- and to have the right type.
-- Finally, the code never used the silent config, so added doSomethingSilently
-- to doSomething.

main :: IO ()
main = do
  config <- getConfiguration
  let result = runReader work config
  print result

work :: ConfigM String
work = do
  -- ...
  doSomething
  -- ...

doSomething :: ConfigM String
doSomething = do
  -- ...
  special <- doSomethingSpecial
  silently <- doSomethingSpecialSilently
  pure $ special ++ " + " ++ silently
  -- ...

doSomethingSpecial :: ConfigM String
doSomethingSpecial = do
  -- ...
  -- Config {verbose} <- ask
  vrb <- asks verbose
  if vrb then beVerbose else pure "Nothing here"
  -- when vrb verbose  -- used when type was ConfigM ()
  -- ...

beVerbose :: ConfigM String
beVerbose = pure "This is verbose mode"

silent :: Config -> Config
silent config = config {verbose = False}

doSomethingSpecialSilently :: ConfigM String
doSomethingSpecialSilently = local silent doSomethingSpecial
