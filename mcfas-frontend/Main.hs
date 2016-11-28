module Main where

import Data.Annotation ( bareH )
import Data.HFunctor ( hcata, unK )
import Language.Oatlab.Pretty ( ppAlg, renderOatlab )

main :: IO ()
main = putStrLn "Hello world"
  -- putStrLn . renderOatlab . unK . hcata (ppAlg . bareH) $ example
