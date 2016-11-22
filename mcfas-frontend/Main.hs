module Main where

import Data.Annotation ( bareH )
import Data.HFunctor ( hcata, unK )
import Language.Oatlab.Example ( example )
import Language.Oatlab.Pretty ( ppAlg, renderOatlab )

main :: IO ()
main = putStrLn . renderOatlab . unK . hcata (ppAlg . bareH) $ example
