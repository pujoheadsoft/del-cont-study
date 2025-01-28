module Main (main) where

import DelimitedContinuation.BuiltinCC as BuiltinCC
import DelimitedContinuation.Example as Example
import DelimitedContinuation.CCDelcont.Example as CCDelcontExample
import DelimitedContinuation.CCDelcont.TreeIterator as CCDelcontTreeExample
import DelimitedContinuation.CCDelcont.BreadthFirstTraversal as CCDelcontBreadthFirstTraversal
import DelimitedContinuation.CCDelcont.ResumableParsing as CCDelcontResumableParsing

main :: IO ()
main = do
  BuiltinCC.program
  Example.program
  CCDelcontExample.program
  CCDelcontTreeExample.program
  CCDelcontBreadthFirstTraversal.program
  CCDelcontResumableParsing.program
