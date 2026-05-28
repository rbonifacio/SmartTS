-- | Evaluation of SmartTS contracts.
--
-- This module re-exports the full public API of the interpreter sub-modules:
--
--   * "SmartTS.Interpreter.Runtime"  — core types and address utilities
--   * "SmartTS.Interpreter.Codec"    — JSON \<-\> Expr conversion
--   * "SmartTS.Interpreter.Eval"     — expression evaluation
--   * "SmartTS.Interpreter.Contract" — statement execution and contract origination/invocation
module SmartTS.Interpreter
  ( module SmartTS.Interpreter.Runtime
  , module SmartTS.Interpreter.Codec
  , module SmartTS.Interpreter.Eval
  , module SmartTS.Interpreter.Contract
  ) where

import SmartTS.Interpreter.Codec
import SmartTS.Interpreter.Contract
import SmartTS.Interpreter.Eval
import SmartTS.Interpreter.Runtime
