-- test/SpecHook.hs
module SpecHook (module SpecHook) where

import Test.Hspec

hook :: Spec -> Spec
hook = parallel
