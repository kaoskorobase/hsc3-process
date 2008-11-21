module Sound.SC3.Server.Process.Accessor (
    deriveAccessors
) where

import qualified Data.Accessor.Template as Accessor
-- import Language.Haskell.TH              (Dec, Name, Q)

-- deriveAccessors :: Name -> Q [Dec]
deriveAccessors = flip Accessor.nameDeriveAccessors (Just . ("_"++))
