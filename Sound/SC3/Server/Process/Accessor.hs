module Sound.SC3.Server.Process.Accessor (
    deriveAccessors
) where

import qualified Data.Accessor.Template as Accessor
import Language.Haskell.TH              (Dec, Name, Q)

-- | Derive accessors with a leading @_@.
deriveAccessors :: Name -> Q [Dec]
deriveAccessors = flip Accessor.nameDeriveAccessors (Just . ("_"++))
