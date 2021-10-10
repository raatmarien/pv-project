module Path where

import Gcl (Expression, Identifier, Type)

import Std hiding (Type)

data Statement =
  Assert Expression |
  Assume Expression |
  Assign Identifier Expression |
  AssignArray Identifier Expression Expression |
  Declaration Identifier Type
  deriving (Show)
