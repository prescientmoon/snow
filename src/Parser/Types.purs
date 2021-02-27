module Snow.Parser.Types where

type SourcePos = { x :: Int, y :: Int }
type SourceRange = { from :: SourcePos, to :: SourcePos }

{-
data ImportPath 
    = PathMany (Array ImportPath)
    | PathSingle String ImportPath
    | ValueImport String

type ScopeHeader = Array ImportPath

data ModuleDeclaration
    = ValueDeclaration Cst
    | Module String Module

type Module
    = { name :: String
      , header :: ScopeHeader 
      , declarations :: Array ModuleDeclaration }
      -}