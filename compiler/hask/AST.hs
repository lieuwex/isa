module AST where


type Name = String

data Program = Program [Decl]
  deriving (Show, Eq)

data Decl = Decl Name Expr
  deriving (Show, Eq)

data Expr = ECall Expr Expr
          | ERef Name
          | EInt Int
          | ELambda Name Expr
          | ELet [(Name, Expr)] Expr
  deriving (Show, Eq)
