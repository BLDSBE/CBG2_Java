module AbsSyn where

type Type = String

data Class = Class(Type, [FieldDecl], [MethodDecl]) deriving (Show)

getMethodDeclsFromClass (Class(_, _, m)) = m

data FieldDecl = FieldDecl(Type, String) deriving (Show)

--                       typ   name    args             code  static
data MethodDecl = Method(Type, String, [(Type,String)], Stmt, Bool) deriving (Show)

data Stmt = Block([Stmt])
          | Return( Expr )
          | While( Expr , Stmt )
          | LocalVarDecl(Type, String) 
          | If(Expr, Stmt , Maybe Stmt)
          | StmtExprStmt(StmtExpr)
          deriving (Show)

data StmtExpr = --Assign(String, Expr)
					Assign(Expr, Expr)
              | New(Type, [Expr])
              | MethodCall(Expr, String, [Expr]) 
              | TypedStmtExpr(StmtExpr, Type)
              deriving (Show)

getTypeFromStmtExpr (TypedStmtExpr(_, typ)) = typ

data Expr = This
          | Super
          | LocalOrFieldVar(String)
          | InstVar(Expr, String)
          | Unary(String, Expr)
          | Binary(String, Expr, Expr) 
          | Integer(Integer)
          | Bool(Bool)
          | Char(Char)
          | String(String)
          | Jnull
          | StmtExprExpr(StmtExpr)
          | TypedExpr(Expr, Type)
          deriving (Show)

getTypeFromExpr (TypedExpr(_, typ)) = typ
          
 
type Prg = [Class]

