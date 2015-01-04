module Typechek where
import AbsSyn
import Data.Map (Map)
import qualified Data.Map as Map

type Env = Map String Type
type ClassMap = Map Type (Env, Env) -- A map className => (Fields, Methods)
type LookUp = Type -> (Env,Env)		-- A function type that allows a direkt lookup in the ClassMap
												-- and prevents from manipulating it

{- TODO:
	- instance vars
-}


--Program-------------------------------------------------------------------------------------------
typecheck :: Prg -> Prg
typecheck p = getTypedClassList (getClassMap p) p

getClassMap :: Prg -> ClassMap
getClassMap (Class(typ, fields, methods) : tail) = 
	Map.insert typ (getEnvFromFields fields, getEnvFromMethods methods) (getClassMap tail)
getClassMap [] = Map.empty

getTypedClassList :: ClassMap -> Prg -> Prg
getTypedClassList cm (c : tail) = 
	getTypedClass
		(  (\m t -> 
				(\a -> case a of Just b -> b; Nothing -> (Map.empty, Map.empty) ) (Map.lookup t m))
		    cm)
		c 
	: (getTypedClassList cm tail)
getTypedClassList cm [] = []
----------------------------------------------------------------------------------------------------



--Classes-------------------------------------------------------------------------------------------
getTypedClass :: LookUp -> Class -> Class
getTypedClass lookUp (Class(typ, fields, methods)) = Class (
	typ,
	fields,
	getTypedMethods 
		(lookUp) 
		(Map.insert "super" "Object" -- insert this and super variable into Env
			(Map.insert "this" typ
				(let (fields, methods) = lookUp typ in fields))) 
		methods     )

getEnvFromMethods :: [MethodDecl] -> Env
getEnvFromMethods (Method(typ, name, _, _) : tail) =
	Map.insert name typ (getEnvFromMethods tail)
getEnvFromMethods [] = Map.empty

getEnvFromFields :: [FieldDecl] -> Env
getEnvFromFields (FieldDecl(typ, name) : tail) = 
	Map.insert name typ (getEnvFromFields tail)
getEnvFromFields [] = Map.empty

getEnvFromArgList :: [(Type,String)] -> Env
getEnvFromArgList ((typ, name) : tail) =
	Map.insert name typ (getEnvFromArgList tail)
getEnvFromArgList [] = Map.empty
----------------------------------------------------------------------------------------------------



--Methods-------------------------------------------------------------------------------------------
getTypedMethods :: LookUp -> Env -> [MethodDecl] -> [MethodDecl]
getTypedMethods lookUp env (Method(typ, name, args, body) : methods) = 
	let newEnv = Map.union (getEnvFromArgList args) env in -- notice that args possibly override env
		Method(typ, name, args, getTypedMethodBody (lookUp) newEnv body)
		: getTypedMethods (lookUp) env methods
getTypedMethods lookUp env [] = []

getTypedMethodBody :: LookUp -> Env -> Stmt -> Stmt
getTypedMethodBody lookUp env b@(Block(_)) = let (s, env) = getTypedStatement (lookUp) env b in s
getTypedMethodBody lookUp env s = s -- This should never happen! 
								 	  			-- (Java doesn't allow other statements as method body)
----------------------------------------------------------------------------------------------------



--Statements----------------------------------------------------------------------------------------
getTypedStatementList :: LookUp -> Env -> [Stmt] -> [Stmt]
getTypedStatementList lookUp env (s : tail) = let 
																(newS, newEnv) = getTypedStatement (lookUp) env s in 
														newS : (getTypedStatementList 
																		(lookUp) (Map.union newEnv env) tail)
getTypedStatementList lookUp env [] = []


getTypedStatement :: LookUp -> Env -> Stmt -> (Stmt, Env)
getTypedStatement lookUp env (Block(sl)) = (Block(getTypedStatementList (lookUp) env sl), env)
getTypedStatement lookUp env (Return(e)) = (Return(getTypedExpression (lookUp) env e), env)
getTypedStatement lookUp env (While(e, s)) = (While(getTypedExpression (lookUp) env e, 
																let (newS, newEnv) = 
																		getTypedStatement (lookUp) env s in newS), env)
getTypedStatement lookUp env lvd@(LocalVarDecl(typ, name)) = 
															(lvd, Map.insert name typ env) --possibly override
getTypedStatement lookUp env (If(e, ifS, Just elseS)) = (If(getTypedExpression (lookUp) env e,
																	let (newIfS, newIfEnv) = 
																			getTypedStatement (lookUp) env ifS 
																		in newIfS,
																	Just (
																		let (newElseS, newElseEnv) = 
																				getTypedStatement (lookUp) env elseS in 
																			newElseS)
																 ), env)
getTypedStatement lookUp env (If(e, ifS, Nothing)) = (If(getTypedExpression (lookUp) env e, 
																let (newIfS, newIfEnv) = 
																		getTypedStatement (lookUp) env ifS in newIfS, 
																Nothing), env)
getTypedStatement lookUp env (StmtExprStmt(se)) = 
												(StmtExprStmt(getTypedStatementExpr (lookUp) env se), env)
----------------------------------------------------------------------------------------------------



--StatementExpressions------------------------------------------------------------------------------
getTypedStatementExpr :: LookUp -> Env -> StmtExpr -> StmtExpr
getTypedStatementExpr lookUp env (Assign(name, e)) = let te = getTypedExpression (lookUp) env e in
														TypedStmtExpr(
															Assign(name, te),
															getTypeFromExpr te) -- TODO: check for errors
getTypedStatementExpr lookUp env (New(typ, es)) =
											TypedStmtExpr(New(typ, getTypedExpressionList (lookUp) env es), typ)
getTypedStatementExpr lookUp env (MethodCall(e,name,es)) = 
								let te = getTypedExpression (lookUp) env e in -- type the expression 
											-- (which returns the class where the method can be found)
									let typ = (let (fields, methods) = 
													-- find the associated class
													lookUp (getTypeFromExpr te) in
														(\a -> case a of -- "de-Maybe-izing"
																Just b -> b; 
																Nothing -> "unknownMethod")
														-- and lookup the type of the method
														(Map.lookup name methods) ) 
																					in
											TypedStmtExpr(
												MethodCall(te, name, getTypedExpressionList (lookUp) env es),
												typ) -- return that type

getTypedStatementExpr lookUp env se = se -- Should also never happen!
----------------------------------------------------------------------------------------------------



--Expressions---------------------------------------------------------------------------------------
getTypedExpressionList :: LookUp -> Env -> [Expr] -> [Expr]
getTypedExpressionList lookUp env (e : tail) =
		getTypedExpression (lookUp) env e : getTypedExpressionList (lookUp) env tail
getTypedExpressionList lookUp env [] = []

getTypedExpression :: LookUp -> Env -> Expr -> Expr
getTypedExpression lookUp env this@(This) = TypedExpr(this,
							(\a -> case a of Just b -> b; Nothing -> "Error:_This_unknown") (Map.lookup "this" env))
getTypedExpression lookUp env super@(Super) = TypedExpr(super,
							(\a -> case a of Just b -> b; Nothing -> "Error:_Super_unknown") (Map.lookup "super" env))
getTypedExpression lookUp env lofv@(LocalOrFieldVar(name)) = TypedExpr(lofv, 
										-- TODO maybe throw errors when a variable has not been declared
							(\a -> case a of Just b -> b; Nothing -> "unknownField") (Map.lookup name env))
--getTypedExpression lookUp env (InstVar(e, name)) = 
getTypedExpression lookUp env (Unary(name, e)) = let te = getTypedExpression (lookUp) env e in
													TypedExpr(
														Unary(name, te),
														getTypeFromExpr te)
getTypedExpression lookUp env (Binary(name, e1, e2)) =
							--TODO: Maybe check and compare types and throw errors if necessary
									let te1 = getTypedExpression (lookUp) env e1 in
										let te2 = getTypedExpression (lookUp) env e2 in
											TypedExpr(
												Binary(name, te1, te2),
												getTypeFromExpr te1)
getTypedExpression lookUp env i@(Integer(_)) = TypedExpr(i, "int")
getTypedExpression lookUp env b@(Bool(_)) = TypedExpr(b, "boolean")
getTypedExpression lookUp env c@(Char(_)) = TypedExpr(c, "char")
getTypedExpression lookUp env s@(String(_)) = TypedExpr(s, "String")
getTypedExpression lookUp env n@(Jnull) = TypedExpr(Jnull, "NullType") --TODO: What is the type of "null"???
getTypedExpression lookUp env (StmtExprExpr(se)) = 
									let tse = getTypedStatementExpr (lookUp) env se in
										TypedExpr(StmtExprExpr(tse), getTypeFromStmtExpr tse)
getTypedExpression lookUp env e = e -- should not happen!
----------------------------------------------------------------------------------------------------


testExample = Class("Bsp", [], [
		Method("void", "main", [("String[]", "args")], 
				Block([
					LocalVarDecl("int", "i"),
					StmtExprStmt(Assign("i", Integer(1))),
					LocalVarDecl("boolean", "a"),
					StmtExprStmt(Assign("a", Bool(True))),
					LocalVarDecl("char", "b"),
					StmtExprStmt(Assign("b", Char('t'))),
					
					If( LocalOrFieldVar("a"), 
							StmtExprStmt(Assign("i", Integer(1))), 
							Just (StmtExprStmt(Assign("b", Char('f'))))  )
				])
			)
	])
