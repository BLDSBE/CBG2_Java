module Typechek where
import AbsSyn
import Data.Map (Map)
import qualified Data.Map as Map

type Env = Map String Type
type ClassMap = Map Type (Env, Env) -- A map className => (Fields, Methods)
type LookUp = Type -> (Env,Env)		-- A function type that allows a direkt lookup in the ClassMap
												-- and prevents from manipulating it


{- TODO:
	- correct type identifiers
	- assigns for InstVars???
	- methods from Object
-}


--Constants-----------------------------------------------------------------------------------------
booleanType 	= "boolean"
charType 		= "char"
intType 			= "int"
stringType 		= "String"
nullType 		= "NullType"
objectType 		= "java/lang/Object"

thisString 		= "this"
superString 	= "super"
----------------------------------------------------------------------------------------------------



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
		((\ m t -> (m Map.! t)) cm) -- extract lookUp function
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
		(Map.insert superString objectType -- insert this and super variable into Env
			(Map.insert thisString typ
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
----------------------------------------------------------------------------------------------------



--Methods-------------------------------------------------------------------------------------------
getTypedMethods :: LookUp -> Env -> [MethodDecl] -> [MethodDecl]
getTypedMethods lookUp env (Method(typ, name, args, body) : methods) =
	let newEnv = Map.union (getEnvFromArgList args) env in -- notice that args possibly override env
		Method(typ, name, args, getTypedMethodBody (lookUp) newEnv body)
		: getTypedMethods (lookUp) env methods
getTypedMethods lookUp env [] = []

getEnvFromArgList :: [(Type,String)] -> Env
getEnvFromArgList ((typ, name) : tail) =
	Map.insert name typ (getEnvFromArgList tail)
getEnvFromArgList [] = Map.empty

getTypedMethodBody :: LookUp -> Env -> Stmt -> Stmt
getTypedMethodBody lookUp env b@(Block(_)) = let (s, newEnv) = (getTypedStatement (lookUp) env b) in s
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
															--getTypeFromExpr te) -- TODO: check for errors
															(env Map.! name)) -- lookup the type by variable name
																					-- and don't take the type from "te"
																					-- to get the correct type if rvalue
																					-- is "null"
getTypedStatementExpr lookUp env (New(typ, es)) =
											TypedStmtExpr(New(typ, getTypedExpressionList (lookUp) env es), typ)
getTypedStatementExpr lookUp env (MethodCall(e,name,es)) = 
								let te = getTypedExpression (lookUp) env e in -- type the expression 
											-- (which returns the class where the method can be found)
									let typ = (let (fields, methods) = 
													-- find the associated class
													lookUp (getTypeFromExpr te) in
														-- and lookup the type of the method
														(methods Map.! name) )		in
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
getTypedExpression lookUp env this@(This) = TypedExpr(this, (env Map.! thisString))
getTypedExpression lookUp env super@(Super) = TypedExpr(super, (env Map.! superString))
getTypedExpression lookUp env lofv@(LocalOrFieldVar(name)) = TypedExpr(lofv, (env Map.! name))
getTypedExpression lookUp env (InstVar(e, name)) = 
								let te = getTypedExpression (lookUp) env e in -- type the expression 
											-- (which returns the class where the variable is declared)
									let typ = (let (fields, methods) = 
													-- find the associated class in the lookUp function
													lookUp (getTypeFromExpr te) in
														-- and lookup the type
														(fields Map.! name) ) in
											TypedExpr(InstVar(te, name), typ) -- return that type

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
												if getTypeFromExpr te1 /= nullType then -- Only return nullType if necessary
													getTypeFromExpr te1	-- ( "null == null" )
												else if getTypeFromExpr te2 /= nullType then
													getTypeFromExpr te2
												else
													nullType)
getTypedExpression lookUp env i@(Integer(_)) = TypedExpr(i, intType)
getTypedExpression lookUp env b@(Bool(_)) = TypedExpr(b, booleanType)
getTypedExpression lookUp env c@(Char(_)) = TypedExpr(c, charType)
getTypedExpression lookUp env s@(String(_)) = TypedExpr(s, stringType)
getTypedExpression lookUp env n@(Jnull) = TypedExpr(Jnull, nullType)
getTypedExpression lookUp env (StmtExprExpr(se)) = 
									let tse = getTypedStatementExpr (lookUp) env se in
										TypedExpr(StmtExprExpr(tse), getTypeFromStmtExpr tse)
getTypedExpression lookUp env e = e -- should not happen!
----------------------------------------------------------------------------------------------------


testExample = [Class("Bsp", [FieldDecl(booleanType, "myBool")], [
		Method("void", "main", [("String[]", "args")], 
				Block([
					LocalVarDecl(intType, "i"),
					StmtExprStmt(Assign("i", Integer(1))),
					LocalVarDecl(booleanType, "a"),
					StmtExprStmt(Assign("a", Bool(True))),
					LocalVarDecl(charType, "b"),
					StmtExprStmt(Assign("b", Char('t'))),
					
					--If(InstVar(LocalOrFieldVar(thisString), "myBool"),
						--	StmtExprStmt(Assign("i", Integer(1))),
							--Just (StmtExprStmt(Assign("b", Char('f'))))  )
							
					LocalVarDecl("MyClass", "x"),
					StmtExprStmt(Assign("x", StmtExprExpr(New("MyClass",[])))),
					
					While(Bool(True),
							Block([StmtExprStmt(MethodCall(LocalOrFieldVar("x"),
								 									"setI", 
								 									[Binary("+",
								 										Unary("-",
								 												StmtExprExpr(MethodCall(
								 													LocalOrFieldVar("x"), "getI",[]))),
								 										Integer(1))]))
								 	])),

					StmtExprStmt(Assign("x", Jnull))
				])
			)
	]),
	Class("MyClass", [FieldDecl(intType, "i")], 
			[
				Method(intType, "getI", [], Block([Return(LocalOrFieldVar("i"))])),
				Method("void", "setI", [(intType, "_i")], Block([StmtExprStmt(Assign("i", LocalOrFieldVar("_i")))])) --TODO: How do assignments and instvars work?
			])
	]
	
testBlock = Block([--LocalVarDecl(charType, "c"),
					--LocalVarDecl(intType, "i"),
					--LocalVarDecl(intType, "j"),
					--StmtExprStmt(Assign("j", LocalOrFieldVar("i")))
					LocalVarDecl("i", intType),
					Return(LocalOrFieldVar("i")) ])
					
testBlock2 = Block([
					--LocalVarDecl(intType, "i"),
					StmtExprStmt(Assign("j", Integer(1))),
					--LocalVarDecl(booleanType, "a"),
					--StmtExprStmt(Assign("a", Bool(True))),
					--LocalVarDecl(charType, "b"),
					--StmtExprStmt(Assign("b", Char('t'))),
					
					
					StmtExprStmt(Assign("i", LocalOrFieldVar("i")))
					--If( Binary("&&", Bool(True), Bool(False)), 
					--		StmtExprStmt(Assign("i", Integer(1))), 
					--		Just (StmtExprStmt(Assign("b", Char('f'))))  )
				])
          
test = Method("void", "main", [("String[]", "args")], 
				Block([
					LocalVarDecl(intType, "i"),
					StmtExprStmt(Assign("i", Integer(1))),
					LocalVarDecl(booleanType, "a"),
					StmtExprStmt(Assign("a", Bool(True))),
					LocalVarDecl(charType, "b"),
					StmtExprStmt(Assign("b", Char('t'))),
					
					If( Binary("&&", Bool(True), Bool(False)),
							StmtExprStmt(Assign("i", Integer(1))), 
							Just (StmtExprStmt(Assign("b", Char('f'))))  )
				])
			)

testInstVars = [
		Class("A", [FieldDecl(intType, "i"), FieldDecl("B", "x")],
			[Method("void", "test", [], Block([
														StmtExprStmt(Assign("i", InstVar(LocalOrFieldVar("x"), "j")))]))]),
		Class("B",[FieldDecl(intType, "j")],[])
	]

main :: IO()
main = print $ typecheck testExample
	--print $ typecheck testInstVars
	--print $ getTypedStatement (\ a -> (Map.empty, Map.empty)) (Map.fromList [("j", intType)]) testBlock
	--print $ getTypedMethodBody (\ a -> (Map.empty, Map.empty)) Map.empty testBlock2
	--print $ getTypedMethods (\ a -> (Map.empty, Map.empty)) Map.empty [test]
	--print $ getTypedExpression (\ a -> (Map.empty, Map.empty)) Map.empty (Binary("==", Jnull, Integer(1)))

