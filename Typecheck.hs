module Typecheck where
import AbsSyn
import Constants
import Data.Map (Map)
import qualified Data.Map as Map
import qualified Text.Show.Pretty as Pr

type Env = Map String Type
type ClassMap = Map Type (Env, Env) -- A map className => (Fields, Methods)
type LookUp = Type -> (Env,Env)		-- A function type that allows a direkt lookup in the ClassMap
												-- and prevents from manipulating it


{- TODO:
	- methods from Object
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
		(lookUpClass cm) -- extract lookUp function
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
getEnvFromMethods (Method(typ, name, _, _, _) : tail) =
	Map.insert name typ (getEnvFromMethods tail)
getEnvFromMethods [] = Map.empty

getEnvFromFields :: [FieldDecl] -> Env
getEnvFromFields (FieldDecl(typ, name) : tail) = 
	Map.insert name typ (getEnvFromFields tail)
getEnvFromFields [] = Map.empty
----------------------------------------------------------------------------------------------------



--Methods-------------------------------------------------------------------------------------------
getTypedMethods :: LookUp -> Env -> [MethodDecl] -> [MethodDecl]
getTypedMethods lookUp env (Method(typ, name, args, body, static) : methods) =
	let newEnv = Map.union (getEnvFromArgList args) env in -- notice that args possibly override env
		Method(typ, name, args, getTypedMethodBody (lookUp) newEnv typ body, static)
		: getTypedMethods (lookUp) env methods
getTypedMethods lookUp env [] = []

getEnvFromArgList :: [(Type,String)] -> Env
getEnvFromArgList ((typ, name) : tail) =
	Map.insert name typ (getEnvFromArgList tail)
getEnvFromArgList [] = Map.empty

getTypedMethodBody :: LookUp -> Env -> Type -> Stmt -> Stmt
getTypedMethodBody lookUp env typ b@(Block(_)) = 
								let (s, newEnv) = (getTypedStatement (lookUp) env typ b) in s
getTypedMethodBody lookUp env typ s = s -- This should never happen!
								 	  			-- (Java doesn't allow other statements as method body)
----------------------------------------------------------------------------------------------------



--Statements----------------------------------------------------------------------------------------
-- LookUp contains all classes with their fields and methods
-- Env contains all local variables
-- Type is the return type of the surrounding method
getTypedStatementList :: LookUp -> Env -> Type -> [Stmt] -> [Stmt]
getTypedStatementList lookUp env typ (s : tail) = let 
															(newS, newEnv) = getTypedStatement (lookUp) env typ s in 
																newS : (getTypedStatementList
																		(lookUp) (Map.union newEnv env) typ tail)
getTypedStatementList lookUp typ env [] = []

-- LookUp contains all classes with their fields and methods
-- Env contains all local variables
-- Type is the return type of the surrounding method
getTypedStatement :: LookUp -> Env -> Type -> Stmt -> (Stmt, Env)
getTypedStatement lookUp env typ (Block(sl)) = (Block(getTypedStatementList (lookUp) env typ sl), env)
getTypedStatement lookUp env typ (Return(e)) = let te = getTypedExpression (lookUp) env e in
																if typ == getTypeFromExpr te then (Return(te), env)
																else if typ == voidType then error notVoidReturnError
																else error (typeMissmatchError (getTypeFromExpr te) typ)
getTypedStatement lookUp env typ r@(ReturnV) = if typ == voidType
															  then (r, env)
															  else error (voidReturnError typ)
getTypedStatement lookUp env typ (While(e, s)) = (While(getTypedExpression (lookUp) env e, 
																let (newS, newEnv) = 
																		getTypedStatement 
																			(lookUp) env typ s in newS), env)
getTypedStatement lookUp env typ lvd@(LocalVarDecl(varType, name)) = 
															(lvd, Map.insert name varType env) --possibly override
getTypedStatement lookUp env typ (If(e, ifS, Just elseS)) = (If(getTypedExpression (lookUp) env e,
																	let (newIfS, newIfEnv) = 
																			getTypedStatement (lookUp) env typ ifS 
																		in newIfS,
																	Just (
																		let (newElseS, newElseEnv) = 
																			getTypedStatement (lookUp) env typ elseS in 
																				newElseS)
																 ), env)
getTypedStatement lookUp env typ (If(e, ifS, Nothing)) = (If(getTypedExpression (lookUp) env e, 
																let (newIfS, newIfEnv) = 
																	getTypedStatement (lookUp) env typ ifS in newIfS, 
																Nothing), env)
getTypedStatement lookUp env typ (StmtExprStmt(se)) = 
												(StmtExprStmt(getTypedStatementExpr (lookUp) env se), env)
--getTypedStatement lookUp env s = (s, env) --error ("Unknown statement: " ++ s)
----------------------------------------------------------------------------------------------------



--StatementExpressions------------------------------------------------------------------------------
-- classes, vars, StmtExpr
getTypedStatementExpr :: LookUp -> Env -> StmtExpr -> StmtExpr
getTypedStatementExpr lookUp env (Assign(var, e)) = let te = getTypedExpression (lookUp) env e in
																	 let tvar = getTypedExpression (lookUp) env var in
																	 let typeExpr = getTypeFromExpr te in
																	 let typeVar = getTypeFromExpr tvar in
																		if isSubType typeExpr typeVar 
																		then TypedStmtExpr(Assign(tvar, te),
																										getTypeFromExpr tvar)
																		else error (typeMissmatchError typeExpr typeVar)
getTypedStatementExpr lookUp env (New(typ, es)) =
											TypedStmtExpr(New(typ, getTypedExpressionList (lookUp) env es), typ)
getTypedStatementExpr lookUp env (MethodCall(e,name,es)) = 
								let te = getTypedExpression (lookUp) env e in -- type the expression 
											-- (which returns the class where the method can be found)
									let typ = (let (fields, methods) = 
													-- find the associated class
													lookUp (getTypeFromExpr te) in
														-- and lookup the type of the method
														lookUpMethod methods name )		in
											TypedStmtExpr(
												MethodCall(te, name, getTypedExpressionList (lookUp) env es),
												typ) -- return that type

getTypedStatementExpr lookUp env se = se --error ("Unknown ExprStmt: " ++ se)
----------------------------------------------------------------------------------------------------



--Expressions---------------------------------------------------------------------------------------
-- classes, vars, list of expressions
getTypedExpressionList :: LookUp -> Env -> [Expr] -> [Expr]
getTypedExpressionList lookUp env (e : tail) =
		getTypedExpression (lookUp) env e : getTypedExpressionList (lookUp) env tail
getTypedExpressionList lookUp env [] = []

-- classes, vars, expression
getTypedExpression :: LookUp -> Env -> Expr -> Expr
getTypedExpression lookUp env this@(This) = TypedExpr(this, lookUpVariable env thisString)
getTypedExpression lookUp env super@(Super) = TypedExpr(super, lookUpVariable env superString)
getTypedExpression lookUp env lofv@(LocalOrFieldVar(name)) = TypedExpr(lofv, lookUpVariable env name)
getTypedExpression lookUp env (InstVar(e, name)) = 
								let te = getTypedExpression (lookUp) env e in -- type the expression 
											-- (which returns the class where the variable is declared)
									let typ = (let (fields, methods) = 
													-- find the associated class in the lookUp function
													lookUp (getTypeFromExpr te) in
														-- and lookup the type
														lookUpVariable fields name ) in
											TypedExpr(InstVar(te, name), typ) -- return that type

getTypedExpression lookUp env (Unary(op, e)) = let te = getTypedExpression (lookUp) env e in
																let typ = getTypeFromExpr te in
																	if (isNumberOperator op && isNumberType typ) 
																		|| (isBooleanOperator op && typ == booleanType) 
																		|| (op == bitwiseNot && isIntegerType typ)
																	then TypedExpr(Unary(op, te), typ)
																	else error (unaryOperatorError op typ)

getTypedExpression lookUp env (Binary(name, e1, e2)) =
									let te1 = getTypedExpression (lookUp) env e1 in
										let te2 = getTypedExpression (lookUp) env e2 in
											let t1 = getTypeFromExpr te1 in
												let t2 = getTypeFromExpr te2 in
													TypedExpr(Binary(name, te1, te2), getTypeByOperator name t1 t2)
												
getTypedExpression lookUp env i@(Integer(_)) = TypedExpr(i, intType)
getTypedExpression lookUp env b@(Bool(_)) = TypedExpr(b, booleanType)
getTypedExpression lookUp env c@(Char(_)) = TypedExpr(c, charType)
getTypedExpression lookUp env s@(String(_)) = TypedExpr(s, stringType)
getTypedExpression lookUp env n@(Jnull) = TypedExpr(n, nullType)
getTypedExpression lookUp env ci@(ClassId(typ)) = TypedExpr(ci, typ)
getTypedExpression lookUp env (StmtExprExpr(se)) = 
									let tse = getTypedStatementExpr (lookUp) env se in
										TypedExpr(StmtExprExpr(tse), getTypeFromStmtExpr tse)
getTypedExpression lookUp env e = e --error ("Unknown Expression: " ++ e)

-- <<,>>,>>>: lvalue
-- *,/: "bigger" type
-- %: integer
-- <,>,<=,>=,instanceof,==,!=: boolean
-- &,|,^: lvalue, rvalue (either integer type or boolean)
-- &&, ||: boolean

-- operator, left type, right type
getTypeByOperator :: String -> Type -> Type -> Type
getTypeByOperator op t1 t2 = 
	if 	  op == shiftL 			then checkShiftTypes op t1 t2
	else if op == shiftR2 			then checkShiftTypes op t1 t2
	else if op == shiftR3 			then checkShiftTypes op t1 t2
	
	else if op == multiplication 	then checkArithmeticTypes op t1 t2
	else if op == division 			then checkArithmeticTypes op t1 t2
	else if op == plus				then checkArithmeticTypes op t1 t2
	else if op == minus				then checkArithmeticTypes op t1 t2
	
	else if op == modulo 			then if isIntegerType t1 && isIntegerType t2 then intType
												  else error (operatorError op t1 t2)
	
	else if op == lessThan 			then checkNumberTypes op t1 t2
	else if op == greaterThan 		then checkNumberTypes op t1 t2
	else if op == lessOrEqual 		then checkNumberTypes op t1 t2
	else if op == greaterOrEqual 	then checkNumberTypes op t1 t2
	
	else if op == equals 			then checkEqualsOp op t1 t2
	else if op == unequal 			then checkEqualsOp op t1 t2
	
	else if op == instanceOf 		then if not (isPrimitiveType t1 || isPrimitiveType t2)
												  then booleanType
												  else error (operatorError op t1 t2)

	else if op == and1 				then matchTypes op t1 t2
	else if op == or1 				then matchTypes op t1 t2
	else if op == xor 				then matchTypes op t1 t2

	else if op == and2 				then checkBooleanOp op t1 t2
	else if op == or2 				then checkBooleanOp op t1 t2
	else error ("Unknown operator: " ++ op)

greaterType :: Type -> Type -> Type
greaterType t1 t2 = intType

matchTypes :: String -> Type -> Type -> Type
matchTypes op t1 t2 = if t1 == booleanType && t2 == booleanType then t1
							 else if isIntegerType t1 && isIntegerType t2 then intType
						 	 else error (operatorError op t1 t2)
						 
isSubType :: Type -> Type -> Bool
-- Returns true iff t1 is castable to t2
isSubType t1 t2 = if 		t2 == intType 		then t1 == intType || t1 == charType
						else if	t2 == charType		then t1 == charType
						else if	t2 == stringType	then t1 == stringType || t1 == nullType
						else if	t2 == booleanType	then t1 == booleanType
						else if 	t2 == objectType	then t1 /= intType && t1 /= charType && t1 /= booleanType
						else 									  t2 == t1

isNumberType :: Type -> Bool
isNumberType typ = typ == charType || typ == intType

isNumberOperator :: String -> Bool
isNumberOperator op = op == plus || op == minus || op == plusPlus || op == minusMinus
											|| op == multiplication || op == division || op == modulo

isBooleanOperator :: String -> Bool
isBooleanOperator op = op == booleanNot || op == instanceOf || op == equals || op == unequal
									|| op == and1 || op == and2 || op == or1 || op == or2 || op == xor

checkShiftTypes :: String -> Type -> Type -> Type
checkShiftTypes op t1 t2 = if isIntegerType t1 && isIntegerType t2 then t1
								 	else error (operatorError op t1 t2)

checkArithmeticTypes :: String -> Type -> Type -> Type
checkArithmeticTypes op t1 t2 = if isNumberType t1 && isNumberType t2 then greaterType t1 t2
										  else error (operatorError op t1 t2)

checkNumberTypes :: String -> Type -> Type -> Type
checkNumberTypes op t1 t2 = if isNumberType t1 && isNumberType t2 then booleanType
									 else error (operatorError op t1 t2)

checkEqualsOp :: String -> Type -> Type -> Type
checkEqualsOp op t1 t2 = if (isPrimitiveType t1 && isPrimitiveType t2
										&& (isSubType t1 t2 || isSubType t2 t1))
									|| not (isPrimitiveType t1 || isPrimitiveType t2)
								 then booleanType
								 else error (operatorError op t1 t2)

checkBooleanOp :: String -> Type -> Type -> Type
checkBooleanOp op t1 t2 = if t1 == booleanType && t2 == booleanType then t1
								  else error (operatorError op t1 t2)

isIntegerType :: Type -> Bool
isIntegerType typ = typ == intType || typ == charType

isPrimitiveType :: Type -> Bool
isPrimitiveType t = t == booleanType || isNumberType t
----------------------------------------------------------------------------------------------------



--LookUps-------------------------------------------------------------------------------------------
lookUpVariable :: Env -> String -> Type
lookUpVariable env var = case Map.lookup var env of
									Just(typ) -> typ
									Nothing	 -> error (var ++ " cannot be resolved to a variable")
									
lookUpClass :: ClassMap -> LookUp
lookUpClass cm typ = case Map.lookup typ cm of
								Just(c)	-> c
								Nothing	-> error (typ ++ " cannot be resolved to a type")
								
lookUpMethod :: Env -> String -> Type
lookUpMethod env method = case Map.lookup method env of
									Just(typ) -> typ
									Nothing	 -> error (method ++ " cannot be resolved to a method")

----------------------------------------------------------------------------------------------------



--Errors--------------------------------------------------------------------------------------------
operatorError :: String -> Type -> Type -> String
operatorError op t1 t2 = "Operator \"" ++ op ++ "\" undefined for " ++ t1 ++ " and " ++ t2

unaryOperatorError :: String -> Type -> String
unaryOperatorError op typ = "The operator " ++ op ++ " is undefined for the argument type " ++ typ

typeMissmatchError :: Type -> Type -> String
typeMissmatchError typ1 typ2 = "Type mismatch: cannot convert from " ++ typ1 ++ " to " ++ typ2
					
voidReturnError :: Type -> String
voidReturnError typ = "This method must return a result of type " ++ typ

notVoidReturnError :: String
notVoidReturnError = "Void methods cannot return a value"
----------------------------------------------------------------------------------------------------



testInstVarsAndAssign = [
		Class("A", [FieldDecl(intType, "i"), FieldDecl("B", "x")],
			[Method(voidType, "test", [], Block([
														StmtExprStmt(Assign(InstVar(LocalOrFieldVar("this"), "i"), 
																					InstVar(LocalOrFieldVar("x"), "j")))]), False)]),
		Class("B",[FieldDecl(intType, "j")],[])
	]

testOperators = [
	Class("Operators", [
			FieldDecl(intType, "i"),
			FieldDecl(charType, "c"),
			FieldDecl(booleanType, "b")
		],
		[Method(voidType, "ops",[],Block([
				StmtExprStmt(Assign(LocalOrFieldVar("i"), Binary(shiftL, Integer(1), Integer(2)))),
				StmtExprStmt(Assign(LocalOrFieldVar("i"), Binary(shiftR2, Integer(1), Integer(2)))),
				StmtExprStmt(Assign(LocalOrFieldVar("i"), Binary(shiftR3, Integer(1), Integer(2)))),
			
				StmtExprStmt(Assign(LocalOrFieldVar("i"), Binary(multiplication, Integer(1), Integer(2)))),
				StmtExprStmt(Assign(LocalOrFieldVar("i"), Binary(division, Integer(1), Integer(2)))),
				StmtExprStmt(Assign(LocalOrFieldVar("i"), Binary(modulo, Integer(1), Integer(2)))),
				
				StmtExprStmt(Assign(LocalOrFieldVar("b"), Binary(lessThan, Integer(1), Integer(2)))),
				StmtExprStmt(Assign(LocalOrFieldVar("b"), Binary(lessOrEqual, Integer(1), Integer(2)))),
				StmtExprStmt(Assign(LocalOrFieldVar("b"), Binary(greaterOrEqual, Integer(1), Integer(2)))),
				StmtExprStmt(Assign(LocalOrFieldVar("b"), Binary(greaterThan, Integer(1), Integer(2)))),
				StmtExprStmt(Assign(LocalOrFieldVar("b"), Binary(equals, Integer(1), Integer(2)))),
				StmtExprStmt(Assign(LocalOrFieldVar("b"), Binary(unequal, Integer(1), Integer(2)))),
				
				StmtExprStmt(Assign(LocalOrFieldVar("i"), Binary(and1, Integer(1), Integer(2)))),
				StmtExprStmt(Assign(LocalOrFieldVar("b"), Binary(and1, Bool(True), Bool(False)))),
				StmtExprStmt(Assign(LocalOrFieldVar("i"), Binary(or1, Integer(1), Integer(2)))),
				StmtExprStmt(Assign(LocalOrFieldVar("b"), Binary(or1, Bool(True), Bool(False)))),
				StmtExprStmt(Assign(LocalOrFieldVar("i"), Binary(xor, Integer(1), Integer(2)))),
				StmtExprStmt(Assign(LocalOrFieldVar("b"), Binary(xor, Bool(True), Bool(False)))),
				StmtExprStmt(Assign(LocalOrFieldVar("b"), Binary(and2, Bool(True), Bool(False)))),
				StmtExprStmt(Assign(LocalOrFieldVar("b"), Binary(or2, Bool(True), Bool(False))))
			]), False)])
	]

testStatic = [
		Class("A", [], [ Method(voidType, "staticMethod", [], Block([]), True) ]),
		Class("B", [], [ Method(voidType, "callStaticMethod", [], Block([
								StmtExprStmt(MethodCall(ClassId("A"), "staticMethod", [])) ]), True) ])
	]

testError = Block([
						LocalVarDecl(objectType, "x"),
						StmtExprStmt(Assign(LocalOrFieldVar("x"), ClassId("B") ))])

testVoidError = [Class("CompareTest",
                    [FieldDecl("IfTest", "a")],
                    [Method(voidType, "compareMethod",
                        [],
                        Block([
                            StmtExprStmt(Assign(LocalOrFieldVar("a"), 
                                StmtExprExpr(New("IfTest", [Integer(3), Integer(2)])))),
                            Return(LocalOrFieldVar("a"))
                        ]), True)]
                    )]

main :: IO()
main = --print $ typecheck testExample
	putStrLn $ Pr.ppShow $ typecheck testInstVarsAndAssign
	--print $ getTypedStatement (\ a -> (Map.empty, Map.empty)) (Map.fromList [("j", intType)]) testBlock
	--print $ getTypedMethodBody (\ a -> (Map.empty, Map.empty)) Map.empty testBlock2
	--print $ getTypedMethods (\ a -> (Map.empty, Map.empty)) Map.empty [test]
	--print $ getTypedExpression (\ a -> (Map.empty, Map.empty)) Map.empty (Binary("==", Jnull, Integer(1)))

