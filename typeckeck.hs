module Typecheck where
import AbsSyn


typecheck :: Class -> Class
typecheck Class(typ, fields, methods) = Class (
	typ,
	fields,
	getTypedMethods methods
)

getTypedMethods :: [MethodDecl] -> [MethodDecl]
getTypedMethods Method(typ, name, args, body) : methods = 
	Method(typ, name, args, aösduhf)
	: getTypedMethods methods
getTypedMethods [] = []

testExample = Class("Bsp", [], [
		Method("void", "main", [("String[]", args)], 
				Block([
					LocalVarDecl("int", "i"),
					Assign("i", Integer(1)),
					LocalVarDecl("boolean", "a"),
					Assign("a", Bool(True)),
					LocalVarDecl("char", "b"),
					Assign("b", Char('t')),
					
					If( LocalOrFieldVar("a"), Assign("i", Integer(1)), Assign("b", Char('f'))  ),
				])		
			)
		])
