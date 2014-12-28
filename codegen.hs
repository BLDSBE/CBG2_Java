module Codegen where
import ClassFormat
import AbsSyn
import Data.Map (Map)             -- This just imports the type name
import qualified Data.Map as Map  -- Imports everything else, but with names 
                                  -- prefixed with "Map." (with the period).



getCPHashTable :: Class -> Map String Int
getCPHashTable (Class(typ, fieldDecls, methodDecls)) = 
	let methodHT = getMethodHT methodDecls 0 in
		let fieldHT = getFieldHT fieldDecls (Map.size methodHT) in 
			Map.union (Map.union methodHT fieldHT) 
				(Map.fromList [(typ, Map.size fieldHT), ("java/lang/Object", (Map.size fieldHT) + 1)])

{- Map.union (
	Map.union (getMethodHT methodDecls 0) (getFieldHT fieldDecls 0)) 
	(Map.fromList [(typ, 0), ("java/lang/Object", 1)]) -}

--getCPHashTable (Class(typ, fieldDecls, methodDecls)) = Map.fromList $ do  
--	let listMethods = (typ, 0) : ( ("java/lang/Object", 1) : (getMethodList methodDecls 2)) 
--	let i = toInteger $ length listMethods
--	let listFieldsMethods = listMethods ++ (getFieldList fieldDecls i)
--	listFieldsMethods

getMethodHT :: [MethodDecl] -> Int -> Map String Int
getMethodHT [] _ = Map.empty
getMethodHT (Method(_, m, _, _):ml) size = 
	Map.union (Map.singleton m size) (getMethodHT ml (size + 1))

getFieldHT :: [FieldDecl] -> Int -> Map String Int
getFieldHT [] _ = Map.empty
getFieldHT (FieldDecl(_, f):fl) size = 
	Map.union (Map.singleton f size) (getFieldHT fl (size + 1))


cpht_toCP_Infos :: Map String Int -> CP_Infos
cpht_toCP_Infos ht = Map.foldWithKey (\name index rest -> (Utf8_Info { tag_cp = TagUtf8, tam_cp = length name, cad_cp = name, desc = name }):rest) [] ht

get_CP_Infos :: Class -> CP_Infos
get_CP_Infos c@(Class(typ, fieldDecls, methodDecls)) = do
	let cpht = getCPHashTable c
	let strings = cpht_toCP_Infos cpht
	let i = Map.size cpht
	let cpStringsClasses = strings ++ [ Class_Info {
    	tag_cp = TagClass,
    	index_cp = cpht Map.! typ,
    	desc = typ
		}, 
		Class_Info {
			tag_cp = TagClass,
    		index_cp = cpht Map.! "java/lang/Object",
    		desc = "java/lang/Object"
		}]
	cpStringsClasses ++ [ FieldRef_Info {
		tag_cp = TagFieldRef,
		index_name_cp = cpht Map.! typ,
		index_nameandtype_cp = cpht Map.! fName,
		desc = fName
		} | FieldDecl(fTyp, fName) <- fieldDecls]


get_CP_Index :: Map String Integer -> String -> Int
get_CP_Index map quStr = fromInteger $ (\a -> case a of Just b -> b; Nothing -> -1) $ Map.lookup quStr map

get_ClassAccessFlags :: Class -> AccessFlags
get_ClassAccessFlags c = AccessFlags [1]

get_Interfaces :: Class -> Interfaces
get_Interfaces c = []

get_Field_Infos :: Class -> Field_Infos
get_Field_Infos c = []

get_Method_Infos :: Class -> Method_Infos
get_Method_Infos c = []

get_Attribute_Infos :: Class -> Attribute_Infos
get_Attribute_Infos c = []


codegen :: Class -> ClassFile
codegen c = ClassFile {
    magic = Magic,
    minver = MinorVersion 0,
    maxver = MajorVersion 52,
    count_cp = length $ get_CP_Infos c,
    array_cp = get_CP_Infos c,
    acfg = get_ClassAccessFlags c,
    this = ThisClass { index_th = -1 },
    super = SuperClass { index_sp = -1 },
    count_interfaces = length $ get_Interfaces c,
    array_interfaces = get_Interfaces c,
    count_fields = length $ get_Field_Infos c,
    array_fields = get_Field_Infos c,
    count_methods = length $ get_Method_Infos c,
    array_methods = get_Method_Infos c,
    count_attributes = length $ get_Attribute_Infos c,
    array_attributes = get_Attribute_Infos c
}



main :: IO()
main = do let example = Class("Bsp", [FieldDecl("int", "n")], [
						  Method("void", "<init>", [], Block([])
						  ),
						  Method("void", "main", [("String[]", "args")], Block([
                            LocalVarDecl("int", "i"),
                            StmtExprStmt(Assign("i", TypedExpr(Integer 0, "int"))),
                            LocalVarDecl("boolean", "a"),
                            StmtExprStmt(Assign("a", TypedExpr(Bool True, "boolean"))),
                            LocalVarDecl("char", "b"),
                            StmtExprStmt(Assign("b", TypedExpr(Char('t'), "char"))),
                            If(TypedExpr(LocalOrFieldVar("a"), "boolean"), 
                                StmtExprStmt(Assign("a", TypedExpr(Integer 5, "int"))), 
                                Just (StmtExprStmt(Assign("b", TypedExpr(Char('f'), "char"))))
                            )]))
						  ])
              --StmtExprStmt(MethodCall(    System.out, "println", [] ))
          print $ getMethodHT (getMethodDeclsFromClass example) 1
          print $ getCPHashTable example
          print $ codegen example
