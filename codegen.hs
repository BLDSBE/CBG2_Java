module Codegen where
import ClassFormat
import AbsSyn
import Data.List.Split
import Data.List
import qualified Text.Show.Pretty as Pr
import Data.Map (Map)             -- This just imports the type name
import qualified Data.Map as Map  -- Imports everything else, but with names 
                                  -- prefixed with "Map." (with the period).


namesAndTypes :: Class -> Int -> Map String Int
namesAndTypes c@(Class(_, [], [])) _ = Map.empty

namesAndTypes c@(Class(typ, [], (Method(t, n, args, _)):methodDecls)) i = 
	Map.insert (n ++ ":" ++ (getMethodType t args)) i (namesAndTypes (Class(typ, [], methodDecls)) (i + 1))

namesAndTypes c@(Class(typ, FieldDecl(t, n):fieldDecls, m)) i = 
	Map.insert (n ++ ":" ++ t) i (namesAndTypes (Class(typ, fieldDecls, m)) (i + 1))

sortMap :: Map String Int -> Map String Int
sortMap m = Map.fromList $ (sortBy (\(a1, b1) (a2, b2) -> b1 `compare` b2)) $ Map.toList m

getCPHashTable :: Class -> Map String Int
getCPHashTable (Class(typ, fieldDecls, methodDecls)) = 
	let methodHT = getMethodHT methodDecls 0 in
	let fieldHT = getFieldHT fieldDecls (Map.size methodHT) in 
	Map.union 
		(Map.union methodHT fieldHT) 
		(Map.fromList 
			[ (typ, (Map.size fieldHT) + (Map.size methodHT))
			, ("java/lang/Object"
			, (Map.size fieldHT) + (Map.size methodHT) + 1)
			]
		)


getMethodHT :: [MethodDecl] -> Int -> Map String Int
getMethodHT [] _ = Map.empty
getMethodHT (Method(typ, m, args, _):ml) size = 
	--addNameAndType (m, (getMethodType typ args)) $ 
	Map.union 
		(Map.fromList [(m, size), (getMethodType typ args, size + 1)]) 
		(getMethodHT ml (size + 2))

getMethodType :: Type -> [(Type, String)] -> String
getMethodType a b = "(" ++ (getMethodTypeH a b)

getMethodTypeH :: Type -> [(Type, String)] -> String
getMethodTypeH retTyp [] = ")" ++ retTyp
getMethodTypeH t ((argT, _):args) = argT ++ (getMethodTypeH t args)

getFieldHT :: [FieldDecl] -> Int -> Map String Int
getFieldHT [] _ = Map.empty
getFieldHT (FieldDecl(typ, f):fl) size = 
	--addNameAndType (f, typ) $ 
	Map.union (Map.fromList [(f, size), (typ, size + 1)]) (getFieldHT fl (size + 2))

addNameAndType :: (String, String) -> Map String Int -> Map String Int
addNameAndType (name, typ) map = Map.insert (name ++ ":" ++ typ) (Map.size map) map

cpht_toCP_Infos :: Map String Int -> CP_Infos
cpht_toCP_Infos ht = foldr
	(\(name, index) rest -> 
		(Utf8_Info { 
			tag_cp = TagUtf8, 
			tam_cp = length name, 
			cad_cp = name, 
			desc = "#" ++ (show index) ++ ":" ++ name 
		}):rest) [] 
	((sortBy (\(a1, b1) (a2, b2) -> b1 `compare` b2)) $ Map.toList ht) 



natHT_toCP_Infos :: Map String Int -> Map String Int -> CP_Infos
natHT_toCP_Infos ht cpHt = Map.foldWithKey (\name index rest -> let [n, t] = splitOn ":" name in 
	(NameAndType_Info { 
		tag_cp = TagNameAndType, 
		index_name_cp = cpHt Map.! n, 
		index_descr_cp = cpHt Map.! t, 
		desc = name
	}):rest) [] ht

get_CP_Infos :: Class -> CP_Infos
get_CP_Infos c@(Class(typ, fieldDecls, methodDecls)) = do
	let cpht = getCPHashTable c
	let natCpht = namesAndTypes c (Map.size cpht)

	let strings = (cpht_toCP_Infos cpht) ++ (natHT_toCP_Infos natCpht cpht)

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
		index_nameandtype_cp = natCpht Map.! (fName ++ ":" ++ fTyp),
		desc = typ ++ "." ++ fName ++ ":" ++ fTyp
		} | FieldDecl(fTyp, fName) <- fieldDecls]


get_CP_Index :: Map String Integer -> String -> Int
get_CP_Index map quStr = fromInteger $ (\a -> case a of Just b -> b; Nothing -> -1) $ Map.lookup quStr map

get_ClassAccessFlags :: Class -> AccessFlags
get_ClassAccessFlags c = AccessFlags [1]

get_Interfaces :: Class -> Interfaces
get_Interfaces c = []

getFieldAccessFlags :: Class -> FieldDecl -> AccessFlags
getFieldAccessFlags _ _ = AccessFlags [1]

get_Field_Infos :: Class -> Field_Infos
get_Field_Infos c@(Class(_, fieldDecls, _)) = let ht = getCPHashTable c in 
	[Field_Info {
	af_fi = getFieldAccessFlags c fr,
	index_name_fi = ht Map.! name,
	index_descr_fi = ht Map.! typ,
	tam_fi = -1,
	array_attr_fi = []
} | fr@(FieldDecl(typ, name)) <- fieldDecls]

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
main = do let example = Class("Bsp", [FieldDecl("I", "n")], [
						  Method("V", "<init>", [], Block([
						  	StmtExprStmt(Assign("n", TypedExpr(Integer 0, "I")))
						  ])),
						  Method("V", "main", [("[Ljava/lang/String;", "args")], Block([
                            LocalVarDecl("I", "i"),
                            StmtExprStmt(Assign("i", TypedExpr(Integer 0, "I"))),
                            LocalVarDecl("boolean", "a"),
                            StmtExprStmt(Assign("a", TypedExpr(Bool True, "boolean"))),
                            LocalVarDecl("char", "b"),
                            StmtExprStmt(Assign("b", TypedExpr(Char('t'), "char"))),
                            If(TypedExpr(LocalOrFieldVar("a"), "boolean"), 
                                StmtExprStmt(Assign("a", TypedExpr(Integer 5, "I"))), 
                                Just (StmtExprStmt(Assign("b", TypedExpr(Char('f'), "char"))))
                            )]))
						  ])
              --StmtExprStmt(MethodCall(    System.out, "println", [] ))
          print $ getMethodHT (getMethodDeclsFromClass example) 1
          print $ getCPHashTable example
          print $ sortBy (\(a1, b1) (a2, b2) -> b1 `compare` b2) (Map.toList $ getCPHashTable example)
          putStrLn $ Pr.ppShow $ codegen example
