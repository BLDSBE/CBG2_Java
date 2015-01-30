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

namesAndTypes c@(Class(typ, [], (Method(t, n, args, _, _)):methodDecls)) i = 
        Map.insert (n ++ ":" ++ (getMethodType t args)) i (namesAndTypes (Class(typ, [], methodDecls)) (i + 1))

namesAndTypes c@(Class(typ, FieldDecl(t, n):fieldDecls, m)) i = 
        Map.insert (n ++ ":" ++ t) i (namesAndTypes (Class(typ, fieldDecls, m)) (i + 1))

sortMap :: Map String Int -> [(String, Int)]
sortMap m = (sortBy (\(a1, b1) (a2, b2) -> b1 `compare` b2)) $ Map.toList m

getUtf8HashTable :: Class -> Map String Int
getUtf8HashTable (Class(typ, fieldDecls, methodDecls)) = 
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
getMethodHT (Method(typ, m, args, _, _):ml) size =  
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

getConstantsHT :: [MethodDecl] -> Int -> Map String Int
getConstantsHT [] _ = Map.empty
getConstantsHT (Method(_, _, _, code, _):r) s = Map.union t (getConstantsHT r (Map.size t))
	where t = getConstantsHTHH code s

getConstantsHTH :: [Stmt] -> Int -> Map String Int
getConstantsHTH [] _     = Map.empty
getConstantsHTH (l:xl) s = Map.union t (getConstantsHTH xl (Map.size t))
	where t = getConstantsHTHH l s

getConstantsHTHH :: Stmt -> Int -> Map String Int
getConstantsHTHH (Block([]))                    s = Map.empty 
getConstantsHTHH (Block(stmt:stmts))            s = Map.union t (getConstantsHTH stmts (Map.size t)) where t = getConstantsHTHH stmt s              
getConstantsHTHH (Return(TypedExpr(expr, typ))) s = getConstantsHTHHE expr s
getConstantsHTHH (Return(_))                    _ = Map.empty
getConstantsHTHH ReturnV                        s = Map.empty
getConstantsHTHH (While(expr, stmt))            s = getConstantsHTHHE expr s
getConstantsHTHH (LocalVarDecl(typ, str))       s = Map.empty
getConstantsHTHH (If(expr, stmt, mbStmt))       s = getConstantsHTHHE expr s
getConstantsHTHH (StmtExprStmt(stmtExpr))       s = getConstantsHTHHS stmtExpr s

getConstantsHTHHE :: Expr -> Int -> Map String Int
getConstantsHTHHE This                         s = Map.empty
getConstantsHTHHE Super                        s = Map.empty
getConstantsHTHHE (LocalOrFieldVar v)          s = Map.fromList [(v, s)]
getConstantsHTHHE (InstVar (expr, str))        s = getConstantsHTHHE expr s
getConstantsHTHHE (Unary (str, expr))          s = getConstantsHTHHE expr s
getConstantsHTHHE (Binary (str, expr1, expr2)) s = Map.union t (getConstantsHTHHE expr2 (Map.size t)) where t = getConstantsHTHHE expr1 s
getConstantsHTHHE (Integer i)                  s = Map.fromList [(show i, s)]
getConstantsHTHHE (Bool b)                     s = Map.empty
getConstantsHTHHE (Char c)                     s = Map.fromList [(show c, s)]
getConstantsHTHHE (String s1)                  s = Map.fromList [(s1, s)]
getConstantsHTHHE (Jnull)                      s = Map.empty
getConstantsHTHHE (StmtExprExpr(stmtExpr))     s = getConstantsHTHHS stmtExpr s
getConstantsHTHHE (TypedExpr(expr, typ))       s = getConstantsHTHHE expr s

getConstantsHTHHS :: StmtExpr -> Int -> Map String Int
getConstantsHTHHS (Assign(e1, e2)) s = Map.union t $ getConstantsHTHHE e2 $ Map.size t where t = getConstantsHTHHE e1 s
getConstantsHTHHS (New(typ, exprs)) s = Map.empty
getConstantsHTHHS (MethodCall(expr, str, exprs)) s = Map.empty 
getConstantsHTHHS (TypedStmtExpr(stmtExpr, typ)) s = Map.empty



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
        (sortMap ht) 



natHT_toCP_Infos :: Map String Int -> Map String Int -> CP_Infos
natHT_toCP_Infos ht cpHt = foldr 
        (\(name, index) rest -> let [n, t] = splitOn ":" name in 
                (NameAndType_Info { 
                        tag_cp = TagNameAndType, 
                        index_name_cp = cpHt Map.! n, 
                        index_descr_cp = cpHt Map.! t, 
                        desc = "#" ++ (show index) ++ ":" ++ name
                }):rest) [] 
        (sortMap ht)

get_CP_Map :: Class -> Map String Int
get_CP_Map c = Map.union strs (namesAndTypes c (Map.size strs))
        where strs = getUtf8HashTable c

getConstantsCpEntries :: Class -> CP_Infos
getConstantsCpEntries c = []


get_CP_Infos :: Class -> CP_Infos
get_CP_Infos c@(Class(typ, fieldDecls, methodDecls)) = do
        let cpht = getUtf8HashTable c
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
        let cpStringsClassesFields = cpStringsClasses ++ [ FieldRef_Info {
                tag_cp = TagFieldRef,
                index_name_cp = cpht Map.! typ,
                index_nameandtype_cp = natCpht Map.! (fName ++ ":" ++ fTyp),
                desc = typ ++ "." ++ fName ++ ":" ++ fTyp
                } | FieldDecl(fTyp, fName) <- fieldDecls]

        let cpStringsClassesFieldsMethods = cpStringsClassesFields ++ [MethodRef_Info {
                tag_cp = TagMethodRef,
                index_name_cp = cpht Map.! typ,
                index_nameandtype_cp = natCpht Map.! (mName ++ ":" ++ (getMethodType mTyp mArgs)),
                desc = typ ++ "." ++ mName ++ ":" ++ mTyp
        } | Method(mTyp, mName, mArgs, _, _) <- methodDecls]
        cpStringsClassesFieldsMethods ++ (getConstantsCpEntries c)


get_CP_Index :: Map String Integer -> String -> Int
get_CP_Index map quStr = fromInteger $ (\a -> case a of Just b -> b; Nothing -> -1) $ Map.lookup quStr map

get_ClassAccessFlags :: Class -> AccessFlags
get_ClassAccessFlags c = AccessFlags [1]

get_Interfaces :: Class -> Interfaces
get_Interfaces c = []

getFieldAccessFlags :: Class -> FieldDecl -> AccessFlags
getFieldAccessFlags _ _ = AccessFlags [1]

get_Field_Infos :: Class -> Field_Infos
get_Field_Infos c@(Class(_, fieldDecls, _)) = let ht = getUtf8HashTable c in 
        [Field_Info {
        af_fi = getFieldAccessFlags c fr,
        index_name_fi = ht Map.! name,
        index_descr_fi = ht Map.! typ,
        tam_fi = -1,
        array_attr_fi = []
} | fr@(FieldDecl(typ, name)) <- fieldDecls]

getMethodAccessFlags :: Class -> MethodDecl -> AccessFlags
getMethodAccessFlags _ (Method (_, _, _, _, False)) = AccessFlags [1]  
getMethodAccessFlags _ (Method (_, _, _, _, True)) = AccessFlags [1, 8]  

get_Method_Infos :: Class -> Method_Infos
get_Method_Infos c@(Class(_, _, methodDecls)) = let ht = get_CP_Map c in 
        [ Method_Info {        
          af_mi = getMethodAccessFlags c m,
          index_name_mi = ht Map.! name,
          index_descr_mi = ht Map.! (getMethodType typ args),
          tam_mi = 1,
          array_attr_mi = [ AttributeCode {
                  index_name_attr = ht Map.! name,
                  tam_len_attr = -1,
                  len_stack_attr = -1,
                  len_local_attr = -1,
                  tam_code_attr = length $ compileStmt code ht,
                  array_code_attr = compileStmt code ht,
                  tam_ex_attr = 0,
                  array_ex_attr = [],
                  tam_atrr_attr = 0,
                  array_attr_attr = []
          }]
}| m@(Method(typ, name, args, code, _)) <- methodDecls]

retTypLookup :: Map Type String
retTypLookup = Map.fromList [("I", "i"), ("C", "c"), ("Z", "i"), 
        ("L", "l"), 
        ("D", "d"),
        ("F", "f")] 

compileStmt :: Stmt -> Map String Int -> [Code]
compileStmt (Block([]))                    ht = []
compileStmt (Block(stmt:stmts))            ht = (compileStmt stmt ht) ++ (compileStmt (Block stmts) ht)
compileStmt (Return(TypedExpr(expr, typ))) ht = (compileExpr expr ht) ++ [(retTypLookup Map.! typ) ++ "return"]
compileStmt (Return(_))                    _  = ["nope"]
compileStmt ReturnV                        ht = ["return"]
compileStmt (While(expr, stmt))            ht = ["while"] ++ (compileExpr expr ht) ++ (compileStmt stmt ht)
compileStmt (LocalVarDecl(typ, str))       ht = ["LocalVarDecl"]
compileStmt (If(expr, stmt, mbStmt))       ht = ["if"] ++ (compileExpr expr ht) ++ ["then"] ++ (compileStmt stmt ht)
compileStmt (StmtExprStmt(stmtEpxr))       ht = ["stmtexprstmt"]

compileExpr :: Expr -> Map String Int -> [Code]
compileExpr This                         ht = ["push this"]
compileExpr Super                        ht = ["push super"]
compileExpr (LocalOrFieldVar s)          ht = ["aload " ++ (show $ ht Map.! s)]
compileExpr (InstVar (expr, str))        ht = ["instvar: "] ++ (compileExpr expr ht) ++ [", " ++ str]
compileExpr (Unary (str, expr))          ht = ["unary: " ++ str ++ ", "] ++ (compileExpr expr ht)
compileExpr (Binary (str, expr1, expr2)) ht = ["binary: " ++ str ++ ", "] ++ (compileExpr expr1 ht) ++ [", "] ++ (compileExpr expr2 ht)
compileExpr (Integer i)                  ht = ["push " ++ (show i)]
compileExpr (Bool b)                     ht = ["push " ++ (show b)]
compileExpr (Char c)                     ht = ["push " ++ [c]]
compileExpr (String s)                   ht = ["push " ++ s]
compileExpr (Jnull)                      ht = ["push null"]
compileExpr (StmtExprExpr(stmtExpr))     ht = compileStmtExpr stmtExpr ht
compileExpr (TypedExpr(expr, typ))       ht = (compileExpr expr ht) ++ [typ]

compileStmtExpr :: StmtExpr -> Map String Int -> [Code]
compileStmtExpr (Assign(
		TypedExpr(expr1, typ1), TypedExpr(expr2, typ2))) ht = 
	(compileExpr expr1 ht) ++ [": " ++ typ1 ++ ":="] ++ (compileExpr expr2 ht) ++ [": " ++ typ2]
compileStmtExpr (New(typ, exprs)) ht = 
	["new: " ++ typ ++ ": "] ++ (foldr (\a r -> a ++ r) [] [compileExpr e ht | e <- exprs])
compileStmtExpr (MethodCall(expr, str, exprs)) ht = 
	["method: "] ++ (compileExpr expr ht) ++ [", " ++ str] ++ (foldr (\a r -> (a ++ r)) [] (map (\e -> compileExpr e ht) exprs))
compileStmtExpr (TypedStmtExpr(stmtExpr, typ)) ht = (compileStmtExpr stmtExpr ht) ++ [": " ++ typ]

--TODO--
get_Attribute_Infos :: Class -> Attribute_Infos
get_Attribute_Infos c = []
--End TODO--

codegen :: Class -> ClassFile
codegen c@(Class(typ, fieldDecls, methodDecls)) = ClassFile {
    magic = Magic,
    minver = MinorVersion 0,
    maxver = MajorVersion 52,
    count_cp = length $ get_CP_Infos c,
    array_cp = get_CP_Infos c,
    acfg = get_ClassAccessFlags c,
    this = ThisClass { index_th = cpHt Map.! typ },
    super = SuperClass { index_sp = cpHt Map.! "java/lang/Object" },
    count_interfaces = length $ get_Interfaces c,
    array_interfaces = get_Interfaces c,
    count_fields = length $ get_Field_Infos c,
    array_fields = get_Field_Infos c,
    count_methods = length $ get_Method_Infos c,
    array_methods = get_Method_Infos c,
    count_attributes = length $ get_Attribute_Infos c,
    array_attributes = get_Attribute_Infos c
} where cpHt = get_CP_Map c



main :: IO()
main = do let example = Class("Bsp", [FieldDecl("I", "n"), FieldDecl("Z", "b1"), FieldDecl("C", "c1"), FieldDecl("F", "f1"), FieldDecl("D", "d1")], [
                          Method("V", "<init>", [], Block([
                            StmtExprStmt(Assign(LocalOrFieldVar("n"), TypedExpr(Integer 0, "I")))
                          ]), False),
                          Method("V", "main", [("[Ljava/lang/String;", "args")], Block([
                            LocalVarDecl("I", "i"),
                            StmtExprStmt(Assign(LocalOrFieldVar("i"), TypedExpr(Integer 50000, "I"))),
                            LocalVarDecl("boolean", "a"),
                            StmtExprStmt(Assign(LocalOrFieldVar("a"), TypedExpr(Bool True, "boolean"))),
                            LocalVarDecl("char", "b"),
                            StmtExprStmt(Assign(LocalOrFieldVar("b"), TypedExpr(Char('t'), "char"))),
                            If(TypedExpr(LocalOrFieldVar("a"), "boolean"), 
                              StmtExprStmt(Assign(LocalOrFieldVar("a"), TypedExpr(Integer 5, "I"))), 
                              Just (StmtExprStmt(Assign(LocalOrFieldVar("b"), TypedExpr(Char('f'), "char"))))
                            ),
                            ReturnV]), True)
                          ])
              --StmtExprStmt(MethodCall(    System.out, "println", [] ))
          putStrLn $ Pr.ppShow $ getConstantsHT (getMethodDeclsFromClass example) 0
          putStrLn $ Pr.ppShow $ codegen example
