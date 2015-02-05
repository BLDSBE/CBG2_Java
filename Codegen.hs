module Codegen where
import ClassFormat
import AbsSyn
import Typecheck hiding (main)
import Constants
import Data.List.Split
import Data.List
import qualified Text.Show.Pretty as Pr
import Data.Map (Map)              -- This just imports the type name
import qualified Data.Map as Map  -- Imports everything else, but with names 
                                  -- prefixed with "Map." (with the period).

readMaybe :: (Read a) => String -> Maybe a
readMaybe s = case reads s of
  [(x, "")] -> Just x
  _ -> Nothing


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
  Map.union (Map.fromList [(f, size), (typ, size + 1)]) (getFieldHT fl (size + 2))

getConstantsHT :: [MethodDecl] -> Int -> Map String Int
getConstantsHT [] _ = Map.empty
getConstantsHT (Method(_, _, _, code, _):r) s = Map.union t (getConstantsHT r (Map.size t))
  where t = getConstantsHTHH code s

getConstantsHTH :: [Stmt] -> Int -> Map String Int
getConstantsHTH [] _     = Map.empty
getConstantsHTH (l:xl) s = Map.union t (getConstantsHTH xl ((s + (Map.size t))))
  where t = getConstantsHTHH l s

getConstantsHTHH :: Stmt -> Int -> Map String Int
getConstantsHTHH (Block([]))                    s = Map.empty 
getConstantsHTHH (Block(stmt:stmts))            s = Map.union t (getConstantsHTH stmts (s + (Map.size t))) where t = getConstantsHTHH stmt s              
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
getConstantsHTHHE (LocalOrFieldVar v)          s = Map.fromList [("var:" ++ v, s)]
getConstantsHTHHE (InstVar (expr, str))        s = getConstantsHTHHE expr s
getConstantsHTHHE (Unary (str, expr))          s = getConstantsHTHHE expr s
getConstantsHTHHE (Binary (str, expr1, expr2)) s = Map.union t (getConstantsHTHHE expr2 (s + (Map.size t))) where t = getConstantsHTHHE expr1 s
getConstantsHTHHE (Integer i)                  s = Map.fromList [(show i, s)]
getConstantsHTHHE (Bool b)                     s = Map.empty
getConstantsHTHHE (Char c)                     s = Map.fromList [(show c, s)]
getConstantsHTHHE (String s1)                  s = Map.fromList [(s1, s)]
getConstantsHTHHE (Jnull)                      s = Map.empty
getConstantsHTHHE (StmtExprExpr(stmtExpr))     s = getConstantsHTHHS stmtExpr s
getConstantsHTHHE (TypedExpr(expr, typ))       s = Map.fromList $ map (\(s, i) -> ((typ ++ ":" ++ s), i)) $ Map.toList $ getConstantsHTHHE expr s

getConstantsHTHHS :: StmtExpr -> Int -> Map String Int
getConstantsHTHHS (Assign(e1, e2)) s = Map.union t $ getConstantsHTHHE e2 $ (s + (Map.size t)) where t = getConstantsHTHHE e1 s
getConstantsHTHHS (New(typ, exprs)) s = Map.empty
getConstantsHTHHS (MethodCall(expr, str, exprs)) s = Map.empty 
getConstantsHTHHS (TypedStmtExpr(stmtExpr, typ)) s = Map.fromList $ map (\(s, i) -> ((typ ++ ":" ++ s), i)) $ Map.toList $ getConstantsHTHHS stmtExpr s

cleanupConstantsCpEntries :: Map String Int -> Map String Int
cleanupConstantsCpEntries = Map.mapKeys 
  (\k -> let (s1:s2:rest) = reverse $ splitOn ":" k in
    s2 ++ ":" ++ s1 
  )

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

get_CP_Map_no_constants :: Class -> Map String Int
get_CP_Map_no_constants c = Map.union strs (namesAndTypes c (Map.size strs))
  where strs = getUtf8HashTable c

get_CP_Map :: Class -> Map String Int
get_CP_Map c = Map.union t (cleanupConstantsCpEntries $ getConstantsHT (getMethodDeclsFromClass c) (Map.size t)) where t = get_CP_Map_no_constants c

getConstantsCpEntries :: Class -> CP_Infos
getConstantsCpEntries c = let ht = sortMap $ getConstantsHT (getMethodDeclsFromClass c) (Map.size t) in
  constantsHTToCP_Infos ht
  where t = get_CP_Map_no_constants c

constantsHTToCP_Infos :: [(String, Int)] -> CP_Infos
constantsHTToCP_Infos l = foldr (\a b -> a ++ b) [] $ map constantsHTEntryToCP_Info l

constantsHTEntryToCP_Info :: (String, Int) -> CP_Infos
constantsHTEntryToCP_Info (s, i) = let l = reverse $ splitOn ":" s in
  case l of 
    (content:typ:rest) 
      | typ == "I"         -> case (readMaybe content) of
        Just content'      -> [Integer_Info { tag_cp = TagInteger, numi_cp = content', desc = typ ++ ":" ++ content }]   
        Nothing            -> [Utf8_Info { tag_cp = TagUtf8, tam_cp = length content, cad_cp = content, desc = typ ++ ":" ++ content}]
      | typ == "C"         -> case content of 
        ['\'', char, '\''] -> [Integer_Info { tag_cp = TagInteger, numi_cp = fromEnum char, desc = typ ++ ":" ++ content }]
        _                   -> [Utf8_Info { tag_cp = TagUtf8, tam_cp = length content, cad_cp = content, desc = typ ++ ":" ++ content}]
      | typ == "var"       -> [Utf8_Info { tag_cp = TagUtf8, tam_cp = length content, cad_cp = content, desc = typ ++ ":" ++ content}]
    
  
  
  
  


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
    }, Class_Info {
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
                  tam_code_attr = length $ compiledCode,
                  array_code_attr = compiledCode,
                  tam_ex_attr = 0,
                  array_ex_attr = [],
                  tam_atrr_attr = 0,
                  array_attr_attr = []
          }] 
}| m@(Method(typ, name, args, code, _)) <- methodDecls, let (compiledCode, locals) = compileStmt code ht Map.empty]

retTypLookup :: Map Type String
retTypLookup = Map.fromList [("I", "i"), ("C", "c"), ("Z", "i"), 
        ("L", "l"), 
        ("D", "d"),
        ("F", "f")] 

compileStmt :: Stmt -> Map String Int -> Map String Int -> ([Code], Map String Int)
compileStmt (Block([]))                      ht locals = ([], locals)
compileStmt (Block(stmt:stmts))              ht locals = 
  let (code1, locals1) = compileStmt stmt ht locals
      (code2, locals2) = compileStmt (Block stmts) ht locals1
  in (code1 ++ code2, locals2)
compileStmt (Return(TypedExpr(expr, typ)))  ht locals = let (code, locals1) = compileExpr expr ht locals in (code ++ [(retTypLookup Map.! typ) ++ "return;"], locals1)
compileStmt ReturnV                          ht locals = (["return;"], locals)
compileStmt (While(expr, stmt))              ht locals = 
  let (code1, locals1) = compileExpr expr ht locals
      (code2, locals2) = compileStmt stmt ht locals1 
  in (["while"] ++ code1 ++ code2, locals2)
compileStmt (LocalVarDecl(typ, str))         ht locals = (["LocalVarDecl: " ++ typ ++ ": " ++ str ++ ";"], Map.insert (str) (Map.size locals) locals)
compileStmt (If(expr, stmt, mbStmt))         ht locals = 
  let (code1, locals1) = compileExpr expr ht locals
      (code2, locals2) = compileStmt stmt ht locals1 in
        case mbStmt of
          Just stmtElse -> let (code3, locals3) = compileStmt stmtElse ht locals2 in
            (["if"] ++ code1 ++ ["then"] ++ code2 ++ ["else"] ++ code3, locals3)
          Nothing -> (["if"] ++ code1 ++ ["then"] ++ code2, locals2)
compileStmt (StmtExprStmt(stmtExpr))         ht locals = compileStmtExpr stmtExpr ht locals

compileExpr :: Expr -> Map String Int -> Map String Int -> ([Code], Map String Int)
compileExpr This                            ht locals = (["push this"], locals)
compileExpr Super                            ht locals = (["push super"], locals)
compileExpr (LocalOrFieldVar s)              ht locals = case (Map.lookup s locals) of
  Just i ->  (["aload_" ++ (show i)], locals)
  Nothing -> (["aload " ++ (show $ ht Map.! ("var:" ++ s))], locals)
compileExpr (InstVar (expr, str))            ht locals = let (code, locals1) = compileExpr expr ht locals in (["instvar: "] ++ code ++ [", " ++ str], locals1)
compileExpr (Unary (str, expr))              ht locals = let (code, locals1) = compileExpr expr ht locals in (["unary: " ++ str ++ ", "] ++ code, locals1)
compileExpr (Binary (str, expr1, expr2))     ht locals = 
  let (code1, locals1) = compileExpr expr1 ht locals
      (code2, locals2) = compileExpr expr2 ht locals in
        (["binary: " ++ str ++ ", "] ++ code1 ++ [", "] ++ code2, locals)
compileExpr (Integer i)                      ht locals = (["push " ++ (show i) ++ ";"], locals)
compileExpr (Bool b)                         ht locals  
  | b == True  = (["iconst_1;"], locals)
  | b == False = (["iconst_0;"], locals)
compileExpr (Char c)                         ht locals = (["push " ++ [c] ++ ";"], locals)
compileExpr (String s)                       ht locals = (["push " ++ s ++ ";"], locals)
compileExpr (Jnull)                          ht locals = (["push null;"], locals)
compileExpr (StmtExprExpr(stmtExpr))         ht locals = let (code, locals1) =compileStmtExpr stmtExpr ht locals in (code, locals1)
compileExpr (TypedExpr(expr, typ))           ht locals = let (code, locals1) = compileExpr expr ht locals in (code ++ [typ], locals1)

compileStmtExpr :: StmtExpr -> Map String Int -> Map String Int -> ([Code], Map String Int)
compileStmtExpr (Assign(TypedExpr(LocalOrFieldVar var, typ1), TypedExpr(expr1, typ2))) ht locals = 
  let (code1, locals1) = compileExpr expr1 ht locals in case (Map.lookup var locals) of
    Just i  -> (code1 ++ ["->" ++ "istore_" ++ (show i)], locals1)
    Nothing -> (code1 ++ ["->" ++ "istore " ++ (show $ ht Map.! ("var:" ++ var))], locals1)

--compileStmtExpr (New(typ, exprs)) ht locals = 
--  (["new: " ++ typ ++ ": "] ++ (foldr (\a r -> a ++ r) [] [compileExpr e ht | e <- exprs]), locals)

--compileStmtExpr (MethodCall(expr, str, exprs)) ht locals = let (code, locals1) = compileExpr expr ht locals in
--  (["method: "] ++ code ++ [", " ++ str] ++ (foldr (\a r -> (a ++ r)) [] (map (\e -> compileExpr e ht locals) exprs)), locals)

compileStmtExpr (TypedStmtExpr(stmtExpr, typ)) ht locals = let (code, locals1) = compileStmtExpr stmtExpr ht locals in (code ++ [": " ++ typ], locals1)

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
main = do 
  let example = head $ typecheck $ [ Class ("Bsp", [FieldDecl("I", "n"), FieldDecl("Z", "b1"), FieldDecl("C", "c1"), FieldDecl("F", "f1"), FieldDecl("D", "d1")], [
        Method ("V", "<init>", [], Block([
          StmtExprStmt $ Assign (LocalOrFieldVar "n", Integer 0)
        ]), False),
        Method ("V", "main", [("[Ljava/lang/String;", "args")], Block ([
          LocalVarDecl("I", "i"),
          StmtExprStmt $ Assign (LocalOrFieldVar "i", Integer 50000),
          LocalVarDecl ("Z", "a"),
          StmtExprStmt $ Assign (LocalOrFieldVar "a", Bool True),
          LocalVarDecl ("C", "b"),
          StmtExprStmt $ Assign (LocalOrFieldVar "b", Char 't'),
          If (LocalOrFieldVar "a", 
            StmtExprStmt $ Assign (LocalOrFieldVar "i", Integer 5), 
            Just $ StmtExprStmt $ Assign $ (LocalOrFieldVar "b", Char 'f')
          ),
          ReturnV
        ]), True)
        ])]
  --StmtExprStmt(MethodCall(    System.out, "println", [] ))
  putStrLn $ Pr.ppShow $ example
  putStrLn $ Pr.ppShow $ getConstantsHT (getMethodDeclsFromClass example) 0
  putStrLn $ Pr.ppShow $ getConstantsCpEntries example
  putStrLn $ Pr.ppShow $ get_CP_Map example
  putStrLn $ Pr.ppShow $ codegen example
