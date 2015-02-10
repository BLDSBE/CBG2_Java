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
  let methodHT = getMethodStringsHT methodDecls 0 in
  let fieldHT = getFieldStringsHT fieldDecls (Map.size methodHT) in 
  Map.union 
    (Map.union methodHT fieldHT) 
    (Map.fromList 
      [ (typ, (Map.size fieldHT) + (Map.size methodHT))
      , ("java/lang/Object"
      , (Map.size fieldHT) + (Map.size methodHT) + 1)
      ]
    )


getMethodStringsHT :: [MethodDecl] -> Int -> Map String Int
getMethodStringsHT [] _ = Map.empty
getMethodStringsHT (Method(typ, m, args, _, _):ml) size =  
  Map.union 
    (Map.fromList [(m, size), (getMethodType typ args, size + 1)]) 
    (getMethodStringsHT ml (size + 2))

getMethodType :: Type -> [(Type, String)] -> String
getMethodType a b = "(" ++ (getMethodTypeH a b)

getMethodTypeH :: Type -> [(Type, String)] -> String
getMethodTypeH retTyp [] = ")" ++ retTyp
getMethodTypeH t ((argT, _):args) = argT ++ (getMethodTypeH t args)

getFieldStringsHT :: [FieldDecl] -> Int -> Map String Int
getFieldStringsHT [] _ = Map.empty
getFieldStringsHT (FieldDecl(typ, f):fl) size = 
  Map.union (Map.fromList [(f, size), (typ, size + 1)]) (getFieldStringsHT fl (size + 2))




getFieldHT :: Class -> Int -> Map String Int
getFieldHT (Class(_, [], _)) _ = Map.empty
getFieldHT (Class(cname, (FieldDecl(typ, f):fl), m)) size = 
  Map.union (Map.fromList [("field:" ++ f, size)]) (getFieldHT (Class(cname, fl, m)) (size + 1))

getMethodHT :: Class -> Int -> Map String Int
getMethodHT (Class(_, _, [])) _ = Map.empty
getMethodHT (Class(cname, f, (Method(typ, name, _, _, _)):ml)) size = 
  Map.union (Map.fromList [("method:" ++ name, size)]) (getMethodHT (Class(cname, f, ml)) (size + 1))




getConstantsHT :: [MethodDecl] -> Int -> Map String Int
getConstantsHT [] _ = Map.empty
getConstantsHT (Method(_, _, _, code, _):r) s = Map.union t (getConstantsHT r (s + (Map.size t)))
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
getConstantsHTHH (If(expr, stmt, Just(stmtElse))) s = Map.union t1 t2 where 
  t1 = getConstantsHTHH stmt s
  t2 = getConstantsHTHH stmtElse (s + (Map.size t1))
getConstantsHTHH (If(expr, stmt, Nothing))      s = getConstantsHTHH stmt s
getConstantsHTHH (StmtExprStmt(stmtExpr))       s = getConstantsHTHHS stmtExpr s

getConstantsHTHHE :: Expr -> Int -> Map String Int
getConstantsHTHHE This                         s = Map.empty
getConstantsHTHHE Super                        s = Map.empty
getConstantsHTHHE (LocalOrFieldVar v)          s = Map.empty
--getConstantsHTHHE (LocalOrFieldVar v)          s = Map.fromList [("var:" ++ v, s)]
getConstantsHTHHE (InstVar (expr, str))        s = getConstantsHTHHE expr s
getConstantsHTHHE (Unary (str, expr))          s = getConstantsHTHHE expr s
getConstantsHTHHE (Binary (str, expr1, expr2)) s = Map.union t (getConstantsHTHHE expr2 (s + (Map.size t))) where t = getConstantsHTHHE expr1 s
getConstantsHTHHE (Integer i)                  s = Map.fromList [(show i, s)]
getConstantsHTHHE (Bool b)                     s = Map.empty
getConstantsHTHHE (Char c)                     s = Map.fromList [(show c, s)]
getConstantsHTHHE (String s1)                  s = Map.fromList [(s1, s+1), ("u:" ++ (show (s+1)), s)]
getConstantsHTHHE (Jnull)                      s = Map.empty
getConstantsHTHHE (StmtExprExpr(stmtExpr))     s = getConstantsHTHHS stmtExpr s
getConstantsHTHHE (TypedExpr(expr, typ))       s = Map.fromList $ map (\(s, i) -> ((typ ++ ":" ++ s), i)) $ Map.toList $ getConstantsHTHHE expr s

getConstantsHTHHS :: StmtExpr -> Int -> Map String Int
getConstantsHTHHS (Assign(e1, e2)) s = getConstantsHTHHE e2 s
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
get_CP_Map_no_constants c@(Class(typ, f, m)) = meths
  where strs   = getUtf8HashTable c
        nat    = Map.union strs (namesAndTypes c (Map.size strs))
        clas   = Map.union nat (Map.fromList [("class:" ++ typ, Map.size nat), ("class:java/lang/Object", (Map.size nat) + 1)])
        fields = Map.union clas (getFieldHT c (Map.size clas))
        meths  = Map.union fields (getMethodHT c (Map.size fields))

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
      | typ == "I"          -> case (readMaybe content) of
        Just content'       -> [Integer_Info { tag_cp = TagInteger, numi_cp = content', desc = typ ++ ":" ++ content }]   
        Nothing             -> [Utf8_Info { tag_cp = TagUtf8, tam_cp = length content, cad_cp = content, desc = typ ++ ":" ++ content}]
      | typ == "C"          -> case content of 
        ['\'', char, '\'']  -> [Integer_Info { tag_cp = TagInteger, numi_cp = fromEnum char, desc = typ ++ ":" ++ content }]
        _                   -> [Utf8_Info { tag_cp = TagUtf8, tam_cp = length content, cad_cp = content, desc = typ ++ ":" ++ content}]
      | typ == stringType   -> [Utf8_Info { tag_cp = TagUtf8, tam_cp = length content, cad_cp = content, desc = "S:" ++ content}]
      | typ == "u"          -> [String_Info {tag_cp = TagString, index_cp = read content, desc = typ ++ ":#" ++ content} ]
      | typ == "var"        -> [Utf8_Info { tag_cp = TagUtf8, tam_cp = length content, cad_cp = content, desc = typ ++ ":" ++ content}]
    
  
 


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
    desc = typ ++ "." ++ mName ++ ":" ++ (getMethodType mTyp mArgs)
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
  [ Field_Info {
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
      len_stack_attr = stackHeight,
      len_local_attr = Map.size locals,
      tam_code_attr = length $ compiledCode,
      array_code_attr = compiledCode,
      tam_ex_attr = 0,
      array_ex_attr = [],
      tam_atrr_attr = 0,
      array_attr_attr = []
  }] 
}| m@(Method(typ, name, args, code, _)) <- methodDecls, 
   let (compiledCode, locals, stackHeight) = compileStmt code ht (Map.fromList [(s, i) | (_, s) <- args, i <- [1..((length args) + 1)]]) 0]

retTypLookup :: Map Type String
retTypLookup = Map.fromList [("I", "i"), ("C", "c"), ("Z", "i"), 
        ("L", "l"), 
        ("D", "d"),
        ("F", "f")] 

compileStmt :: Stmt -> Map String Int -> Map String Int -> Int -> ([Code], Map String Int, Int)
compileStmt (Block([]))                      ht locals sh = ([], locals, 0)
compileStmt (Block(stmt:stmts))              ht locals sh = 
  let (code1, locals1, sh1) = compileStmt stmt ht locals sh
      (code2, locals2, sh2) = compileStmt (Block stmts) ht locals1 sh1
  in (code1 ++ code2, locals2, max sh1 sh2)
compileStmt (Return(TypedExpr(expr, typ)))   ht locals sh = let (code, locals1, sh1) = compileExpr expr ht locals sh in (code ++ [(retTypLookup Map.! typ) ++ "return;"], locals1, sh1)
compileStmt ReturnV                          ht locals sh = (["return;"], locals, sh)
compileStmt (While(expr, stmt))              ht locals sh = 
  let (codeIf, locals1, sh1) = compileExpr expr ht locals sh
      (codeBody, locals2, sh2) = compileStmt stmt ht locals1 sh1
      jumplength = (length codeBody) + (length codeIf) + 1
  -- while con do stuff == if(!con) jump; stuff; if(con) -jump
  in (codeIf ++ ["ifeq " ++ show (jumplength)] ++ codeBody ++ codeIf ++ ["ifne " ++ show (-jumplength)], locals2,  max sh1 sh2)
compileStmt (LocalVarDecl(typ, str))         ht locals sh = ([], Map.insert (str) (Map.size locals) locals, sh)
compileStmt (If(expr, stmt, mbStmt))         ht locals sh = 
  let (codeIf,   locals1, sh1) = compileExpr expr ht locals  sh
      (codeThen, locals2, sh2) = compileStmt stmt ht locals1 sh1 in
        case mbStmt of
          Just stmtElse -> let (codeElse, locals3, sh3) = compileStmt stmtElse ht locals2 sh2 in
            -- ifeq: if stacktop == 0 jump ahead (length codeThen) instructions 
            -- else: jump afterwards ahead length of elseblock
            (codeIf ++ ["ifeq " ++ (show $ length codeThen) ++ ";"] ++ codeThen ++ ["iconst_0", "ifeq " ++ (show $ length codeElse)] ++ codeElse, locals3, max 1 $ max sh1 $ max sh2 sh3)
          Nothing -> (codeIf ++ ["ifeq " ++ (show $ length codeThen) ++ ";"] ++ codeThen, locals2, max sh1 sh2)
compileStmt (StmtExprStmt(stmtExpr))         ht locals sh = compileStmtExpr stmtExpr ht locals sh

compileExpr :: Expr -> Map String Int -> Map String Int -> Int -> ([Code], Map String Int, Int)
                                       --this is a hack: i really should lookup "this". but in the construction, "this" is always before "super"
compileExpr This                             ht locals sh = (["aload " ++ (show $ (ht Map.! "class:java/lang/Object") - 1)],  locals, sh + 1)
compileExpr Super                            ht locals sh = (["aload " ++ (show $ ht Map.! "class:java/lang/Object")], locals, sh + 1)
compileExpr (LocalOrFieldVar s)              ht locals sh = case (Map.lookup s locals) of
  Just i ->  (["aload_" ++ (show i)], locals, sh + 1)
  Nothing -> (["aload " ++ (show $ ht Map.! ("field:" ++ s))], locals, sh + 1)
compileExpr (InstVar (expr, str))            ht locals sh = let (code, locals1, sh1) = compileExpr expr ht locals sh in (["instvar: "] ++ code ++ [", " ++ str], locals1, sh1)
compileExpr (Unary (str, expr))              ht locals sh = let (code, locals1, sh1) = compileExpr expr ht locals sh in (["unary: " ++ str ++ ", "] ++ code, locals1, sh1)
compileExpr (Binary (str, expr1, expr2))     ht locals sh = 
  let (code1, locals1, sh1) = compileExpr expr1 ht locals sh
      (code2, locals2, sh2) = compileExpr expr2 ht locals sh1
      bin = [str]
      {-bin                   = case str of
        "==" ->
        ">"  ->
        "<"  ->
        ">=" ->
        "<=" ->-}
  in  (code1 ++ code2 ++ bin, locals, max sh1 sh2)
compileExpr (Integer i)                      ht locals sh = (["iload " ++ (show $ ht Map.! ("I:" ++ (show i))) ++ ";"], locals, sh + 1)
compileExpr (Bool b)                         ht locals sh 
  | b == True  = (["iconst_1;"], locals, sh + 1)
  | b == False = (["iconst_0;"], locals, sh + 1)
compileExpr (Char c)                         ht locals sh = (["iload " ++ (show $ ht Map.! ("C:" ++ (show c))) ++ ";"], locals, sh + 1)
compileExpr (String s)                       ht locals sh = (["push " ++ s ++ ";"], locals, sh + 1)
compileExpr (Jnull)                          ht locals sh = (["push null;"], locals, sh + 1)
compileExpr (StmtExprExpr(stmtExpr))         ht locals sh = let (code, locals1, sh1) = compileStmtExpr stmtExpr ht locals sh in (code, locals1, sh1)
compileExpr (TypedExpr(expr, typ))           ht locals sh = let (code, locals1, sh1) = compileExpr expr ht locals sh in (code, locals1, sh1)

compileStmtExpr :: StmtExpr -> Map String Int -> Map String Int -> Int -> ([Code], Map String Int, Int)
compileStmtExpr (Assign(TypedExpr(LocalOrFieldVar var, typ1), TypedExpr(expr1, typ2))) ht locals sh = 
  let (code1, locals1, sh1) = compileExpr expr1 ht locals sh in case (Map.lookup var locals) of
    Just i  -> (code1 ++ ["istore " ++ (show i)], locals1, sh1)
    Nothing -> (["aload " ++ (show $ (ht Map.! "class:java/lang/Object") - 1)] ++ 
                  code1 ++ ["putfield " ++ (show $ ht Map.! ("field:" ++ var)) ++ ";"], locals1, sh1 + 1)

compileStmtExpr (New(typ, exprs)) ht locals sh = 
  let (code1, locals1, sh1) = compileExprs exprs ht locals sh in 
    (["new: " ++ typ ++ ":"] ++ code1, locals1, sh1)

--compileStmtExpr (MethodCall(expr, str, exprs)) ht locals = let (code, locals1) = compileExpr expr ht locals in
--  (["method: "] ++ code ++ [", " ++ str] ++ (foldr (\a r -> (a ++ r)) [] (map (\e -> compileExpr e ht locals) exprs)), locals)

compileStmtExpr (TypedStmtExpr(stmtExpr, typ)) ht locals sh = let (code, locals1, sh1) = compileStmtExpr stmtExpr ht locals sh in (code, locals1, sh1)

compileExprs :: [Expr] -> Map String Int -> Map String Int -> Int -> ([Code], Map String Int, Int)
compileExprs (e:es) ht locals sh = 
  let (code1, locals1, sh1) = compileExpr e ht locals sh
      (code2, locals2, sh2) = compileExprs es ht locals sh1 in
        (code1 ++ code2, locals2, max sh1 sh2)
compileExprs [] _ locals sh = ([], locals, sh)

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
  let example = head $ typecheck $ [ Class ("Bsp", [FieldDecl("I", "n"), FieldDecl("Z", "b1"), FieldDecl("C", "c1"), FieldDecl("F", "f1"), FieldDecl("D", "d1"), FieldDecl("Ljava/lang/String", "s1")], [
        Method ("V", "<init>", [], Block([
          StmtExprStmt $ Assign (LocalOrFieldVar "n", Integer 0),
          StmtExprStmt $ Assign (LocalOrFieldVar "s1", String "abc")
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
  --putStrLn $ Pr.ppShow $ sortMap $ get_CP_Map_no_constants example
  --putStrLn $ Pr.ppShow $ getConstantsHT (getMethodDeclsFromClass example) 0
  --putStrLn $ Pr.ppShow $ getConstantsCpEntries example
  --putStrLn $ Pr.ppShow $ sortMap $ get_CP_Map example
  putStrLn $ Pr.ppShow $ codegen example
  --putStrLn $ Pr.ppShow $ sortMap $ get_CP_Map example
  --putStrLn $ Pr.ppShow $ get_CP_Infos example 
