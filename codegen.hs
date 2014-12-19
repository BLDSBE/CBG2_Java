module Codegen where
import ClassFormat
import AbsSyn

get_CP_Infos :: Class -> CP_Infos
get_CP_Infos (Class(typ, fieldDecls, methodDecls)) = [ Class_Info {
    tag_cp = TagClass,
    index_cp = 0,
    desc = typ
}]

get_AccessFlags :: Class -> AccessFlags
get_AccessFlags c = AccessFlags []

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
    acfg = get_AccessFlags c,
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
main = do let example = Class("Bsp", [], [Method("void", "main", [("String[]", "args")], Block([
                          LocalVarDecl("int", "i"),
                          StmtExprStmt(Assign("i", TypedExpr(Integer 0, "int"))),
                          LocalVarDecl("boolean", "a"),
                          StmtExprStmt(Assign("a", TypedExpr(Bool True, "boolean"))),
                          LocalVarDecl("char", "b"),
                          StmtExprStmt(Assign("b", TypedExpr(Char('t'), "char"))),
                          If(TypedExpr(LocalOrFieldVar("a"), "boolean"), 
                              StmtExprStmt(Assign("a", TypedExpr(Integer 5, "int"))), 
                              Just (StmtExprStmt(Assign("b", TypedExpr(Char('f'), "char"))))
                          )]))])
              --StmtExprStmt(MethodCall(    System.out, "println", [] ))
          print $ codegen example
