module TestSuite where

import Typecheck hiding (main)
import AbsSyn
import Constants
import Codegen hiding (main)
import qualified Text.Show.Pretty as Pr

--Test class to evaluate a void method
voidClass = [Class("VoidTest",
                [],
                [Method("V", "<init>", [], Block[], False),
                Method("V", "voidMethod", [], 
                    Block([
                        LocalVarDecl("I", "i")
                    ]), True)]
                )]

--Test class to evaluate the 'this' call
thisClass = [Class("ThisTest",
                [FieldDecl("I", "i")],
                [Method("V", "<init>", [],
                    Block[(
                        StmtExprStmt(Assign((LocalOrFieldVar("i"), Integer(2))))
                    )], False),
                Method("I", "thisMethod", [],
                    Block([
                        StmtExprStmt(Assign(LocalOrFieldVar("i"), Integer(2))),
                        Return(LocalOrFieldVar("i"))
                    ]), True)]
                    )]

--Test class to evaluate an addition of two integer
--Later used in the example program fibNumClass
addClass = [Class("AddTest",
                [],
                [Method("V", "<init>", [],
                    Block([
                            --LocalVarDecl("I", "i"),
                            --LocalVarDecl("I", "j"),
                            --StmtExprStmt(Assign(LocalOrFieldVar("i"), Integer(2))), 
                            --StmtExprStmt(Assign(LocalOrFieldVar("j"), Integer(2)))
                            --StmtExprStmt(Assign(InstVar(LocalOrFieldVar("this"), "i"), Integer(2))), 
                            --StmtExprStmt(Assign(InstVar(LocalOrFieldVar("this"), "j"), Integer(2)))
                        ]), False),
                Method("I", "addMethod", [("I", "i"), ("I", "j")],
                        Block([
                            --StmtExprStmt(Assign(LocalOrFieldVar("i"), Integer(2))), 
                            --StmtExprStmt(Assign(LocalOrFieldVar("j"), Integer(3))),
                            Return(Binary("+", LocalOrFieldVar("i"), LocalOrFieldVar("j")))
                            ]), True)]
                )]

--Test class to evaluate the subtraction of two integer
--Later used in the example program fibNumClass                
subClass = [Class("SubTest",
            [],
                [Method("V", "<init>", [],
                    Block([
                            --StmtExprStmt(Assign(InstVar(LocalOrFieldVar("this"), "i"), Integer(2))),
                            --StmtExprStmt(Assign(InstVar(LocalOrFieldVar("this"), "j"), Integer(2)))
                        ]), False),
            Method("I", "subMethod", [("I", "i"), ("I", "j")],
                    Block([
                        --StmtExprStmt(Assign(LocalOrFieldVar("i"), Integer(2))), 
                        --StmtExprStmt(Assign(LocalOrFieldVar("j"), Integer(3))),
                        Return(Binary("-", LocalOrFieldVar("i"), LocalOrFieldVar("j")))
                        ]), True)]
                 )]
                 
--Test class to evaluate the multiplication of two integer
multClass = [Class("MultTest",
                [FieldDecl("I", "i"), FieldDecl("I", "j")],
                [Method("V", "<init>", [],
                    Block([
                            StmtExprStmt(Assign(LocalOrFieldVar("i"), Integer(2))), 
                            StmtExprStmt(Assign(LocalOrFieldVar("j"), Integer(2)))
                        ]), False),
                Method("I", "multMethod", [],
                        Block([
                            --StmtExprStmt(Assign(LocalOrFieldVar("i"), Integer(2))), 
                            --StmtExprStmt(Assign(LocalOrFieldVar("j"), Integer(3))),
                            Return(Binary("*", LocalOrFieldVar("i"), LocalOrFieldVar("j")))
                            ]), True)]
                 )]
                 
--Test class to evaluate the division of two Integer
--Check whether the divisor is zero is in place
divClass = [Class("divTest",
                [FieldDecl("I", "i"), FieldDecl("I", "j")],
                [Method("V", "<init>", [],
                    Block([
                            StmtExprStmt(Assign(LocalOrFieldVar("i"), Integer(2))), 
                            StmtExprStmt(Assign(LocalOrFieldVar("j"), Integer(2)))
                        ]), False),
                Method("I", "divMethod", [],
                        Block([
                            --StmtExprStmt(Assign(LocalOrFieldVar("i"), Integer(2))), 
                            --StmtExprStmt(Assign(LocalOrFieldVar("j"), Integer(3))),
                            If(Binary("!=", LocalOrFieldVar("j"), Integer(0)),
                                Return(Binary("/", LocalOrFieldVar("i"), LocalOrFieldVar("j"))),
                                Just(Return(Integer(0))))
                            ]), True)]
                 )]
                 
--Test class to evaluate whether two integer have the same value
equIntClass = [Class("EquIntTest",
                    [FieldDecl("I", "i"), FieldDecl("I", "j")],
                    [Method("V", "<init>", [], 
                              Block([
                                  LocalVarDecl("I", "k"),
                                  LocalVarDecl("I", "l"),
                                  StmtExprStmt(Assign(LocalOrFieldVar("k"), LocalOrFieldVar("i"))), 
                                  StmtExprStmt(Assign(LocalOrFieldVar("l"), LocalOrFieldVar("j")))
                                ]), False),
                    Method("Z", "equIntMethod", [],
                            Block([
                                LocalVarDecl("I", "k"),
                                LocalVarDecl("I", "l"),
                                StmtExprStmt(Assign(LocalOrFieldVar("k"), LocalOrFieldVar("i"))), 
                                StmtExprStmt(Assign(LocalOrFieldVar("l"), LocalOrFieldVar("j"))),
                                If(Binary("==", LocalOrFieldVar("k"), LocalOrFieldVar("l")), 
                                    Return(Bool(True)),
                                    Just(Return(Bool False)))
                                ]), True)]
                    )]
                 
--Test class to evaluate whether two boolean have the same value
equBolClass = [Class("EquBolTest",
                [FieldDecl("Z", "i")],
                [Method("V", "<init>", [],
                        Block([
                            LocalVarDecl("Z", "j"),
                            StmtExprStmt(Assign(LocalOrFieldVar("j"), LocalOrFieldVar("i")))
                        ]), False),
                Method("Z", "equBolMethod", [],
                        Block([
                            LocalVarDecl("Z", "j"),
                            StmtExprStmt(Assign(LocalOrFieldVar("j"), LocalOrFieldVar("i"))),
                            If(Binary("==", LocalOrFieldVar("j"), Bool(True)), 
                                Return(Bool(True)),
                                Just(Return(Bool False)))
                            ]), True)]
                )]
                 
--Test class to evaluate whether one of two integer has a greater value than the other
gretClass = [Class("GretTest",
                [FieldDecl("I", "i"), FieldDecl("I", "j")],
                [Method("V", "<init>", [],
                        Block([
                            LocalVarDecl("I", "k"),
                            LocalVarDecl("I", "l"),
                            StmtExprStmt(Assign(LocalOrFieldVar("k"), LocalOrFieldVar("i"))), 
                            StmtExprStmt(Assign(LocalOrFieldVar("l"), LocalOrFieldVar("j")))
                        ]), False),
                Method("Z", "gretMethod", [],
                        Block([
                            LocalVarDecl("I", "k"),
                            LocalVarDecl("I", "l"),
                            StmtExprStmt(Assign(LocalOrFieldVar("k"), LocalOrFieldVar("i"))), 
                            StmtExprStmt(Assign(LocalOrFieldVar("l"), LocalOrFieldVar("j"))),
                            If(Binary(">", LocalOrFieldVar("k"), LocalOrFieldVar("l")), 
                                Return(Bool(True)),
                                Just(Return(Bool False)))
                            ]), True)]
                )]
                 
--Test class to evaluate whether one of two integer has a lesser value than the other
lestClass = [Class("LestTest",
                [FieldDecl("I", "i"), FieldDecl("I", "j")],
                [Method("V", "<init>", [], 
                        Block([
                            LocalVarDecl("I", "k"),
                            LocalVarDecl("I", "l"),
                            StmtExprStmt(Assign(LocalOrFieldVar("k"), LocalOrFieldVar("i"))), 
                            StmtExprStmt(Assign(LocalOrFieldVar("l"), LocalOrFieldVar("j")))
                        ]), False),
                Method("Z", "lestMethod", [],
                        Block([
                            LocalVarDecl("I", "k"),
                            LocalVarDecl("I", "l"),
                            StmtExprStmt(Assign(LocalOrFieldVar("k"), LocalOrFieldVar("i"))), 
                            StmtExprStmt(Assign(LocalOrFieldVar("l"), LocalOrFieldVar("j"))),
                            If(Binary("<", LocalOrFieldVar("k"), LocalOrFieldVar("l")), 
                                Return(Bool(True)),
                                Just(Return(Bool False)))
                            ]), True)]
                )]
                 
whileClass = [Class("WhileTest",
                [FieldDecl("I", "x")],
                [Method("V", "<init>", [],
                    Block([
                        LocalVarDecl("I", "i"),
                        StmtExprStmt(Assign(LocalOrFieldVar("i"), Integer(0)))
                    ]), False),
                Method("I", "whileMethod",
                    [("I", "i")],
                    Block([
                        StmtExprStmt(Assign(LocalOrFieldVar("i"), Integer(0))),
                        While(Binary("<", LocalOrFieldVar("i"), LocalOrFieldVar("x")), 
                            Block([
                            StmtExprStmt(Assign(LocalOrFieldVar("i"), Binary("+", LocalOrFieldVar("i"), Integer(1))))
                                ])), 
                        Return(LocalOrFieldVar("i"))
                        ]),  True)]
                )]
                   


--Test program to calculate the n's Fibonacci number in either the positive or negative
fibNumClass = [head addClass, head subClass, Class("FibNumbers",
                    [], --FieldDecl("AddTest", "add"), FieldDecl("SubTest", "sub")
                    [Method("V", "<init>", [],
                        Block([
                              LocalVarDecl("I", "i"),
                              LocalVarDecl("I", "j"),
                              LocalVarDecl("I", "z"),
                              LocalVarDecl("Z", "b"),
                              StmtExprStmt(Assign(LocalOrFieldVar("i"), Integer(1))), 
                              StmtExprStmt(Assign(LocalOrFieldVar("j"), Integer(1))),
                              StmtExprStmt(Assign(LocalOrFieldVar("z"), Integer(6))),
                              StmtExprStmt(Assign(LocalOrFieldVar("b"), Bool(True))),
                              LocalVarDecl("I", "k"),
                              StmtExprStmt(Assign(LocalOrFieldVar("k"), Integer(0))), 
                              LocalVarDecl("I", "temp"),
                              StmtExprStmt(Assign(LocalOrFieldVar("temp"), LocalOrFieldVar("j")))
                              --StmtExprStmt(Assign(LocalOrFieldVar("add"), 
                              --    StmtExprExpr(New("AddTest", [])))),
                              --StmtExprStmt(Assign(LocalOrFieldVar("sub"), 
                              --    StmtExprExpr(New("SubTest", []))))
                        ]), False),
                    Method("I", "fibMethod",
                        [],
                        Block([
                            --Creation of used methods with new Statement
                            --StmtExprStmt(Assign(LocalOrFieldVar("add"), 
                            --    StmtExprExpr(New("AddTest", [])))),
                            --StmtExprStmt(Assign(LocalOrFieldVar("sub"), 
                            --    StmtExprExpr(New("SubTest", [])))),
                            --Creation of the local variables and subsequent assignment of values
                            LocalVarDecl("I", "i"), -- first Fibonacci number
                            LocalVarDecl("I", "j"), -- second Fibonacci number
                            LocalVarDecl("I", "z"), -- results in the calculation Fibonacci number f(x) where x=(z-2)
                            LocalVarDecl("Z", "b"), -- Determines whether the positive or negative Fibonacci number is calculated, where True means positive
                            StmtExprStmt(Assign(LocalOrFieldVar("i"), Integer(1))),
                            StmtExprStmt(Assign(LocalOrFieldVar("j"), Integer(1))),
                            StmtExprStmt(Assign(LocalOrFieldVar("z"), Integer(6))),
                            StmtExprStmt(Assign(LocalOrFieldVar("b"), Bool(True))),
                            --Determine whether a calculation will take place, e.g. the count variable z is greater 2, if not return 1
                            --If(StmtExprExpr(MethodCall(LocalOrFieldVar("eqBol"), "equBolMethod", [LocalOrFieldVar("b"), Bool(True)])),
                            If(Binary(">", LocalOrFieldVar("z"), Integer(2)),
                                Block([
                                    --Since we calculate, create and initiate the counting variable
                                    LocalVarDecl("I", "k"),
                                    StmtExprStmt(Assign(LocalOrFieldVar("k"), Integer(0))),
                                    --Next check whether we calculate a positive or negative Fibonacci number
                                    If(Binary("==", LocalOrFieldVar("b"), Bool(True)),
                                        --positive Fibonacci number calculation
                                        Block([
                                            While(Binary("<", LocalOrFieldVar("k"), LocalOrFieldVar("z")),
                                                Block([
                                                    LocalVarDecl("I", "temp"),
                                                    StmtExprStmt(Assign(LocalOrFieldVar("temp"), LocalOrFieldVar("j"))),
                                                    StmtExprStmt(Assign(LocalOrFieldVar("j"), LocalOrFieldVar("i"))),
                                                    StmtExprStmt(Assign(LocalOrFieldVar("i"),
                                                        StmtExprExpr(MethodCall(ClassId("AddTest"), "addMethod", 
                                                            [LocalOrFieldVar("i"), LocalOrFieldVar("temp")])))),
                                                    StmtExprStmt(Assign(LocalOrFieldVar("k"), Integer(1)))
                                                    ]))
                                            ]),
                                        --negative Fibonacci number calculation
                                        Just(Block([
                                                While(Binary("<", LocalOrFieldVar("k"), LocalOrFieldVar("z")),
                                                    Block([
                                                        LocalVarDecl("I", "temp"),
                                                        StmtExprStmt(Assign(LocalOrFieldVar("temp"), LocalOrFieldVar("j"))),
                                                        StmtExprStmt(Assign(LocalOrFieldVar("j"), LocalOrFieldVar("i"))),
                                                        StmtExprStmt(Assign(LocalOrFieldVar("i"),
                                                            StmtExprExpr(MethodCall(ClassId("SubTest"), "subMethod", 
                                                                [LocalOrFieldVar("temp"), LocalOrFieldVar("i")])))),
                                                        StmtExprStmt(Assign(LocalOrFieldVar("k"), Integer(1)))
                                                        ]))
                                            ]))),
                                --at the end of the calculation return the the result
                                Return(LocalOrFieldVar("i"))]),
                                --if no calculation was done, return 1
                                Just(Return(LocalOrFieldVar("i"))))
                        ]), True)]
                    )]
                    
main :: IO()
main = do
    putStrLn $ Pr.ppShow $ typecheck equIntClass
    putStrLn $ Pr.ppShow $ getConstantsHT (getMethodDeclsFromClass $ head $ typecheck equIntClass) 0
    putStrLn $ Pr.ppShow $ getConstantsCpEntries $ head $ typecheck equIntClass
    putStrLn $ Pr.ppShow $ get_CP_Map $ head $ typecheck equIntClass
    putStrLn $ Pr.ppShow $ get_CP_Infos $ head $ typecheck equIntClass
    --putStrLn $ Pr.ppShow $ codegen $ head $ typecheck equIntClass
    --putStrLn $ Pr.ppShow $ compIntClass
    --putStrLn $ Pr.ppShow $ codegen $ head $ typecheck compareIntClass
    putStrLn $ Pr.ppShow $ typecheck $ whileClass
    --putStrLn $ Pr.ppShow $ codegen $ head $ typecheck whileClass
    putStrLn $ Pr.ppShow $ typecheck $ voidClass
    --putStrLn $ Pr.ppShow $ codegen $ head $ typecheck voidClass
    putStrLn $ Pr.ppShow $ typecheck $ thisClass
    --putStrLn $ Pr.ppShow $ codegen $ head $ typecheck thisClass
    putStrLn $ Pr.ppShow $ typecheck $ addClass
    --putStrLn $ Pr.ppShow $ codegen $ head $ typecheck addClass
    putStrLn $ Pr.ppShow $ typecheck $ subClass
    --putStrLn $ Pr.ppShow $ codegen $ head $ typecheck subClass
    putStrLn $ Pr.ppShow $ typecheck $ multClass
    --putStrLn $ Pr.ppShow $ codegen $ head $ typecheck multClass
    putStrLn $ Pr.ppShow $ typecheck $ divClass
    --putStrLn $ Pr.ppShow $ codegen $ head $ typecheck divClass
    putStrLn $ Pr.ppShow $ typecheck $ equBolClass
    --putStrLn $ Pr.ppShow $ codegen $ head $ typecheck equBolClass
    putStrLn $ Pr.ppShow $ typecheck $ equIntClass
    --putStrLn $ Pr.ppShow $ codegen $ head $ typecheck equIntClass
    putStrLn $ Pr.ppShow $ typecheck $ gretClass
    --putStrLn $ Pr.ppShow $ codegen $ head $ typecheck gretClass
    putStrLn $ Pr.ppShow $ typecheck $ lestClass
    --putStrLn $ Pr.ppShow $ codegen $ head $ typecheck lestClass
    putStrLn $ Pr.ppShow $ typecheck $ fibNumClass
    --putStrLn $ Pr.ppShow $ codegen $ head $ typecheck fibNumClass
