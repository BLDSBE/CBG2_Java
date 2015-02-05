module TestSuite where

import Typecheck hiding (main)
import AbsSyn
import Constants
import Codegen hiding (main)
import qualified Text.Show.Pretty as Pr

ifClass = [Class("IfTest",
            [FieldDecl("I", "i"), FieldDecl("I", "j")],
            [Method("Z", "ifMethod", [],
                    Block([
                        StmtExprStmt(Assign(LocalOrFieldVar("i"), Integer(2))), 
                        StmtExprStmt(Assign(LocalOrFieldVar("j"), Integer(3))),
                        If(Binary("==", LocalOrFieldVar("i"), LocalOrFieldVar("j")), 
                            Return(Bool(True)),
                            Just(Return(Bool False)))
                        ]), True)]
                 )]

whileClass = [Class("WhileTest",
                [FieldDecl("I", "x")],
                [Method("I", "whileMethod",
                    [("I", "i")],
                    Block([
                        While(Binary("<", LocalOrFieldVar("i"), LocalOrFieldVar("x")), 
                            Block([
                                StmtExprStmt(Assign(LocalOrFieldVar("i"), Integer(1)))
                                ])), 
                            Return(LocalOrFieldVar("i"))
                        ]),  True)]
                )]
                   
compareClass = [Class("CompareTest",
                    [FieldDecl("IfTest", "a")],
                    [Method("Z", "compareMethod",
                        [],
                        Block([
                            StmtExprStmt(Assign(LocalOrFieldVar("a"), 
                                StmtExprExpr(New("IfTest", [Integer(3), Integer(2)])))),
                            Return (LocalOrFieldVar("a"))
                        ]), True)]
                    )]

main :: IO()
main = do
    putStrLn $ Pr.ppShow $ typecheck ifClass
    putStrLn $ Pr.ppShow $ getConstantsHT (getMethodDeclsFromClass $ head $ typecheck ifClass) 0
    putStrLn $ Pr.ppShow $ getConstantsCpEntries $ head $ typecheck ifClass
    putStrLn $ Pr.ppShow $ get_CP_Map $ head $ typecheck ifClass
    putStrLn $ Pr.ppShow $ get_CP_Infos $ head $ typecheck ifClass
    --putStrLn $ Pr.ppShow $ codegen $ head $ typecheck ifClass
    putStrLn $ Pr.ppShow $ compareClass
    --putStrLn $ Pr.ppShow $ codegen $ head $ typecheck compareClass
    putStrLn $ Pr.ppShow $ whileClass
    --putStrLn $ Pr.ppShow $ codegen $ head $ typecheck whileClass
    