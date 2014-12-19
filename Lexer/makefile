default:
	rm -f Lexer.hs
	alex Lexer.x
	happy JavaParser.y
	ghc -o testJavaParser Lexer.hs JavaParser.hs
clean:
	rm -f JavaParser.hs
	rm -f Lexer.hs
	rm -f *.hi
	rm -f *.o
	rm -f testJavaParser