all: lexer.l parser.y
	bison -d -Wcounterexamples parser.y
	flex lexer.l
	cc parser.tab.c lex.yy.c -lfl

clean:
	-rm -f *.o
	-rm -f a.out
	-rm -f lex.yy.c
	-rm -f parser.tab.h parser.tab.c
