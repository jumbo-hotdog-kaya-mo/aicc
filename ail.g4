grammar ail;

WS : [ \t\f\r\n]+ -> skip;
LNCOMMENT : '/' '/' ~[\r\n]* ([\r\n] | EOF) -> skip;
BLKCOMMENT : '/' '*' .*? '*' '/' -> skip;
IDENT : [a-zA-Z_][a-zA-Z0-9_]*;

STRING: '"' (~'"' | '\\"')* '"'
      | '\'' (~'\'' | '\\\'')* '\'';
COLOR : HASH [0-9a-fA-F] [0-9a-fA-F] [0-9a-fA-F] [0-9a-fA-F] [0-9a-fA-F] [0-9a-fA-F];
NUMBER : [1-9][0-9]* (DOT [0-9]*)?
       | '0x' [0-9a-fA-F]+
       | '0' [0-7]*
       | '0b' [0-1]+;
BOOL : 'true'
     | 'false';

AND : '&';
CARET : '^';
COLON : ':';
COMMA : ',';
DOLLAR : '$';
DOT : '.';
EQUAL : '=';
EXCLAMATION : '!';
GLOBAL : 'global';
HASH : '#';
LBRACE : '{';
LBRACKET : '[';
LPAREN : '(';
MINUS : '-';
PERCENT : '%';
PIPE : '|';
PLUS : '+';
RBRACE : '}';
RBRACKET : ']';
RPAREN : ')';
SEMICOLON : ';';
SLASH : '/';
STAR : '*';
TILDE : '~';

main : (when | func_def | global_init)* EOF;

when : 'when' DOLLAR IDENT (COLON IDENT)? DOT IDENT block_stmt;
func_def : 'func' IDENT arglist (block_expr | block_stmt);
global_init : GLOBAL IDENT EQUAL expr SEMICOLON;

stmt : if_stmt
     | while_stmt
     | for_stmt
     | call_stmt
     | assign_stmt
	 | modify_stmt;

if_stmt : 'if' expr block_stmt ('else' 'if' expr block_stmt)* ('else' block_stmt)?;
while_stmt : 'while' expr block_stmt;
for_stmt : 'for'
           ( IDENT 'from' expr 'to' expr 'by' expr
	       | IDENT 'in' expr
	       | IDENT COMMA IDENT 'in' expr
		   )
		   block_stmt;
call_stmt : IDENT calllist SEMICOLON;
assign_stmt : assignlist block_stmt SEMICOLON;
modify_stmt : lvalue EQUAL expr SEMICOLON;

unary_op: PLUS | MINUS | EXCLAMATION;

binary_op: STAR | SLASH | PERCENT | PLUS | MINUS | AND | CARET | PIPE | '==' | '!=' | '<' | '>' | '<=' | '>=' | '&&' | '||';

expr : LPAREN expr RPAREN
     | unary_op expr
	 | expr binary_op expr
	 | block_expr
	 | call_expr
	 | assign_expr
     | '#(' expr COMMA expr COMMA expr (COMMA expr?)? RPAREN
     | LBRACKET (expr COMMA)* (expr COMMA?)? RBRACKET
     | LBRACE (expr COLON expr COMMA)* (expr COLON expr COMMA?)? RBRACE
     | rvalue
     | COLOR
     | STRING
     | MINUS? NUMBER
     | BOOL;

rvalue : DOLLAR IDENT COLON IDENT
       | DOLLAR IDENT
       | lvalue;

lvalue : IDENT
    | GLOBAL IDENT
    | DOLLAR IDENT COLON IDENT DOT IDENT;

assign_expr : assignlist block_expr;
call_expr : IDENT calllist;

block_stmt : block;
block_expr : block;

block : LBRACE stmt* expr? RBRACE;

assignlist : 'let' (IDENT EQUAL expr COMMA)* (IDENT EQUAL expr COMMA?)? 'in';
arglist : LPAREN (IDENT COMMA)* (IDENT COMMA?)? RPAREN;
calllist : LPAREN (expr COMMA)* (expr COMMA?)? RPAREN;

INVALID : .;
