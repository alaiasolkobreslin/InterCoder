#lang brag

expr: NUM
| LPAREN expr RPAREN
| LPAREN PLUS expr expr RPAREN
| LPAREN MINUS expr expr RPAREN
| LPAREN TIMES expr expr RPAREN
