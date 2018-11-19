%{
#include <stdio.h>
#include"projet.tab.h"
extern YYSTYPE yylval; 

int l=1;
int c=1;
int i=0;

%}

UnInt 0|[1-9][0-9]*
idf [a-z]|[a-z][a-z0-9_]*[a-z0-9]
Unfloat {UnInt}+"."{UnInt}
COM1 "//"(.)*
COM2 [/][*][^*]*[*]+([^*/][^*]*[*]+)*[/]
%%

{COM1} c+=yyleng ;

{COM2} { for(i = 0 ; i < yyleng ; i++) { if (yytext[i] == '\n' ) { l++; c=1; } else c++; } }

"{" { c+=yyleng; return '{'; }

"Uint" { c+=yyleng; return Uint; }

"Ufloat" { c+=yyleng; return Ufloat; }

"define" { c+=yyleng; return define;}

"}" { c=+yyleng; return '}'; }

"CHECK" { c+=yyleng; return CHECK; }

"ENDCHECK" { c+=yyleng; return ENDCHECK; }

"REPEAT" { c+=yyleng; return REPEAT; }

"ENDREPEAT" { c+=yyleng; return ENDREPEAT; }

"(" { c+=yyleng; return '('; }

")" { c+=yyleng; return ')'; }

"+" { c+=yyleng; yylval.chaine = strdup(yytext) ; return ADD; }

"-" { c+=yyleng; yylval.chaine = strdup(yytext) ; return SUB; }

"*" { c+=yyleng; yylval.chaine = strdup(yytext) ; return MUL; }

"/" { c+=yyleng; yylval.chaine = strdup(yytext) ; return DIV; }

"\n" {l++; c=1;}

"\r" {l++; c=1;}

"\t" c=c+8;
		
" " c++;
			
";" { c+=yyleng; return ';'; }

"," { c+=yyleng; return ','; }

"<=" { c+=yyleng; return LE; }

"<" { c+=yyleng; return L; }

">=" { c+=yyleng; return GE;  }

">" { c+=yyleng; return G;}

"==" { c+=yyleng; return EQ; }

"=" { c+=yyleng; yylval.chaine = strdup(yytext); return AFF ;}

"!=" { c+=yyleng; return DEFF; }

"!" { c+=yyleng; return NOT; }

":" { c+=yyleng; return ':' ;}

"&" { c+=yyleng; return AND ; }

"|" { c+=yyleng; return OR ; }

{idf} { if(yyleng <= 8) {c+=yyleng; yylval.chaine = strdup(yytext) ;return idf ;} else {printf("%d : %d : Error : identificator too long\n",l,c);}}

{UnInt} {c+=yyleng; yylval.entier = atoi(yytext); if(yylval.entier < 32567) return Vint ;else {printf("%d : %d : Error : number too big\n",l,c);}}

{Unfloat} {c+=yyleng; yylval.reel = atof(yytext);return Vfloat ;}

. { printf("\n%d:%d : %s : caused lexical error\n",l,c,yytext); }
%%

int yywrap(void)
{
return 0;
}

