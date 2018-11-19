//Alim Yanis && Mellal Hodaifa

%{
#include<stdio.h>
#include<stdlib.h>
#include<string.h>
#include"ListesChainées.h"
#include"Quad.h"

int yylex();
int yyerror(char *);
extern FILE* yyin ;
extern int yyleng ;
extern int l ;
extern int c ;
extern char* yytext ;

int Save=0,QC=0,cpt=1,Not=0,InRep=0,AdrFin=0,AdrInc=0,AdrInstr=0,ind=0;

char *T , *DebInstr , *DebCond , *AdrIncS, *Temp;
char Valeur[254] = { } ;

Element* TS=NULL;
Quad* Qdr=NULL;
Pile* reP=NULL;
%}
 
%union
{
char *chaine ;
int entier ;
float reel;

struct 	{
int Type;
char* Valeur;	
	} TV;

struct {
char *Op , *Op1 , *Op2 ;
int used,Not;
	} Qd;
}

%token CHECK ENDCHECK REPEAT ENDREPEAT define NOT AND OR Uint Ufloat LE L GE G EQ DEFF

%token <chaine> idf AFF ADD SUB MUL DIV 
%token <entier> Vint
%token <reel> Vfloat

%type <Qd> A B C OneC
%type <TV> E T F NB Val 
%type <chaine> opL

%left OR
%left AND
%left NOT
%left LE L GE G EQ DEFF
%left SUB ADD
%left MUL DIV
%left '(' ')'

%start S

%%

S : idf '{' D '{' Intrs '}' '}' { printf("Program compiled with no errors.\n"); return 0; };

D : D Dec | Dec ;

Dec : Var | Const ;

Var : Type List ';' ;

Type : Uint {Save=1;} | Ufloat {Save=0;};

List :      idf {if (!RechercheElt(TS ,$1)) InsertElt(&TS,$1, Save, 1);else {printf("%d:%d : %s : redeclared\n",l,c-yylen,$1);exit(1);}} 
| idf ','  List {if (!RechercheElt(TS ,$1)) InsertElt(&TS,$1, Save, 1);else {printf("%d:%d : %s : redeclared\n",l,c-yylen,$1);exit(1);}} ;

Const : define Type idf AFF E ';'{if (!RechercheElt(TS ,$3)) InsertElt(&TS,$3,Save, 0);
				  else { printf("%d:%d : %s : redeclared\n",l,c-yylen,yytext-yylen);exit(1);} 
				  if(Save!=$5.Type){printf("%d:%d : %s : Uncompatibility of types detected\n",l,c-yylen,yytext-yylen);exit(1);}
				  T=strdup($3); 
      				  InsertQuad(&Qdr,$4,$5.Valeur," ",T,QC);
				  QC++;	
				   };

Val :  Vint   {$$.Type=1; sprintf(Valeur,"%d",$1); $$.Valeur = strdup(Valeur);}
     | Vfloat {$$.Type=0; sprintf(Valeur,"%f",$1); $$.Valeur = strdup(Valeur);};

Intrs : I | Intrs I ;

I : Aff | Op | Loop ;

Aff : idf {if(!RechercheElt(TS,$1)) {printf("%d:%d : %s : undeclared\n",l,c-yylen,$1); }
           else { if (RechercheNat(TS,$1) == 0) {printf("%d:%d : %s : constants cannot be modified\n",l,c-yylen,yytext-yylen);exit(1);}} } 
      AFF E ';' { if( RechercheType(TS,$1) != $4.Type ) {printf("%d:%d : %s : Uncompatibility of types detected\n",l,c-yylen,yytext-yylen);exit(1);}
      		T=strdup($1); 
      		InsertQuad(&Qdr,$3,$4.Valeur," ",T,QC);	
      		QC++;	
	   	};

E : E ADD T {if ($1.Type != $3.Type ) {printf("%d:%d : %s : Uncompatibility of types detected\n",l,c-yylen,yytext-yylen);exit(1);}
			    sprintf(Valeur,"T%d",cpt); T = strdup(Valeur); 
			    InsertQuad(&Qdr,$2,$1.Valeur,$3.Valeur,T,QC); $$.Valeur = strdup(T);
			    cpt++; QC++;     
	    }
  | E SUB T {if( $1.Type != $3.Type ) {printf("%d:%d : %s : Uncompatibility of types detected\n",l,c-yylen,yytext-yylen);exit(1);}
			     sprintf(Valeur,"T%d",cpt); T = strdup(Valeur); 
			     InsertQuad(&Qdr,$2,$1.Valeur,$3.Valeur,T,QC);$$.Valeur = strdup(T);
			     cpt++; QC++;
	     }
  | T {$$.Type=$1.Type;$$.Valeur=strdup($1.Valeur);} ;

T : T MUL F {if ($1.Type != $3.Type ) {printf("%d:%d : %s : Uncompatibility of types detected\n",l,c-yylen,yytext-yylen);exit(1);}
			     sprintf(Valeur,"T%d",cpt); T = strdup(Valeur); 
			     InsertQuad(&Qdr,$2,$1.Valeur,$3.Valeur,T,QC);$$.Valeur = strdup(T);
			     cpt++; QC++;
	    }
  | T DIV F {if ($1.Type != $3.Type ) {printf("%d:%d : %s : Uncompatibility of types detected\n",l,c-yylen,yytext-yylen);exit(1);}
			     sprintf(Valeur,"T%d",cpt); T = strdup(Valeur); 
			     InsertQuad(&Qdr,$2,$1.Valeur,$3.Valeur,T,QC);$$.Valeur = strdup(T);
			     cpt++; QC++;
	     }
  | F {$$.Type=$1.Type;$$.Valeur=strdup($1.Valeur);} ;

F :'('E')' {$$.Type=$2.Type; $$.Valeur=strdup($2.Valeur);} | NB {$$.Type=$1.Type; $$.Valeur=strdup($1.Valeur);};

NB : Val {$$.Type=$1.Type;$$.Valeur=strdup($1.Valeur);}

   | idf { if(!RechercheElt(TS,$1)) {printf("%d:%d : %s : undeclared\n",l,c-yylen,$1);exit(1);}
           $$.Type=RechercheType(TS,$1);
	   $$.Valeur=strdup($1);	
	  };

Op : CHECK '(' {ind++;} 
	      A { if(! $4.used) {if($4.Not == 0 ) {sprintf(Valeur,"Else%d",ind);Temp=strdup(Valeur);
						   InsertQuad(&Qdr,$4.Op,Temp,$4.Op1,$4.Op2,QC); }
		  		  else {sprintf(Valeur,"Else%d",ind);Temp=strdup(Valeur);
					InsertQuad(&Qdr,Reverse($4.Op),Temp,$4.Op1,$4.Op2,QC);} 
				  QC++; } }
               ')' ':' {sprintf(Valeur,"Then%d",ind);Temp=strdup(Valeur);Chainage(&Qdr,Temp,QC);} 
     		      Instr ENDCHECK ;

A : A OR B { if(! $1.used) { if(Not == 0) { if ($1.Not == 1) {InsertQuad(&Qdr,$1.Op,"Then",$1.Op1,$1.Op2,QC);$1.Not=0;}
			     else InsertQuad(&Qdr,Reverse($1.Op),"Then",$1.Op1,$1.Op2,QC);}
			     else {InsertQuad(&Qdr,Reverse($1.Op),"Else",$1.Op1,$1.Op2,QC);}
			     QC++; $$.used=1; }
				
	     if(! $3.used) { if(Not == 0) {if ($3.Not == 1) {InsertQuad(&Qdr,Reverse($3.Op),"Else",$3.Op1,$3.Op2,QC); $3.Not = 0;} 
			     else {InsertQuad(&Qdr,$3.Op,"Else",$3.Op1,$3.Op2,QC);}}
			     else {InsertQuad(&Qdr,Reverse($3.Op),"Else",$3.Op1,$3.Op2,QC);} 			
			     QC++; $3.used=1; }
           }
    | B { $$.Op = strdup($1.Op); $$.Op1 = strdup($1.Op1); $$.Op2 = strdup($1.Op2);};

B : B  AND C { 
	if(! $1.used) { if(Not == 0) {if($1.Not ==1) {InsertQuad(&Qdr,Reverse($1.Op),"Else",$1.Op1,$1.Op2,QC);$1.Not = 0;}
			else {InsertQuad(&Qdr,$1.Op,"Else",$1.Op1,$1.Op2,QC);}}
			else InsertQuad(&Qdr,$1.Op,"Then",$1.Op1,$1.Op2,QC); QC++; $$.used=1; }
       	if(! $3.used) { if(Not == 0) {if ($3.Not == 1) {InsertQuad(&Qdr,Reverse($3.Op),"Else",$3.Op1,$3.Op2,QC);$3.Not = 0;} 
			else InsertQuad(&Qdr,$3.Op,"Else",$3.Op1,$3.Op2,QC);}			
			else InsertQuad(&Qdr,Reverse($3.Op),"Else",$3.Op1,$3.Op2,QC);
			QC++; $3.used=1; }
       	     }
    | C {$$.Op = strdup($1.Op); $$.Op1 = strdup($1.Op1);$$.Not=$1.Not;$$.Op2 = strdup($1.Op2);$$.used = $1.used;};

C : NOT 	  { Not=1; }
       '(' A ')'  {$$.Op=strdup($4.Op);$$.Op1 = strdup($4.Op1); $$.Op2 = strdup($4.Op2);$$.used = $4.used;$$.Not=$4.Not; 
		  if ($4.used==0) {$$.Not =1;}
		  Not=0; }
   | '(' A ')' 	{$$.Op=strdup($2.Op); $$.Op1 = strdup($2.Op1); $$.Op2 = strdup($2.Op2);$$.used = $2.used;}
   | OneC 	{$$.Op=strdup($1.Op); $$.Op1 = strdup($1.Op1); $$.Op2 = strdup($1.Op2);$$.used = $1.used;};

OneC : E opL E { if($1.Type != $3.Type) {printf("%d:%d : %s : Uncompatibility of types detected\n",l,c-yylen,yytext-yylen);exit(1);}
		$$.Op = strdup($2);
		$$.Op1 = strdup($1.Valeur);
		$$.Op2 = strdup($3.Valeur);
		$$.used = 0;
 		$$.Not = 0;
		};

opL : GE  {$$=strdup("BL"); }
    | G   {$$=strdup("BLE");}
    | LE  {$$=strdup("BG"); }
    | L   {$$=strdup("BGE");} 
    | EQ  {$$=strdup("BNE");}
    | DEFF{$$=strdup("BE"); };

Instr :  Intrs 	   {sprintf(Valeur,"Else%d",ind);Temp=strdup(Valeur);Chainage(&Qdr,Temp,QC);ind--;}
       | Intrs ':' {sprintf(Valeur,"Fin%d",ind); Temp=strdup(Valeur);InsertQuad(&Qdr,"BR",Temp," "," ",QC);QC++;
		    sprintf(Valeur,"Else%d",ind);Temp=strdup(Valeur);Chainage(&Qdr,Temp,QC);} Intrs 
		   {sprintf(Valeur,"Fin%d",ind);Temp=strdup(Valeur);Chainage(&Qdr,Temp,QC);ind--;} ;

Loop : REPEAT Body ENDREPEAT 
       {sprintf(Valeur,"%d",Depiler(&reP));AdrIncS=strdup(Valeur);InsertQuad(&Qdr,"BR",AdrIncS," "," ",QC); QC++; MaJ(&Qdr,Depiler(&reP),QC); };

Body : ':' Af ':' '(' OneC  {InsertQuad(&Qdr,$5.Op," ",$5.Op1,$5.Op2,QC);Empiler(&reP,QC);sprintf(Valeur,"%d",QC); DebCond=strdup(Valeur);QC++; 
			     InsertQuad(&Qdr,"BR"," "," "," ",QC);AdrInstr=QC;QC++;Empiler(&reP,QC);  }   
			   ')' ':' Af ':' {MaJ(&Qdr,AdrInstr,QC+1);InsertQuad(&Qdr,"BR",DebCond," "," ",QC);QC++;} 
					  Intrs; 

Af : idf      {if(!RechercheElt(TS,$1)) {printf("%d:%d : %s : undeclared\n",l,c-yylen,$1); }
               else { if (RechercheNat(TS,$1) == 0) {printf("%d:%d : %s : constants cannot be modified\n",l,c-yylen,yytext-yylen);exit(1);}}} 
        AFF E {if( RechercheType(TS,$1) != $4.Type ) {printf("%d:%d : %s : Uncompatibility of types detected\n",l,c-yylen,yytext-yylen);exit(1);}
    	       T=strdup($1); 
      	       InsertQuad(&Qdr,$3,$4.Valeur," ",$1,QC);	
      	       QC++;};
%%

int yyerror(char* msg)
{
printf("%d:%d : %s caused %s\n",l,c-yyleng,yytext,msg);
	return 1;
}

	int main()	{
//yyin=fopen("test.txt","r");
yyparse();
AffichageElts(TS); 
AffichageQuad(Qdr);
return 0;		}
