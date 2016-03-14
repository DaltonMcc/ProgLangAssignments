%{
  open Types
%}

%token <float> FLOAT
%token TRUE FALSE
%token IF THEN ELSE
%token DBLSEMI
%nonassoc FLOAT
%nonassoc ELSE

%start main
%type <Types.exprS> main
%%

main:
  | headEx DBLSEMI               { $1 }
;

headEx:
  | expr                         { $1 }

;

expr:
  | FLOAT                        { NumS $1 }
  | TRUE						 { BoolS true }
  | FALSE 						 { BoolS false }
  | IF expr THEN expr ELSE expr	 { IfS ($2, $4, $6)}
;

