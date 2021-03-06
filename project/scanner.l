%{
  #include "y.tab.h"
  #include <stdlib.h>
  #include "utils.h"
  #include <llvm-c/Core.h>

  void yyerror(LLVMModuleRef module, LLVMBuilderRef builder, const char* s);
%}

DIGIT    [0-9]
IDE       [A-Za-z][A-Za-z0-9]*

%option noyywrap
%%

if                 { return IF; }
else               { return ELSE; }
while              { return WHILE; }
print              { return PRINT; }
int                { return INT_TYPE; }
bool               { return BOOL_TYPE; }
true               { return TRUE; }
false              { return FALSE; }
for                { return FOR; }
{DIGIT}+           { yylval.value = atoi(yytext); return VAL; }
{IDE}               { yylval.id = string_int_get(&global_ids, yytext); return IDE; }
[ \t\r\n]+         /* discard whitespace */
[-*/+><=,;:\{\}\(\)\[\]] { return *yytext; }
\>=                { return GE; }
\<=                { return LE; }
==                 { return EQ; }
!=                 { return NE; }
.                  yyerror(NULL, NULL, "Unexpected character");

%%
