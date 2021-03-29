%x MULTILINE_COMMENT
%{
#include <string>
#include <cstring>

#include "Scanner.h"
#include "Operator.h"
#undef YY_DECL
#define YY_DECL int Fern::Scanner::yylex(Fern::Parser::semantic_type* lval, Fern::Parser::location_type* location)

using token = Fern::Parser::token;

#define yyterminate() return token::END

#define YY_NO_UNISTD_H

/* update location on matching */
#define YY_USER_ACTION loc->step(); loc->columns(yyleng);

//#define ERROR(msg)  yyval->build<std::string>(msg); return token::ERROR
#define ERROR(msg)  std::string s(msg); Fern::Parser::syntax_error exc(*loc, s); throw exc
#define OP(op) yyval->build<Fern::Operator>(Fern::Operator::op); return token::op

static int comment_nesting = 0;

%}

%option debug
%option nodefault
%option yyclass="Fern::Scanner"
%option noyywrap
%option c++
%option yylineno

number      [-+]?[0-9_]+
id          [a-zA-Z][a-zA-Z0-9_]*
string      \"[^"\n]*\"
tag_lit     "`"{id}

comment     "//"[^\n]*
whitespace  [ \t\r]+

/* bad patterns */
bad_string  \"[^"\n]*

%%

%{
    yyval = lval;
%}

{whitespace}    { /* ignore */ }
{comment}       { /* ignore */ }

<INITIAL>"/*"   { BEGIN(MULTILINE_COMMENT); comment_nesting++; }

<MULTILINE_COMMENT>"/*"     { comment_nesting++; }
<MULTILINE_COMMENT>"*/"     { comment_nesting--; if (comment_nesting == 0) BEGIN(INITIAL); }
<MULTILINE_COMMENT><<EOF>>  { ERROR("EOF in multiline comment"); }
<MULTILINE_COMMENT>.        { /* ignore */ }


"if"        { return token::IF; }
"then"      { return token::THEN; }
"else"      { return token::ELSE; }
"while"     { return token::WHILE; }
"do"        { return token::DO; }
"over"      { return token::OVER; }

{number}    { yyval->build<std::string>(yytext); return token::NUMBER; }
{id}        { yyval->build<std::string>(yytext); return token::ID; }
{string}    {
                std::string val(yytext);
                // trim opening and closing quotes
                val = val.substr(1, val.size() - 2);
                yyval->build<std::string>(val);
                return token::STRING;
            }
{tag_lit}   {
                std::string val(yytext);
                // trim opening backtick
                val.erase(0, 1);
                yyval->build<std::string>(val);
                return token::TAG_LITERAL;
            }

"+"         { OP(PLUS); }
"-"         { OP(MINUS); }

"*"         { OP(STAR); }
"/"         { OP(SLASH); }
"%"         { OP(MODULO); }

"&&"        { OP(DOUBLE_AND); }
"||"        { OP(DOUBLE_OR); }

"==="       { OP(TRIPLE_EQUAL); }
"=="        { OP(DOUBLE_EQUAL); }
"!="        { OP(BANG_EQUAL); }
"<="        { OP(LT_EQUAL); }
">="        { OP(GT_EQUAL); }
"<"         { OP(LT); }
">"         { OP(GT); }
"~"         { OP(TILDE); }

"&"         { OP(AND); }
"|"         { OP(OR); }
"^"         { OP(XOR); }

"!"         { OP(BANG); }

":="        { OP(WALRUS); }
"="         { OP(EQUAL); }
"."         { OP(DOT); }

"("         { return token::L_PAREN; }
")"         { return token::R_PAREN; }
"{"         { return token::L_CURLY; }
"}"         { return token::R_CURLY; }
"["         { return token::L_SQUARE; }
"]"         { return token::R_SQUARE; }

"#"         { return token::HASH; }
","         { return token::COMMA; }
":"         { return token::COLON; }
";"         { return token::SEMICOLON; }
"?"         { return token::QUESTION; }
"\\"        { return token::BACKSLASH; }



<*>\n       { loc->lines(); }

{bad_string} { ERROR("no closing \" in string"); }
.            { ERROR(yytext); }

%%
