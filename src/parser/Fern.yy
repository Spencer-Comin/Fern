%skeleton "lalr1.cc"
%require "3.0"
%debug
%defines
%define api.namespace {Fern}
%define api.parser.class {Parser}

%code requires{
    namespace Fern {
        class Driver;
        class Scanner;
    }

#ifndef YY_NULLPTR
#  if defined __cplusplus && 201103L <= __cplusplus
#    define YY_NULLPTR nullptr
#  else
#    define YY_NULLPTR 0
#  endif
#endif




#include "Operator.h"
#include "AST.h"

}

%parse-param { Scanner &scanner }
%parse-param { Driver &driver }

%code{
    #include <iostream>
    #include <cstdlib>
    #include <fstream>


    #include "Driver.h"

    #undef yylex
    #define yylex scanner.yylex
   
}

%define api.value.type variant

%token                  END     0   "end of file"

%token <Fern::Operator> PLUS
%token <Fern::Operator> MINUS

%token <Fern::Operator> AND
%token <Fern::Operator> OR
%token <Fern::Operator> XOR

%token <Fern::Operator> STAR
%token <Fern::Operator> SLASH
%token <Fern::Operator> MODULO

%token <Fern::Operator> TRIPLE_EQUAL
%token <Fern::Operator> DOUBLE_EQUAL
%token <Fern::Operator> BANG_EQUAL
%token <Fern::Operator> LT
%token <Fern::Operator> GT
%token <Fern::Operator> LT_EQUAL
%token <Fern::Operator> GT_EQUAL
%token <Fern::Operator> TILDE

%token <Fern::Operator> DOUBLE_AND
%token <Fern::Operator> DOUBLE_OR

%token <Fern::Operator> BANG

%token <std::string>    NUMBER
%token <std::string>    ID
%token <std::string>    STRING
%token <std::string>    TAG_LITERAL

%token  IF
%token  THEN
%token  ELSE
%token  WHILE
%token  DO
%token  OVER

%token  L_PAREN
%token  R_PAREN
%token  L_CURLY
%token  R_CURLY
%token  L_SQUARE
%token  R_SQUARE

%token  HASH
%token  COMMA
%token  COLON
%token  SEMICOLON
%token  QUESTION
%token  AT
%token  BACKSLASH

%token <Fern::Operator> EQUAL
%token <Fern::Operator> WALRUS

%type  <Fern::Operator> add_op
%type  <Fern::Operator> mul_op
%type  <Fern::Operator> logic_op
%type  <Fern::Operator> comp_op
%type  <Fern::Operator> bit_op
%type  <Fern::Operator> unary_op

%type  <Fern::ASTNode*> statement_list;
%type  <Fern::ASTNode*> expression;
%type  <Fern::ASTNode*> statement;
%type  <Fern::ASTNode*> literal;
%type  <Fern::ASTNode*> term;
%type  <Fern::ASTNode*> unary_exp;
%type  <Fern::ASTNode*> mul_exp;
%type  <Fern::ASTNode*> add_exp;
%type  <Fern::ASTNode*> comp_exp;
%type  <Fern::ASTNode*> logic_exp;
%type  <Fern::ASTNode*> statement_body;
%type  <Fern::ASTNode*> copy_stmt;
%type  <Fern::ASTNode*> bind_stmt;
%type  <Fern::ASTNode*> initialisation_stmt;
%type  <Fern::ASTNode*> id;
%type  <Fern::ASTNode*> declaration_stmt;

%type  <std::vector<std::string>> id_concatenation;
%type  <std::vector<std::string>> tag;


// %left AND OR XOR
// %left DOUBLE_AND DOUBLE_OR
// %left TRIPLE_EQUAL DOUBLE_EQUAL BANG_EQUAL LT_EQUAL GT_EQUAL LT GT TILDE
// %left PLUS MINUS
// %left STAR SLASH MODULO

%locations

%{
    extern int yylineno;
%}

%%

program:
    statement_list END
        {
            driver.setASTRoot($1);
        }
    ;

statement_list:
    statement_list statement
        {
            $$ = $1;
            $$->children.push_back($2);
        }
    | %empty
        {
            $$ = new Fern::ASTBranch();
        }
    ;

statement: statement_body SEMICOLON;

statement_body: expression | copy_stmt | bind_stmt | declaration_stmt | initialisation_stmt;

copy_stmt:
    id EQUAL expression
        {
            $$ = new Fern::Binary($1, $3, $2);
        }
    ;

bind_stmt:
    id WALRUS expression
        {
            $$ = new Fern::Binary($1, $3, $2);
        }
    ;

expression:
    logic_exp
    ;

id:
    ID
        {
            $$ = new Fern::ID($1);
        }
    ;

id_concatenation:
    id_concatenation COMMA ID
        {
            $$ = $1;
            $$.push_back($3);
        }
    | ID
        {
            $$ = std::vector<std::string>();
            $$.push_back($1);
        }
    | %empty
        {
            $$ = std::vector<std::string>();
        }
    ;

tag:
    HASH id_concatenation HASH
        {
            $$ = $2;
        }
    ;

declaration_stmt:
    tag id
        {
            $$ = $2;
            $$->setTags($1);
        }
    ;

initialisation_stmt:
    tag copy_stmt
        {
            $$ = $2;
            $$->setTags($1);
        }
    | tag bind_stmt
        {
            $$ = $2;
            $$->setTags($1);
        }
    ;

logic_exp:
    logic_exp logic_op comp_exp
        {
            $$ = new Fern::Binary($1, $3, $2);
        }
    | comp_exp
    ;

comp_exp:
    comp_exp comp_op add_exp
        {
            $$ = new Fern::Binary($1, $3, $2);
        }
    | add_exp
    ;

add_exp:
    add_exp add_op mul_exp
        {
            $$ = new Fern::Binary($1, $3, $2);
        }
    | mul_exp
    ;

mul_exp:
    mul_exp mul_op unary_exp
        {
            $$ = new Fern::Binary($1, $3, $2);
        }
    | unary_exp
    ;

unary_exp:
    unary_op unary_exp
        {
            $$ = new Fern::Unary($2, $1);
        }
    | term
    ;

term:
    L_PAREN expression R_PAREN
        {
            $$ = $2;
        }
    | id
    | literal
    ;

literal:
    STRING
        {
            $$ = new Fern::Literal($1, Fern::Literal::STRING);
        }
    | NUMBER
        {
            $$ = new Fern::Literal($1, Fern::Literal::NUMBER);
        }
    | TAG_LITERAL
        {
            $$ = new Fern::Literal($1, Fern::Literal::TAG_LITERAL);
        }
    ;

mul_op: STAR | SLASH | MODULO;

add_op: PLUS | MINUS;

comp_op: TRIPLE_EQUAL | DOUBLE_EQUAL | BANG_EQUAL | LT_EQUAL | GT_EQUAL | LT | GT | TILDE;

logic_op: DOUBLE_AND | DOUBLE_OR;

bit_op: AND | OR | XOR;

unary_op: PLUS | MINUS | TILDE | BANG;

%%

void Fern::Parser::error(const location_type &l, const std::string &err_message) {
    std::cerr << "Error: " << err_message << " at line " << scanner.lineno() << "\n";
}