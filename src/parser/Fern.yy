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

%token                  NEWLINE
%token <std::string>    ERROR

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
%token <std::string>    TAG
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

%token  EQUAL
%token  WALRUS
%token  COMMA
%token  COLON
%token  SEMICOLON
%token  QUESTION
%token  AT
%token  BACKSLASH

%type  <Fern::Operator> add_op
%type  <Fern::Operator> mul_op
%type  <Fern::Operator> logic_op
%type  <Fern::Operator> comp_op
%type  <Fern::Operator> bit_op
%type  <Fern::Operator> unary_op
%type  <Fern::Operator> operator

%locations

%{
    extern int yylineno;
%}

%%

list_option: END | list END;

list: item | list item;

item: 
    | operator  { driver.report("OPERATOR: " + Fern::opToString($1)); }
    | NUMBER    { driver.report("NUMBER: " + $1); }
    | ID        { driver.report("ID: " + $1); }
    | STRING    { driver.report("STRING: " + $1); }
    | TAG       { driver.report("TAG: " + $1); }
    | TAG_LITERAL   { driver.report("TAG_LITERAL: " + $1); }
    | IF        { driver.report("KEYWORD: IF"); }
    | THEN      { driver.report("KEYWORD: THEN"); }
    | ELSE      { driver.report("KEYWORD: ELSE"); }
    | WHILE     { driver.report("KEYWORD: WHILE"); }
    | DO        { driver.report("KEYWORD: DO"); }
    | OVER      { driver.report("KEYWORD: OVER"); }
    | special
    | utilities
    ;

operator: add_op | mul_op | logic_op | comp_op | bit_op | unary_op;

add_op: PLUS | MINUS;

mul_op: STAR | SLASH | MODULO;

logic_op: DOUBLE_AND | DOUBLE_OR;

comp_op: TRIPLE_EQUAL | DOUBLE_EQUAL | BANG_EQUAL | LT_EQUAL | GT_EQUAL | LT | GT | TILDE;

bit_op: AND | OR | XOR;

unary_op: BANG;

special:
    L_PAREN     { driver.report("L_PAREN"); }
    | R_PAREN   { driver.report("R_PAREN"); }
    | L_CURLY   { driver.report("L_CURLY"); }
    | R_CURLY   { driver.report("R_CURLY"); }
    | L_SQUARE  { driver.report("L_SQUARE"); }
    | R_SQUARE  { driver.report("R_SQUARE"); }
    | WALRUS    { driver.report("WALRUS"); }
    | EQUAL     { driver.report("EQUAL"); }
    | COMMA     { driver.report("COMMA"); }
    | COLON     { driver.report("COLON"); }
    | SEMICOLON { driver.report("SEMICOLON"); }
    | QUESTION  { driver.report("QUESTION"); }
    | AT        { driver.report("AT"); }
    | BACKSLASH { driver.report("BACKSLASH"); }
    ;


utilities:
    | ERROR     { driver.report_error($1); }
    ;

%%

void Fern::Parser::error(const location_type &l, const std::string &err_message) {
    std::cerr << "Error: " << err_message << " at " << l << "\n";
}