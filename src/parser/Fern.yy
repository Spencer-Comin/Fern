%skeleton "lalr1.cc"
%require "3.0"
%verbose
%defines
%define api.namespace {Fern}
%define api.parser.class {Parser}
%define parse.trace {true}

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
    #include "types.h"

    #undef yylex
    #define yylex scanner.yylex

    template<typename Base, typename T> inline bool isinstance(const T *ptr);
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
%token  BACKSLASH

%token <Fern::Operator> DOT
%token <Fern::Operator> EQUAL
%token <Fern::Operator> WALRUS

%type  <Fern::Operator> add_op
%type  <Fern::Operator> mul_op
%type  <Fern::Operator> logic_op
%type  <Fern::Operator> comp_op
%type  <Fern::Operator> bit_op
%type  <Fern::Operator> unary_op
%type  <Fern::Operator> assign_op

%type  <Fern::ASTNode*> statement_list;
%type  <Fern::ASTNode*> expression;
%type  <Fern::ASTNode*> expression_body;
%type  <Fern::ASTNode*> statement;
%type  <Fern::ASTNode*> literal;
%type  <Fern::ASTNode*> term;
%type  <Fern::ASTNode*> unary_exp;
%type  <Fern::ASTNode*> mul_exp;
%type  <Fern::ASTNode*> add_exp;
%type  <Fern::ASTNode*> bit_exp;
%type  <Fern::ASTNode*> comp_exp;
%type  <Fern::ASTNode*> logic_exp;
%type  <Fern::ASTNode*> index_exp;
%type  <Fern::ASTNode*> concatenation_exp;
%type  <Fern::ASTNode*> evaluation_exp;
%type  <Fern::ASTNode*> decision_exp;
%type  <Fern::ASTNode*> iteration_exp;
%type  <Fern::ASTNode*> visit_exp;
%type  <Fern::ASTNode*> slice_exp;
%type  <Fern::ASTNode*> statement_body;
%type  <Fern::ASTNode*> assign_stmt;
%type  <Fern::ASTNode*> assign_target;
%type  <Fern::ASTNode*> id;
%type  <Fern::ASTNode*> block;
%type  <Fern::ASTNode*> basic_block;
%type  <Fern::ASTNode*> conditional_block;

%type  <std::set<std::string>> tag_list;
%type  <std::set<std::string>> tag;


// %left AND OR XOR
// %left DOUBLE_AND DOUBLE_OR
// %left TRIPLE_EQUAL DOUBLE_EQUAL BANG_EQUAL LT_EQUAL GT_EQUAL LT GT TILDE
// %left PLUS MINUS
// %left STAR SLASH MODULO

%left PLUS MINUS
%left EQUAL WALRUS
%left L_PAREN R_PAREN
%left DOT L_SQUARE
%left COMMA
%left THEN QUESTION WHILE
%left ELSE COLON DO


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
            $$->addChild($2);
        }
    | %empty
        {
            $$ = new Fern::ASTNode();
        }
    ;

statement: statement_body SEMICOLON;

statement_body: expression | assign_stmt;

assign_stmt:
    assign_target assign_op expression
        {
            $$ = new Fern::Binary($1, $3, $2);
        }
    | tag assign_target assign_op expression
        {
            $$ = new Fern::Binary($2, $4, $3);
            $$->addTags($1);
        }
    ;

assign_target: id | index_exp;

assign_op: EQUAL | WALRUS;

expression_body:
    logic_exp
    | iteration_exp
    | visit_exp
    ;

expression:
    expression_body
    | concatenation_exp
    | decision_exp
    | tag expression_body
        {
            $$ = $2;
            $$->addTags($1);
        }
    ;

iteration_exp:
    WHILE expression DO expression
        {
            $$ = new Fern::Binary($2, $4, Fern::Operator::ITERATION);
        }
    ;

visit_exp:
    block OVER block
        {
            $$ = new Fern::Binary($1, $3, Fern::Operator::VISIT);
        }
    | block BACKSLASH block
        {
            $$ = new Fern::Binary($1, $3, Fern::Operator::VISIT);
        }
    ;

slice_exp:
    block L_SQUARE expression COLON expression R_SQUARE
        {
            $$ = new Fern::Ternary($1, $3, $5, Fern::Ternary::TernaryType::SLICE);
        }
    ;

decision_exp:
    IF expression THEN expression
        {
            $$ = new Fern::Binary($2, $4, Fern::Operator::DECISION);
        }
    | IF expression THEN expression ELSE expression
        {
            $$ = new Fern::Ternary($2, $4, $6, Fern::Ternary::TernaryType::DECISION);
        }
    | expression QUESTION expression
        {
            $$ = new Fern::Binary($1, $3, Fern::Operator::DECISION);
        }
    | expression QUESTION expression COLON expression
        {
            $$ = new Fern::Ternary($1, $3, $5, Fern::Ternary::TernaryType::DECISION);
        }
    ;

index_exp:
    block L_SQUARE expression R_SQUARE
        {
            $$ = new Fern::Binary($1, $3, Fern::Operator::DOT);
        }
    | block DOT block
        {
            $$ = new Fern::Binary($1, $3, $2);
        }
    ;

id:
    ID
        {
            $$ = new Fern::ID($1);
        }
    ;

basic_block:
    L_CURLY statement_list R_CURLY
        {
            $$ = new Block($2);
        }
    ;

term:
    L_PAREN expression R_PAREN
        {
            $$ = $2;
            $$->parenthesized = true;
        }
    | block
    ;

conditional_block:
    L_SQUARE tag_list R_SQUARE basic_block
        {
            $$ = $4;
            $$->setConditions($2);
        }
    ;

block:
    basic_block
    | conditional_block
    | id
    | literal
    | index_exp
    | slice_exp
    | evaluation_exp
    ;

tag_list:
    tag_list COMMA ID
        {
            $$ = $1;
            $$.insert($3);
        }
    | ID
        {
            $$ = std::set<std::string>();
            $$.insert($1);
        }
    ;

evaluation_exp:
    block L_PAREN expression R_PAREN
        {
            $$ = $1;
            $$->addEvaluationList($3);
        }
    | block L_PAREN R_PAREN
        {
            $$ = $1;
            $$->addEvaluationList(nullptr);
        }
    ;

concatenation_exp:
    expression COMMA expression
        {
            if (isinstance<Concatenation>($1) && !$1->parenthesized) {
                $$ = $1;
                $$->addChild($3);
            } else {
                $$ = new Concatenation();
                $$->addChild($1);
                $$->addChild($3);
            }
        }
    ;


tag:
    HASH tag_list HASH
        {
            $$ = $2;
        }
    | HASH HASH
        {
            $$ = std::set<std::string>();
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
    comp_exp comp_op bit_exp
        {
            $$ = new Fern::Binary($1, $3, $2);
        }
    | bit_exp
    ;

bit_exp:
    bit_exp bit_op add_exp
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


literal:
    STRING
        {
            Fern::FernType s {$1};
            $$ = new Fern::Literal(s);
        }
    | NUMBER
        {
            Fern::FernType n {std::stoi($1)};
            $$ = new Fern::Literal(n);
        }
    | TAG_LITERAL
        {
            Fern::FernType t{Fern::TagType($1)};
            $$ = new Fern::Literal(t);
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

template<typename Base, typename T>
inline bool isinstance(const T *ptr) {
    // https://stackoverflow.com/a/25231384
    return dynamic_cast<const Base*>(ptr) != nullptr;
}