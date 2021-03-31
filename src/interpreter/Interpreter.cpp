//
// Created by Spencer Comin on 2021-03-30.
//

#include "Interpreter.h"
#include <iostream>

void Fern::Interpreter::interpret(Fern::ASTNode *root) {
    std::cout << "\ninterpreting\n";
    root->accept(this);
}

void Fern::Interpreter::visitRoot(Fern::ASTNode *node) {
    std::cout << "interpreter visiting an ASTNode\n";
    visitAllChildren(node);
}

void Fern::Interpreter::visitConcatenation(Fern::Concatenation *node) {
    std::cout << "interpreter visiting a Concatenation\n";
    visitAllChildren(node);
}

void Fern::Interpreter::visitBlock(Fern::Block *node) {
    std::cout << "interpreter visiting a Block\n";
    visitAllChildren(node);
}

void Fern::Interpreter::visitBinary(Fern::Binary *node) {
    std::cout << "interpreter visiting a Binary\n";
    visitAllChildren(node);
}

void Fern::Interpreter::visitTernary(Fern::Ternary *node) {
    std::cout << "interpreter visiting a Ternary\n";
    visitAllChildren(node);
}

void Fern::Interpreter::visitUnary(Fern::Unary *node) {
    std::cout << "interpreter visiting a Unary\n";
    visitAllChildren(node);
}

void Fern::Interpreter::visitLiteral(Fern::Literal *node) {
    std::cout << "interpreter visiting a Literal\n";
    visitAllChildren(node);
}

void Fern::Interpreter::visitID(Fern::ID *node) {
    std::cout << "interpreter visiting an ID\n";
    visitAllChildren(node);
}
