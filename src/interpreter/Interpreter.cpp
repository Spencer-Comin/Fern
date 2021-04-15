//
// Created by Spencer Comin on 2021-03-30.
//

#include "Interpreter.h"
#include "errors.h"
#include <iostream>

void Fern::Interpreter::interpret(Fern::ASTNode *root) {
    std::cout << "\ninterpreting\n";
    root->accept(this);
}

void Fern::Interpreter::visitRoot(Fern::ASTNode *node) {
    visitAllChildren(node);
    std::cout << "interpreter visiting a Root\n";
}

void Fern::Interpreter::visitConcatenation(Fern::Concatenation *node) {
    visitAllChildren(node);
    std::cout << "interpreter visiting a Concatenation\n";
}

void Fern::Interpreter::visitBlock(Fern::Block *node) {
    visitAllChildren(node);
    std::cout << "interpreter visiting a Block\n";
}

void Fern::Interpreter::visitBinary(Fern::Binary *node) {
    visitAllChildren(node);
    std::cout << "interpreter visiting a Binary\n";
}

void Fern::Interpreter::visitTernary(Fern::Ternary *node) {
    visitAllChildren(node);
    std::cout << "interpreter visiting a Ternary\n";
}

void Fern::Interpreter::visitUnary(Fern::Unary *node) {
    node->children[0]->accept(this);
    // assuming that returnBucket contains a literal, not sure how to define otherwise atm
    if (returnBucket.tag != Reference::LITERAL) throw Fern::DebugError("Attempted to use a unary with non-literal returnBucket");
    switch (node->op) {
        case Operator::PLUS:
            returnBucket.literal = +returnBucket.literal;
            break;
        case Operator::MINUS:
            returnBucket.literal = -returnBucket.literal;
            break;
        case Operator::TILDE:
            returnBucket.literal = ~returnBucket.literal;
            break;
        case Operator::BANG:
            returnBucket.literal = !returnBucket.literal;
            break;
        default:
            break;
    }
    std::cout << "interpreter visiting a Unary\n";
}

void Fern::Interpreter::visitLiteral(Fern::Literal *node) {
    //visitAllChildren(node);
    Literal obj = *node;
    FernType value {obj};
    returnBucket = Fern::Reference(value);
    std::cout << "interpreter visiting a Literal: " << node->value << "\n";
}

void Fern::Interpreter::visitID(Fern::ID *node) {
    visitAllChildren(node);
    std::cout << "interpreter visiting an ID: " << node->name << "\n";
}

//Fern::Interpreter::Interpreter() {
//    std::cout << "making interpreter\n";
//}
