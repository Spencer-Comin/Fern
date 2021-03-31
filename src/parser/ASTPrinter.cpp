//
// Created by Spencer Comin on 2021-03-30.
//

#include "ASTPrinter.h"
#include <iostream>


void Fern::ASTPrinter::visitRoot(Fern::ASTNode *node) {
    output() << "Root\n";
    visitAllChildren(node);
}

void Fern::ASTPrinter::visitConcatenation(Fern::Concatenation *node) {
    output() << "Concatenation\n";
    visitAllChildren(node);
}

void Fern::ASTPrinter::visitBlock(Fern::Block *node) {
    output() << "Block\n";
    visitAllChildren(node);
}

void Fern::ASTPrinter::visitBinary(Fern::Binary *node) {
    output() << "Binary op: " << Fern::opToString(node->op) << '\n';
    visitAllChildren(node);
}

void Fern::ASTPrinter::visitTernary(Fern::Ternary *node) {
    string typeName{};
    switch (node->type) {
        case Ternary::TernaryType::DECISION:
            typeName = "DECISION";
            break;
        case Ternary::TernaryType::SLICE:
            typeName = "SLICE";
            break;
        case Ternary::TernaryType::REPLACE:
            typeName = "REPLACE";
            break;
    }
    output() << "Ternary op: " << typeName << '\n';
    visitAllChildren(node);
}

void Fern::ASTPrinter::visitUnary(Fern::Unary *node) {
    output() << "Unary op: " << Fern::opToString(node->op) << '\n';
    visitAllChildren(node);
}

void Fern::ASTPrinter::visitLiteral(Fern::Literal *node) {
    string typeName{};
    switch (node->type) {
        case Literal::Type::NUMBER:
            typeName = "Number";
            break;
        case Literal::Type::STRING:
            typeName = "String";
            break;
        case Literal::Type::TAG_LITERAL:
            typeName = "Tag";
            break;
    }
    output() << typeName << " literal: " << node->value << '\n';
    visitAllChildren(node);
}

void Fern::ASTPrinter::visitID(Fern::ID *node) {
    output() << "ID: " << node->name << '\n';
    visitAllChildren(node);
}

void Fern::ASTPrinter::print(Fern::ASTNode *root) {
    output() << "\nprinting\n";
    visitRoot(root);
}

void Fern::ASTPrinter::visitAllChildren(Fern::ASTNode *node) {
    indent++;
    for (auto const &condition: node->conditions) {
        output() << "[CONDITION]: " << condition << '\n';
    }
    for (auto const &tag: node->tags) {
        output() << "#TAG#: " << tag << '\n';
    }
    ASTVisitor::visitAllChildren(node);
    indent--;
}

std::ostream &Fern::ASTPrinter::output() {
    out << std::string(indent, '\t');
    return out;
}
