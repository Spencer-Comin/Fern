//
// Created by Spencer Comin on 2021-03-30.
//

#include "ASTPrinter.h"
#include "types.h"
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

void Fern::ASTPrinter::visitEvaluator(Fern::Evaluator *node) {
    output() << "Evaluator\n";
    output() << "BODY: ";
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
    if (std::holds_alternative<int>(node->value))
        typeName = "Number";
    else if (std::holds_alternative<string>(node->value))
        typeName = "String";
    else if (std::holds_alternative<TagType>(node->value))
        typeName = "Tag";
    else if (std::holds_alternative<Boolean>(node->value))
        typeName = "Bool";
    output() << typeName << " literal: " << node->value << '\n';
    visitAllChildren(node);
}

void Fern::ASTPrinter::visitID(Fern::ID *node) {
    output() << "ID: " << node->name << '\n';
    visitAllChildren(node);
}

void Fern::ASTPrinter::print(Fern::ASTNode *root) {
    root->accept(this);
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


