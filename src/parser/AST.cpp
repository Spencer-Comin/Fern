//
// Created by Spencer Comin on 2021-03-17.
//

#include "AST.h"

Fern::Binary::Binary(Fern::ASTNode *left, Fern::ASTNode *right,
                     Fern::Operator op) {
    children.resize(2);
    children[0] = left;
    children[1] = right;
    this->op = op;
}

std::string Fern::Binary::info() {
    return "Binary Expression, op: " + Fern::opToString(op);
}

Fern::Literal::Literal(std::string &value, Fern::Literal::Type type) {
    this->value = value;
    this->type = type;
}

std::string Fern::Literal::info() {
    std::string typeName{};
    switch (type) {
        case NUMBER:
            typeName = "Number";
            break;
        case STRING:
            typeName = "String";
            break;
        case TAG_LITERAL:
            typeName = "Tag";
            break;
    }
    return typeName + " literal: " + value;
}

Fern::ID::ID(std::string &name) {
    this->name = name;
}

std::string Fern::ID::info() {
    return "Identifier: " + name;
}


std::ostream &Fern::operator<<(std::ostream &os, ASTNode &node) {
    static int indent = 0;
    os << std::string(indent, '\t') << node.info() << '\n';
    indent++;
    for (auto const &child: node.children) {
        os << *child;
    }
    indent--;
    return os;
}

std::string Fern::ASTNode::info() {
    return "Node";
}
