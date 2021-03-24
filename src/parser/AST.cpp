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

Fern::Unary::Unary(Fern::ASTNode *child, Fern::Operator op) {
    children.resize(1);
    children[0] = child;
    this->op = op;
}

string Fern::Unary::info() {
    return "Unary Expression, op: " + Fern::opToString(op);
}

string Fern::Binary::info() {
    return "Binary Expression, op: " + Fern::opToString(op);
}

Fern::Literal::Literal(string &value, Fern::Literal::Type type) {
    this->value = value;
    this->type = type;
}

string Fern::Literal::info() {
    string typeName{};
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

Fern::ID::ID(const string &name) {
    this->name = name;
}

string Fern::ID::info() {
    return "Identifier: " + name;
}


ostream &Fern::operator<<(ostream &os, ASTNode &node) {
    static int indent = 0;
    os << string(indent, '\t');
    os << node.info() << '\n';
    indent++;
    for (auto const &tag: node.tags) {
        os << string(indent, '\t') << "TAG: " << tag << '\n';
    }
    for (auto const &condition: node.conditions) {
        os << string(indent, '\t') << "CONDITION: " << condition << '\n';
    }
    for (auto const &child: node.children) {
        os << *child;
    }
    indent--;

    return os;
}

string Fern::ASTNode::info() {
    return "Root";
}

Fern::ASTNode::~ASTNode() {
    for (auto &child: children) {
        delete child;
    }
}

void Fern::ASTNode::addTag(string &tag) {
    tags.insert(tag);
}

void Fern::ASTNode::setTags(set<string> &new_tags) {
    tags = new_tags;
}

void Fern::ASTNode::addChild(Fern::ASTNode *child) {
    children.push_back(child);
}

void Fern::ASTNode::setConditions(set<string> &new_conditions) {
    conditions = new_conditions;
}

void Fern::ASTNode::addEvaluationList(Fern::ASTNode *evaluationList) {
    evaluationIndex = children.size();
    addChild(evaluationList);
}


string Fern::Concatenation::info() {
    return "Concatenation";
}

string Fern::Block::info() {
    return "Block";
}

Fern::Block::Block(Fern::ASTNode *list) {
    children = list->children;
    tags = list->tags;
    //delete list;
}
