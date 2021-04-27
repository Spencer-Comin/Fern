//
// Created by Spencer Comin on 2021-03-17.
//

#include "AST.h"
#include "errors.h"

/*******************************************************************************
 *  ASTNode Utilities
 ******************************************************************************/

Fern::ASTNode::~ASTNode() {
    for (auto &child: children) {
        delete child;
    }
}

void Fern::ASTNode::addTags(set<string> &new_tags) {
    tags.insert(new_tags.begin(), new_tags.end());
}

void Fern::ASTNode::addChild(Fern::ASTNode *child) {
    children.push_back(child);
}

void Fern::ASTNode::setConditions(vector<string> &new_conditions) {
    conditions = new_conditions;
}

void Fern::ASTNode::addEvaluationList(Fern::ASTNode *evaluationList) {
    if (evaluationList != nullptr) {
        evaluationIndex = children.size();
        addChild(evaluationList);
    }
    isEvaluation = true;
}


/*******************************************************************************
 *  Constructors
 ******************************************************************************/

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

Fern::Literal::Literal(Fern::FernType &value) : value(value) {}

Fern::ID::ID(const string &name) {
    this->name = name;
}

Fern::Block::Block(Fern::ASTNode *list) {
    children = list->children;
    tags = list->tags;
}

Fern::Ternary::Ternary(Fern::ASTNode *left, Fern::ASTNode *center, Fern::ASTNode *right,
                       Fern::Ternary::TernaryType type) {
    children.resize(3);
    children[0] = left;
    children[1] = center;
    children[2] = right;
    this->type = type;
}

Fern::Evaluator::Evaluator(Fern::ASTNode *block, Fern::ASTNode *conditionList) {
    children.resize(2);
    children[0] = block;
    children[1] = conditionList;
}


/******************************************************************************
 *  Visitor acceptors
 ******************************************************************************/

void Fern::Unary::accept(Fern::ASTVisitor *visitor) {
    visitor->visitUnary(this);
}

void Fern::Binary::accept(Fern::ASTVisitor *visitor) {
    visitor->visitBinary(this);
}

void Fern::Literal::accept(Fern::ASTVisitor *visitor) {
    visitor->visitLiteral(this);
}

void Fern::ID::accept(Fern::ASTVisitor *visitor) {
    visitor->visitID(this);
}

void Fern::Concatenation::accept(Fern::ASTVisitor *visitor) {
    visitor->visitConcatenation(this);
}

void Fern::Block::accept(Fern::ASTVisitor *visitor) {
    visitor->visitBlock(this);
}

void Fern::Ternary::accept(Fern::ASTVisitor *visitor) {
    visitor->visitTernary(this);
}

void Fern::ASTNode::accept(Fern::ASTVisitor *visitor) {
    visitor->visitRoot(this);
}

void Fern::Evaluator::accept(Fern::ASTVisitor *visitor) {
    visitor->visitEvaluator(this);
}

void Fern::ASTVisitor::visitAllChildren(Fern::ASTNode *node) {
    for (auto &child: node->children) {
        if (child != nullptr)
            child->accept(this);
    }
}
