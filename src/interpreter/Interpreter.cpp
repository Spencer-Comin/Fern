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

static inline bool isCxxOp(Fern::Operator op) {
    switch (op) {
        case Fern::Operator::AND:
        case Fern::Operator::OR:
        case Fern::Operator::XOR:
        case Fern::Operator::PLUS:
        case Fern::Operator::MINUS:
        case Fern::Operator::STAR:
        case Fern::Operator::SLASH:
        case Fern::Operator::MODULO:
        case Fern::Operator::DOUBLE_EQUAL:
        case Fern::Operator::BANG_EQUAL:
        case Fern::Operator::LT:
        case Fern::Operator::GT:
        case Fern::Operator::LT_EQUAL:
        case Fern::Operator::GT_EQUAL:
        case Fern::Operator::BANG:
            return true;

        case Fern::Operator::TILDE:
        case Fern::Operator::DOUBLE_AND:
        case Fern::Operator::DOUBLE_OR:
        case Fern::Operator::EQUAL:
        case Fern::Operator::TRIPLE_EQUAL:
        case Fern::Operator::WALRUS:
        case Fern::Operator::DOT:
        case Fern::Operator::DECISION:
        case Fern::Operator::ITERATION:
        case Fern::Operator::VISIT:
            return false;
    }
}

void Fern::Interpreter::visitBinary(Fern::Binary *node) {
    //visitAllChildren(node);
    std::cout << "interpreter visiting a Binary\n";
    /* visit left side */
    node->children[0]->accept(this);
    Reference left = returnBucket;
    /* visit right side */
    node->children[1]->accept(this);
    Reference right = returnBucket;

    switch (node->op) {
        // for operators that exist in c++, we can use operator overloading and treat all identically
#define CXX_OP(op) do { \
        if (left.tag == Reference::LITERAL && right.tag == Reference::LITERAL) { \
            returnBucket.tag = Reference::LITERAL; \
            returnBucket.literal = left.literal op right.literal; \
            break; \
        } else \
            throw DebugError("Binary operation with non-literal nodes"); \
    } while (0)

        case Operator::AND:
            CXX_OP(&);
        case Operator::OR:
            CXX_OP(|);
        case Operator::XOR:
            CXX_OP(^);
        case Operator::PLUS:
            CXX_OP(+);
        case Operator::MINUS:
            CXX_OP(-);
        case Operator::STAR:
            CXX_OP(*);
        case Operator::SLASH:
            CXX_OP(/);
        case Operator::MODULO:
            CXX_OP(%);
        case Operator::DOUBLE_EQUAL:
            CXX_OP(==);
        case Operator::BANG_EQUAL:
            CXX_OP(!=);
        case Operator::LT:
            CXX_OP(<);
        case Operator::GT:
            CXX_OP(>);
        case Operator::LT_EQUAL:
            CXX_OP(<=);
        case Operator::GT_EQUAL:
            CXX_OP(>=);

        case Operator::TRIPLE_EQUAL:
            break;
        case Operator::TILDE:
            break;
        case Operator::DOUBLE_AND: // handled differently than CXX_OP to keep short circuiting
            break;
        case Operator::DOUBLE_OR: // handled differently than CXX_OP to keep short circuiting
            break;
        case Operator::EQUAL:
            break;
        case Operator::WALRUS:
            break;
        case Operator::DOT:
            break;
        case Operator::DECISION:
            break;
        case Operator::ITERATION:
            break;
        case Operator::VISIT:
            break;
        default:
            throw DebugError("unrecognized binary op");
    }
}

void Fern::Interpreter::visitTernary(Fern::Ternary *node) {
    visitAllChildren(node);
    std::cout << "interpreter visiting a Ternary\n";
}

void Fern::Interpreter::visitUnary(Fern::Unary *node) {
    node->children[0]->accept(this);
    if (returnBucket.tag == Reference::NODE_PTR)
        returnBucket = Reference(new Unary(returnBucket.fernNode, node->op));
    else
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
                throw DebugError("unrecognized unary op");
        }
    std::cout << "interpreter visiting a Unary\n";
}

void Fern::Interpreter::visitLiteral(Fern::Literal *node) {
    //visitAllChildren(node);
    Literal obj = *node;
    FernType value{obj};
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
