//
// Created by Spencer Comin on 2021-03-30.
//

#include "Interpreter.h"
#include "errors.h"
#include "globals.h"
#include "types.h"

#ifdef DEBUG

#include <iostream>

#endif

using std::holds_alternative;
using std::get;

Fern::Reference Fern::Interpreter::interpret(Fern::ASTNode *root) {
    root->accept(this);
    return returnBucket;
}

void Fern::Interpreter::visitRoot(Fern::ASTNode *node) {
#ifdef DEBUG
    std::cout << "interpreter visiting a Root\n";
#endif
    visitAllChildren(node);
}

void Fern::Interpreter::visitConcatenation(Fern::Concatenation *node) {
#ifdef DEBUG
    std::cout << "interpreter visiting a Concatenation\n";
#endif
    visitAllChildren(node);
}

void Fern::Interpreter::visitBlock(Fern::Block *node) {
#ifdef DEBUG
    std::cout << "interpreter visiting a Block\n";
#endif
    Fern::Block *enclosing{currentScope};
    SymbolTable *parent{environment};
    currentScope = node;
    try {
        environment = SymbolTable::getTable(node);
    } catch (RuntimeError &) {
        environment = new SymbolTable(parent, node);
    }

    visitAllChildren(node);

    string return_name{"return"};
    returnBucket = environment->get(return_name);

    currentScope = enclosing;
    environment = parent;
}

void
Fern::Interpreter::decide(Fern::Reference &condition_body, Reference &if_body, Reference &else_body) {
    bool condition = std::visit(overload{
            [this](ASTNode *node) -> bool {
                node->accept(this);
                return get<FernType>(returnBucket).truthValue();
            },
            [](FernType &literal) -> bool { return literal.truthValue(); },
            [](auto &&) -> bool { throw DebugError("bad condition reference in decision"); }
    }, condition_body);
    if (condition) {
        std::visit(overload{
                [this](ASTNode *node) { node->accept(this); },
                [this](FernType &literal) { returnBucket = literal; },
                [](auto &&) { throw DebugError("bad if body reference in decision"); }
        }, if_body);
    } else
        std::visit(overload{
                [this](ASTNode *node) { node->accept(this); },
                [this](FernType &literal) { returnBucket = literal; },
                [](auto &&) { throw DebugError("bad else body reference in decision"); }
        }, else_body);
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
        case Fern::Operator::BANG:return true;

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
        case Fern::Operator::AT:return false;
    }
}

void Fern::Interpreter::visitBinary(Fern::Binary *node) {
#ifdef DEBUG
    std::cout << "interpreter visiting a Binary\n";
#endif
    Reference left, right;

    if (!(node->op == Operator::EQUAL || node->op == Operator::WALRUS)) {
        /* visit left side */
        node->children[0]->accept(this);
        left = returnBucket;
        /* visit right side */
        node->children[1]->accept(this);
        right = returnBucket;
    }

    if (isCxxOp(node->op) && !(holds_alternative<FernType>(left) && holds_alternative<FernType>(right))) {
        ASTNode *leftNode, *rightNode;
        if (holds_alternative<ASTNode *>(left))
            leftNode = get<ASTNode *>(left);
        else if (holds_alternative<FernType>(left))
            leftNode = new Literal(get<FernType>(left));
        else
            throw DebugError("invalid lvalue for lazy evaluation");

        if (holds_alternative<ASTNode *>(right))
            rightNode = get<ASTNode *>(right);
        else if (holds_alternative<FernType>(right))
            rightNode = new Literal(get<FernType>(right));
        else
            throw DebugError("invalid rvalue for lazy evaluation");

        returnBucket = Reference(new Binary(leftNode, rightNode, node->op));
        return;
    }

    switch (node->op) {
        // for operators that exist in c++, we can use operator overloading and treat all identically
#define CXX_OP(op) returnBucket = get<FernType>(left) op get<FernType>(right); break
        case Operator::AND: CXX_OP(&);
        case Operator::OR: CXX_OP(|);
        case Operator::XOR: CXX_OP(^);
        case Operator::PLUS: CXX_OP(+);
        case Operator::MINUS: CXX_OP(-);
        case Operator::STAR: CXX_OP(*);
        case Operator::SLASH: CXX_OP(/);
        case Operator::MODULO: CXX_OP(%);
        case Operator::DOUBLE_EQUAL: CXX_OP(==);
        case Operator::BANG_EQUAL: CXX_OP(!=);
        case Operator::LT: CXX_OP(<);
        case Operator::GT: CXX_OP(>);
        case Operator::LT_EQUAL: CXX_OP(<=);
        case Operator::GT_EQUAL: CXX_OP(>=);
        case Operator::DOUBLE_AND: CXX_OP(&&);
        case Operator::DOUBLE_OR: CXX_OP(||);
#undef CXX_OP

        case Operator::TRIPLE_EQUAL: {
            returnBucket = left.tripleEqual(right);
            break;
        }
        case Operator::TILDE: {
            returnBucket = left.tilde(right);
            break;
        }
        case Operator::AT: {
            FernType left_val;
            if (holds_alternative<ASTNode *>(right) && holds_alternative<FernType>(left) &&
                holds_alternative<TagType>(left_val = get<FernType>(left))) {
                auto &tag_set = get<ASTNode *>(right)->tags;
                returnBucket = Fern::Boolean{tag_set.find(get<TagType>(left_val)) != tag_set.end()};
            } else returnBucket = False;
            break;
        }
        case Operator::DOT:break; // TODO: figure out dot
        case Operator::DECISION: {
            FernType null_value{};
            Literal else_node{null_value};
            Reference else_body{&else_node};
            decide(left, right, else_body);
            break;
        }
        case Operator::ITERATION: { // TODO: figure out iterations

            break;
        }
        case Operator::VISIT: {
            if (!holds_alternative<ASTNode *>(left)) throw RuntimeError("bad lvalue for visit");
            if (!holds_alternative<ASTNode *>(right)) throw RuntimeError("bad rvalue for visit");
            auto left_node = get<ASTNode *>(left), right_node = get<ASTNode *>(right);
            if (left_node->conditions.size() != 1) throw RuntimeError("visit lvalue must have one condition");
            SymbolTable *env;
            Block *body = nullptr;
            auto id = dynamic_cast<ID *>(node->children[0]);
            if (id != nullptr) { // body is ID
                id->accept(this);
                if (holds_alternative<ASTNode *>(returnBucket))
                    body = dynamic_cast<Block *>(get<ASTNode *>(returnBucket));
                else goto nonblock;
            } else { // body is not ID, try block
                body = dynamic_cast<Block *>(node->children[0]);
            }
            nonblock:
            if (body == nullptr) throw RuntimeError("cannot evaluate non block with parameters");
            try {
                env = SymbolTable::getTable(body);
            } catch (RuntimeError &) {
                env = new SymbolTable(environment, body);
            }
            for (auto child: right_node->children) {
                child->accept(this);
                env->set(left_node->conditions[0], returnBucket);
                left_node->accept(this);
            }
            break;
        }
        case Operator::EQUAL: {
            string name = dynamic_cast<ID *>(node->children[0])->name;
            node->children[1]->accept(this);
            if (node->tags.find(string{"global"}) != node->tags.end()) {
                global->set(name, returnBucket);
            } else {
                environment->set(name, returnBucket);
            }
            break;
        }
        case Operator::WALRUS: {
            string name = dynamic_cast<ID *>(node->children[0])->name;
            right = node->children[1];
            environment->set(name, right);
            break;
        }
        default:throw DebugError("unrecognized binary op");
    }
}

void Fern::Interpreter::visitTernary(Fern::Ternary *node) {
#ifdef DEBUG
    std::cout << "interpreter visiting a Ternary\n";
#endif
    switch (node->type) {
        case Ternary::DECISION: {
            node->children[0]->accept(this);
            auto left = returnBucket;
            node->children[1]->accept(this);
            auto centre = returnBucket;
            node->children[2]->accept(this);
            auto right = returnBucket;
            decide(left, centre, right);
            break;
        }
        case Ternary::SLICE: {
            FernType literal;
            int top, bottom;
            node->children[1]->accept(this);
            if (!holds_alternative<FernType>(returnBucket) ||
                !holds_alternative<int>(literal = get<FernType>(returnBucket)))
                throw RuntimeError("slice bottom must be an int");
            bottom = get<int>(literal);
            node->children[2]->accept(this);
            if (!holds_alternative<FernType>(returnBucket) ||
                !holds_alternative<int>(literal = get<FernType>(returnBucket)))
                throw RuntimeError("slice top must be an int");
            top = get<int>(literal);
            auto slice = new Concatenation();

            auto list = dynamic_cast<Concatenation*>(node->children[0]);
            if (list == nullptr) {
                node->children[0]->accept(this);
                if (!holds_alternative<ASTNode*>(returnBucket)) throw RuntimeError("bad slice");
                list = dynamic_cast<Concatenation*>(get<ASTNode*>(returnBucket));
                if (list == nullptr) throw RuntimeError("bad slice");
            }
            for (int i = bottom; i < top; i++) {
                slice->children.push_back(list->children[i]);
            }
            returnBucket = slice;
            break;
        }
        case Ternary::REPLACE:break; //TODO: figure out replace
    }
}

void Fern::Interpreter::visitUnary(Fern::Unary *node) {
#ifdef DEBUG
    std::cout << "interpreter visiting a Unary\n";
#endif
    node->children[0]->accept(this);
    if (holds_alternative<ASTNode *>(returnBucket))
        returnBucket = Reference(new Unary(get<ASTNode *>(returnBucket), node->op));
    else
        switch (node->op) {
            case Operator::PLUS:returnBucket = +get<FernType>(returnBucket);
                break;
            case Operator::MINUS:returnBucket = -get<FernType>(returnBucket);
                break;
            case Operator::TILDE:returnBucket = ~get<FernType>(returnBucket);
                break;
            case Operator::BANG:returnBucket = !get<FernType>(returnBucket);
                break;
            default:throw DebugError("unrecognized unary op");
        }
}

void Fern::Interpreter::visitLiteral(Fern::Literal *node) {
#ifdef DEBUG
    std::cout << "interpreter visiting a Literal: " << node->value << "\n";
#endif
    returnBucket = node->value;
}

void Fern::Interpreter::visitID(Fern::ID *node) {
#ifdef DEBUG
    std::cout << "interpreter visiting an ID: " << node->name << "\n";
#endif
    returnBucket = environment->get(node->name);
}

void Fern::Interpreter::visitEvaluator(Fern::Evaluator *node) {
#ifdef DEBUG
    std::cout << "interpreter visiting an Evaluator\n";
#endif
    auto parameters = node->children[1];
    if (parameters == nullptr) { // no parameters, valid to evaluate anything
        node->children[0]->accept(this);
        std::visit(overload{
                [this](FernType &literal) { returnBucket = literal; },
                [this](ASTNode *node) { node->accept(this); },
                [](auto &&) { throw RuntimeError("Cannot evaluate null value"); }
        }, returnBucket);
    } else { // yes parameters, only valid for block

        Block *body = nullptr;
        auto id = dynamic_cast<ID *>(node->children[0]);
        if (id != nullptr) { // body is ID
            id->accept(this);
            if (holds_alternative<ASTNode *>(returnBucket))
                body = dynamic_cast<Block *>(get<ASTNode *>(returnBucket));
            else goto nonblock;
        } else { // body is not ID, try block
            body = dynamic_cast<Block *>(node->children[0]);
        }
        nonblock:
        if (body == nullptr) throw RuntimeError("cannot evaluate non block with parameters");

        // set parameters in body's environment
        SymbolTable *env;
        try {
            env = SymbolTable::getTable(body);
        } catch (RuntimeError &) {
            env = new SymbolTable(environment, body);
        }
        if (body->conditions.size() == 1) {
            parameters->accept(this);
            env->set(body->conditions[0], returnBucket);
        } else if (body->conditions.size() != parameters->children.size())
            throw RuntimeError("wrong number of parameters");
        else {
            for (int i = 0; i < node->children.size(); i++) {
                parameters->children[i]->accept(this);
                env->set(body->conditions[i], returnBucket);
            }
        }
        // evaluate body
        body->accept(this);
    }
}
