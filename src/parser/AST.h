//
// Created by Spencer Comin on 2021-03-17.
//

#ifndef FERN_AST_H
#define FERN_AST_H

#include "Operator.h"
#include "types.h"

#include <ostream>
#include <vector>
#include <unordered_map>
#include <unordered_set>
#include <set>

using std::vector;
using std::string;
using std::ostream;
using std::unordered_map;
using std::unordered_set;
using std::set;

namespace Fern {

    class ASTVisitor;

    class AST {
    public:
        virtual void accept(ASTVisitor *visitor) = 0;
    };

    class ASTNode : public AST {

    public:
        virtual ~ASTNode();

        void addTags(set<string> &tags);

        void setConditions(set<string> &conditions);

        void addChild(ASTNode *child);

        void addEvaluationList(ASTNode *evaluationList);

        void accept(ASTVisitor *visitor) override;

        friend class Block;

        friend class ASTVisitor;

        bool parenthesized = false;
        bool isEvaluation = false;

        set<string> tags = {};
        set<string> conditions = {};

        vector<ASTNode *> children;

        int evaluationIndex = -1;
    };

    class Concatenation : public ASTNode {
    public:
        void accept(ASTVisitor *visitor) override;
    };

    class Block : public ASTNode {
    public:
        explicit Block(ASTNode *list);

        void accept(ASTVisitor *visitor) override;
    };

    class Binary : public ASTNode {
    public:
        Fern::Operator op;

        Binary(ASTNode *left, ASTNode *right, Fern::Operator op);

        void accept(ASTVisitor *visitor) override;
    };

    class Ternary : public ASTNode {
    public:
        enum TernaryType {
            DECISION,
            SLICE,
            REPLACE
        } type;

        Ternary(ASTNode *left, ASTNode *center, ASTNode *right, TernaryType type);

        void accept(ASTVisitor *visitor) override;
    };

    class Unary : public ASTNode {
    public:
        Fern::Operator op;

        Unary(ASTNode *child, Fern::Operator op);

        void accept(ASTVisitor *visitor) override;
    };

    class Literal : public ASTNode {
    public:
        string value;
        FernType::Type type;

        Literal(string &value, FernType::Type type);

        void accept(ASTVisitor *visitor) override;
    };

    class ID : public ASTNode {
    public:
        string name;

        explicit ID(const string &name);

        void accept(ASTVisitor *visitor) override;
    };


    class ASTVisitor {
    public:
        virtual void visitRoot(ASTNode *node) = 0;

        virtual void visitConcatenation(Concatenation *node) = 0;

        virtual void visitBlock(Block *node) = 0;

        virtual void visitBinary(Binary *node) = 0;

        virtual void visitTernary(Ternary *node) = 0;

        virtual void visitUnary(Unary *node) = 0;

        virtual void visitLiteral(Literal *node) = 0;

        virtual void visitID(ID *node) = 0;

        virtual void visitAllChildren(ASTNode *node);
    };
}

#endif //FERN_AST_H