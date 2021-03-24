//
// Created by Spencer Comin on 2021-03-17.
//

#ifndef FERN_AST_H
#define FERN_AST_H

#include "Operator.h"

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

    class ASTNode {
    public:
        friend ostream &operator<<(ostream &os, ASTNode &node);

        virtual ~ASTNode();

        void addTag(string &tag);

        void setTags(set<string> &tags);
        void setConditions(set<string> &conditions);

        void addChild(ASTNode *child);
        void addEvaluationList(ASTNode *evaluationList);


        friend class Block;

        bool parenthesized = false;

    protected:
        virtual string info();
        unordered_set<int> conditionIndices;
        set<string> conditions = {};
        int evaluationIndex = -1;

        //ASTNode *parent;
        set<string> tags = {};

        vector<ASTNode *> children;
    };

    class ASTBranch : public ASTNode {

    };

    class ASTLeaf : public ASTNode {

    };

    class Concatenation : public ASTBranch {
    private:
        string info() override;
    };

    class Block : public ASTBranch {
    public:
        explicit Block(ASTNode *list);

    private:
        string info() override;

        unordered_map<string, ASTNode *> symbolTable;
    };

    class Binary : public ASTBranch {
    public:
        Fern::Operator op;

        Binary(ASTNode *left, ASTNode *right, Fern::Operator op);

    private:
        string info() override;
    };

    class Unary : public ASTBranch {
    public:
        Fern::Operator op;

        Unary(ASTNode *child, Fern::Operator op);

    private:
        string info() override;
    };

    class Literal : public ASTLeaf {
    public:
        string value;
        enum Type {
            STRING,
            NUMBER,
            TAG_LITERAL
        } type;

        Literal(string &value, Type type);

    private:
        string info() override;
    };

    class ID : public ASTLeaf {
    public:
        string name;

        explicit ID(const string &name);

    private:
        string info() override;
    };


    template<class T>
    class Lazy : public T {
    };

    template<class T>
    class Eager : public T {
    };
}


#endif //FERN_AST_H
