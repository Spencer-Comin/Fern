//
// Created by Spencer Comin on 2021-03-17.
//

#ifndef FERN_AST_H
#define FERN_AST_H

#include "Operator.h"

#include <ostream>
#include <vector>

namespace Fern {

    class ASTNode {
    public:
        std::vector<ASTNode*> children;
        friend std::ostream& operator<<(std::ostream &os, ASTNode &node);

    protected:
        virtual std::string info();
    };

    class ASTBranch : public ASTNode {

    };

    class ASTLeaf : public ASTNode {

    };

    class Binary : public ASTBranch {
    public:
        Fern::Operator op;
        Binary(ASTNode* left, ASTNode* right, Fern::Operator op);
    private:
        std::string info() override;
    };

    class Literal : public ASTLeaf {
    public:
        std::string value;
        enum Type {
            STRING,
            NUMBER,
            TAG_LITERAL
        } type;
        Literal(std::string &value, Type type);
    private:
        std::string info() override;
    };

    class ID : public ASTLeaf {
    public:
        std::string name;
        explicit ID(std::string &name);
    private:
        std::string info() override;
    };


    template<class T>
    class Lazy : public T {};

    template<class T>
    class Eager : public T {};
}



#endif //FERN_AST_H
