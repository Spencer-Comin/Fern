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
        virtual ~ASTNode();

        void addTag(std::string &tag);
        void setTags(std::vector<std::string> &tags);

    protected:
        virtual std::string info();
        //ASTNode *parent;
        std::vector<std::string> tags = {};
    };

    class ASTBranch : public ASTNode {

    };

    class ASTLeaf : public ASTNode {

    };

    class Concatenation : public ASTBranch {
    private:
        std::string info() override;
    };

    class Binary : public ASTBranch {
    public:
        Fern::Operator op;
        Binary(ASTNode* left, ASTNode* right, Fern::Operator op);
    private:
        std::string info() override;
    };

    class Unary : public ASTBranch {
    public:
        Fern::Operator op;
        Unary(ASTNode* child, Fern::Operator op);

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
