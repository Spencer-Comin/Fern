//
// Created by Spencer Comin on 2021-03-30.
//

#ifndef FERN_ASTPRINTER_H
#define FERN_ASTPRINTER_H

#include "AST.h"

#include <iostream>

namespace Fern {
    class ASTPrinter : private ASTVisitor {
    public:
        explicit ASTPrinter(std::ostream &os) : out(os) {}

        virtual ~ASTPrinter() = default;

        void print(ASTNode *root);

    private:
        void visitRoot(ASTNode *node) override;

        void visitConcatenation(Concatenation *node) override;

        void visitBlock(Block *node) override;

        void visitBinary(Binary *node) override;

        void visitTernary(Ternary *node) override;

        void visitUnary(Unary *node) override;

        void visitLiteral(Literal *node) override;

        void visitID(ID *node) override;

        void visitAllChildren(ASTNode *node) override;

        std::ostream &output();

        std::ostream &out;

        int indent = 0;
    };
}

#endif //FERN_ASTPRINTER_H
