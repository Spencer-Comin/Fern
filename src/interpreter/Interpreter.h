//
// Created by Spencer Comin on 2021-03-30.
//

#ifndef FERN_INTERPRETER_H
#define FERN_INTERPRETER_H

#include "AST.h"


namespace Fern {
    class Interpreter : private ASTVisitor{
    public:
        virtual ~Interpreter() = default;

        void interpret(ASTNode *root);

    private:
        void visitRoot(ASTNode *node) override;

        void visitConcatenation(Concatenation *node) override;

        void visitBlock(Block *node) override;

        void visitBinary(Binary *node) override;

        void visitTernary(Ternary *node) override;

        void visitUnary(Unary *node) override;

        void visitLiteral(Literal *node) override;

        void visitID(ID *node) override;
    };
}


#endif //FERN_INTERPRETER_H
