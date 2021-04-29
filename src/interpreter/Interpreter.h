//
// Created by Spencer Comin on 2021-03-30.
//

#ifndef FERN_INTERPRETER_H
#define FERN_INTERPRETER_H

#include "AST.h"
#include "SymbolTable.h"

namespace Fern {

    class Interpreter : private ASTVisitor {
    public:
        ~Interpreter() = default;

        Reference interpret(ASTNode *root);

    private:

        void visitRoot(ASTNode *node) override;

        void visitConcatenation(Concatenation *node) override;

        void visitBlock(Block *node) override;

        void visitEvaluator(Evaluator* node) override;

        void visitBinary(Binary *node) override;

        void visitTernary(Ternary *node) override;

        void visitUnary(Unary *node) override;

        void visitLiteral(Literal *node) override;

        void visitID(ID *node) override;

        void decide(Reference &condition, Reference& if_body, Reference& else_body);

        SymbolTable *environment = new SymbolTable(nullptr, nullptr);

        SymbolTable *global = environment;

        Block *currentScope = nullptr;

        Reference returnBucket {};
    };
}


#endif //FERN_INTERPRETER_H
