//
// Created by Spencer Comin on 2021-04-12.
//

#ifndef FERN_SYMBOL_TABLE_H
#define FERN_SYMBOL_TABLE_H

#include "types.h"
#include "AST.h"

#include <unordered_map>
#include <string>
#include <stack>

using std::string;
using std::unordered_map;
using std::stack;


namespace Fern {
    class Reference {
    public:

        enum Type {
            LITERAL,
            NODE_PTR
        } tag;
        union {
            FernType literal;
//            struct {
            ASTNode *fernNode;
//                int cache;
//                bool cacheIsValid;
//                vector<ASTNode *> parents;
//           };
        };

        explicit Reference(FernType &);

        explicit Reference(ASTNode *);

        Reference(Reference &reference);

        Reference();

        Reference &operator=(const Reference &);

        ~Reference();
    };

    class SymbolTable {
    public:
        explicit SymbolTable(SymbolTable *parent, Block *scope);

        void set(string &name, Reference &value);

        Reference *get(string &name, Block *currentScope);

    private:
        SymbolTable *parent;
        Block *scope;
        unordered_map<string, Reference *> lookup{};
    };
}


#endif //FERN_SYMBOL_TABLE_H
