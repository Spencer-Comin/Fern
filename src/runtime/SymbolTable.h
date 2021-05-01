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
#include <variant>

using std::string;
using std::unordered_map;
using std::stack;
using std::variant;


namespace Fern {
    class Reference : public variant<std::monostate, FernType, ASTNode *> {
    public:
        using variant::variant;

        virtual Boolean tripleEqual(const Reference &);

        virtual Boolean tilde(const Reference &);

//        virtual Reference &dot(const Reference &);
//
//        virtual Reference &decision(const Reference &);
//
//        virtual Reference &iteration(const Reference &);
//
//        virtual Reference &visit(const Reference &);
    };

    class NullReference : public Reference {
        using Reference::Reference;

        Boolean tripleEqual(const Reference &) override;

        Boolean tilde(const Reference &) override;

//        Reference &dot(const Reference &) override;
//
//        Reference &decision(const Reference &) override;
//
//        Reference &iteration(const Reference &) override;
//
//        Reference &visit(const Reference &) override;
    };

    extern const NullReference &null;

    class SymbolTable {
    public:
        SymbolTable(SymbolTable *parent, Block *scope);

        void set(string &name, Reference &value);

        Reference &get(string &name);

        static SymbolTable *getTable(Block *scope);

        static void deregisterTable(Block *scope);

    private:
        static inline unordered_map<Block *, stack<SymbolTable *>> directory{};
        SymbolTable *parent{};
        unordered_map<string, Reference> lookup{};
    };
}


#endif //FERN_SYMBOL_TABLE_H
