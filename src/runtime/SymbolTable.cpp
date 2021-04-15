//
// Created by Spencer Comin on 2021-04-12.
//

#include "SymbolTable.h"

Fern::Reference::Reference(Fern::FernType &val) {
    tag = LITERAL;
    literal = val;
}

Fern::Reference::Reference() {
    tag = NODE_PTR;
    fernNode = nullptr;
    //parents = {};
}

Fern::Reference::~Reference() {
    delete &literal;
}

Fern::Reference &Fern::Reference::operator=(const Reference& other) {
    return *this;
}

Fern::SymbolTable::SymbolTable(Fern::SymbolTable *parent, Fern::Block *scope) {
    this->parent = parent;
    this->scope = scope;
}

void Fern::SymbolTable::set(string &name, Fern::Reference &value) {
    lookup[name] = &value;
}

Fern::Reference *Fern::SymbolTable::get(string &name, Fern::Block *currentScope) {
    if (currentScope == scope) {
        if (lookup.find(name) != lookup.end())
            return lookup[name];
        else
            return nullptr;
    } else if (parent != nullptr) {
        return parent->get(name, currentScope);
    } else
        return nullptr;
}
