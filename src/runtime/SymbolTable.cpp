//
// Created by Spencer Comin on 2021-04-12.
//

#include "SymbolTable.h"
#include "errors.h"
#include <queue>

const Fern::NullReference &Fern::null{};

Fern::Boolean Fern::Reference::tilde(const Fern::Reference &right) {
    // search dependencies of fernNode for target
    ASTNode *current, *target = std::get<ASTNode *>(right);
    std::queue<ASTNode *> q{};
    q.push(std::get<ASTNode *>(*this));
    while (!q.empty()) {
        current = q.front();
        q.pop();
        if (current == target) return True;
        for (auto child : current->children)
            if (child != nullptr) q.push(child);
    }
    return False;
}

Fern::Boolean Fern::Reference::tripleEqual(const Fern::Reference &right) {
    return {this == &right};
}

Fern::SymbolTable::SymbolTable(Fern::SymbolTable *parent, Fern::Block *scope) {
    this->parent = parent;
    directory[scope] = this;
}

void Fern::SymbolTable::set(string &name, Fern::Reference &value) {
    lookup[name] = value;
}

Fern::Reference &Fern::SymbolTable::get(string &name) {

    if (lookup.find(name) != lookup.end())
        return lookup.at(name);
    else if (parent != nullptr) {
        return parent->get(name);
    } else
        return const_cast<NullReference &>(null);
}

Fern::SymbolTable *Fern::SymbolTable::getTable(Fern::Block *scope) {
    if (directory.find(scope) != directory.end())
        return directory.at(scope);
    else
        throw Fern::RuntimeError("Cannot find an associated Symbol Table for given scope");
}


/*******************************************************************************
 * NullReference methods (all return False/null)
 ******************************************************************************/

Fern::Boolean Fern::NullReference::tripleEqual(const Fern::Reference &) { return False; }

Fern::Boolean Fern::NullReference::tilde(const Fern::Reference &) { return False; }

//Fern::Reference &Fern::NullReference::dot(const Fern::Reference &) { return const_cast<NullReference &>(null); }
//
//Fern::Reference &Fern::NullReference::decision(const Fern::Reference &) { return const_cast<NullReference &>(null); }
//
//Fern::Reference &Fern::NullReference::iteration(const Fern::Reference &) { return const_cast<NullReference &>(null); }
//
//Fern::Reference &Fern::NullReference::visit(const Fern::Reference &) { return const_cast<NullReference &>(null); }
