//
// Created by Spencer Comin on 2021-04-01.
//

#include "types.h"
#include "errors.h"
#include "AST.h"

using std::holds_alternative;
using std::get;

Fern::FernType::FernType(Fern::Literal &ast_literal) {
    *this = ast_literal.value;
}

/******************************************************************************
 * debug print functionality
 ******************************************************************************/
ostream &Fern::operator<<(ostream &os, const Fern::FernType &v) {
    if (holds_alternative<int>(v))
        os << get<int>(v);
    else if (holds_alternative<string>(v))
        os << get<string>(v);
    else if (holds_alternative<TagType>(v))
        os << get<TagType>(v);
    else if (holds_alternative<bool>(v))
        os << get<bool>(v);
    else
        throw DebugError("Unknown type");
    return os;
}

bool Fern::FernType::truthValue() {
    if (holds_alternative<int>(*this))
        return get<int>(*this) != 0;
    else if (holds_alternative<string>(*this))
        return !get<string>(*this).empty();
    else if (holds_alternative<TagType>(*this))
        return true;
    else if (holds_alternative<bool>(*this))
        return get<bool>(*this);
    else
        throw DebugError("Unknown type for truth value");
}

/******************************************************************************
 * unary operators
 ******************************************************************************/

Fern::FernType Fern::FernType::operator+() {
    // not valid for strings or tag literals
    if (holds_alternative<int>(*this) || holds_alternative<bool>(*this)) // int or bool
        return *this;
    else if (holds_alternative<string>(*this)) // string
        throw SemanticError("Unary + cannot be used for string");
    else if (holds_alternative<TagType>(*this)) // tag
        throw SemanticError("Unary + cannot be used for tag literal");
    else
        throw DebugError("Unknown type for unary +");
}

Fern::FernType Fern::FernType::operator-() {
    // not valid for strings or tag literals
    if (holds_alternative<int>(*this)) // int
        return -get<int>(*this);
    else if (holds_alternative<bool>(*this)) // bool
        return get<bool>(*this);
    else if (holds_alternative<string>(*this)) // string
        throw SemanticError("Unary - cannot be used for string");
    else if (holds_alternative<TagType>(*this)) // tag
        throw SemanticError("Unary - cannot be used for tag literal");
    else
        throw DebugError("Unknown type for unary -");
}

Fern::FernType Fern::FernType::operator!() {
    return !this->truthValue();
}

Fern::FernType Fern::FernType::operator~() {
    // not valid for strings or tag literals
    if (holds_alternative<int>(*this)) // int
        return ~get<int>(*this);
    else if (holds_alternative<bool>(*this)) // bool
        return true;
    else if (holds_alternative<string>(*this)) // string
        throw SemanticError("Unary ~ cannot be used for string");
    else if (holds_alternative<TagType>(*this)) // tag
        throw SemanticError("Unary ~ cannot be used for tag literal");
    else
        throw DebugError("Unknown type for unary ~");
}

/******************************************************************************
 * binary operators
 ******************************************************************************/

Fern::FernType Fern::FernType::operator+(Fern::FernType right) {
    if (holds_alternative<int>(*this) && holds_alternative<int>(right)) // int + int
        return get<int>(*this) + get<int>(right);
    else if (holds_alternative<bool>(*this)) // bool
        return true;
    else if (holds_alternative<string>(*this)) // string
        throw SemanticError("Unary ~ cannot be used for string");
    else if (holds_alternative<TagType>(*this)) // tag
        throw SemanticError("Unary ~ cannot be used for tag literal");
    else
        throw DebugError("Unknown type for unary ~");
}