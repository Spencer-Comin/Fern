//
// Created by Spencer Comin on 2021-04-01.
//

#include "types.h"
#include "errors.h"
#include "AST.h"

using std::holds_alternative;
using std::get;
using std::visit;

template<class... Ts>
struct overload : Ts ... {
    using Ts::operator()...;
};
template<class... Ts> overload(Ts...) -> overload<Ts...>;

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
    else if (holds_alternative<Boolean>(v))
        os << (get<Boolean>(v).value ? "true" : "false");
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
    else if (holds_alternative<Boolean>(*this))
        return get<Boolean>(*this).value;
    else
        throw DebugError("Unknown type for truth value");
}

/******************************************************************************
 * unary operators
 ******************************************************************************/

//Fern::FernType Fern::FernType::operator+() {
//    // not valid for strings or tag literals
//    if (holds_alternative<int>(*this) || holds_alternative<Boolean>(*this)) // int or bool
//        return *this;
//    else if (holds_alternative<string>(*this)) // string
//        throw SemanticError("Unary + cannot be used with string");
//    else if (holds_alternative<TagType>(*this)) // tag
//        throw SemanticError("Unary + cannot be used with tag literal");
//    else
//        throw DebugError("Unknown type for unary +");
//}

Fern::FernType Fern::FernType::operator+() {
    // not valid for strings or tag literals
    return std::visit(overload{
            [](int &i) -> FernType { return i; },
            [](string &) -> FernType { throw SemanticError("Unary + cannot be used with string"); },
            [](TagType &) -> FernType { throw SemanticError("Unary + cannot be used with tag literal"); },
            [](Boolean &b) -> FernType { return b; },
            [](auto &&) -> FernType { throw DebugError("unrecognized type"); }
    }, *this);
}

Fern::FernType Fern::FernType::operator-() {
    // not valid for strings or tag literals
    return std::visit(overload{
            [](int &i) -> FernType { return -i; },
            [](string &) -> FernType {
                throw SemanticError("Unary - cannot be used with string");
            },
            [](TagType &) -> FernType {
                throw SemanticError("Unary - cannot be used with tag literal");
            },
            [](Boolean &b) -> FernType { return b; },
            [](auto &&) -> FernType { throw DebugError("unrecognized type"); }
    }, *this);
}

Fern::FernType Fern::FernType::operator!() {
    return !this->truthValue();
}

Fern::FernType Fern::FernType::operator~() {
    // not valid for strings or tag literals
    return std::visit(overload{
            [](int &i) -> FernType { return ~i; },
            [](string &) -> FernType {
                throw SemanticError("Unary ~ cannot be used with string");
            },
            [](TagType &) -> FernType {
                throw SemanticError("Unary ~ cannot be used with tag literal");
            },
            [](Boolean &b) -> FernType { return True; },
            [](auto &&) -> FernType { throw DebugError("unrecognized type"); }
    }, *this);
}

/******************************************************************************
 * binary operators
 ******************************************************************************/

Fern::FernType Fern::FernType::operator+(Fern::FernType right) {
    if (holds_alternative<int>(*this) && holds_alternative<int>(right)) // int + int
        return get<int>(*this) + get<int>(right);
    else if (holds_alternative<Boolean>(*this) || holds_alternative<Boolean>(right)) // bool
        throw SemanticError("Binary + cannot be used with bool");
    else if (holds_alternative<string>(*this) && holds_alternative<string>(right)) // string + string
        return get<string>(*this) + get<string>(right);
    else if (holds_alternative<TagType>(*this) || holds_alternative<TagType>(right)) // tag
        throw SemanticError("Binary + cannot be used with tag literal");
    else
        throw SemanticError("Type mismatch for binary +");
}


Fern::FernType Fern::FernType::operator-(Fern::FernType right) {
    if (holds_alternative<int>(*this) && holds_alternative<int>(right)) // int + int
        return get<int>(*this) - get<int>(right);
    else
        throw SemanticError("Bad typing for binary -");
}

Fern::FernType Fern::FernType::operator*(Fern::FernType right) {
    if (holds_alternative<int>(*this) && holds_alternative<int>(right)) // int + int
        return get<int>(*this) * get<int>(right);
    else
        throw SemanticError("Bad typing for *");
}

Fern::FernType Fern::FernType::operator/(Fern::FernType right) {
    if (holds_alternative<int>(*this) && holds_alternative<int>(right)) // int + int
        return get<int>(*this) / get<int>(right);
    else
        throw SemanticError("Bad typing for /");
}

Fern::FernType Fern::FernType::operator%(Fern::FernType right) {
    if (holds_alternative<int>(*this) && holds_alternative<int>(right)) // int + int
        return get<int>(*this) % get<int>(right);
    else
        throw SemanticError("Bad typing for %");
}

Fern::FernType Fern::FernType::operator&(Fern::FernType right) {
    if (holds_alternative<int>(*this) && holds_alternative<int>(right)) // int + int
        return get<int>(*this) & get<int>(right);
    else
        throw SemanticError("Bad typing for &");
}

Fern::FernType Fern::FernType::operator|(Fern::FernType right) {
    if (holds_alternative<int>(*this) && holds_alternative<int>(right)) // int + int
        return get<int>(*this) | get<int>(right);
    else
        throw SemanticError("Bad typing for |");
}

Fern::FernType Fern::FernType::operator^(Fern::FernType right) {
    if (holds_alternative<int>(*this) && holds_alternative<int>(right)) // int + int
        return get<int>(*this) ^ get<int>(right);
    else
        throw SemanticError("Bad typing for ^");
}

Fern::Boolean Fern::FernType::operator==(Fern::FernType right) {
    if (holds_alternative<int>(*this) && holds_alternative<int>(right)) // int + int
        return {get<int>(*this) == get<int>(right)};
    else if (holds_alternative<Boolean>(*this) && holds_alternative<Boolean>(right)) // bool
        return {get<Boolean>(*this).value == get<Boolean>(right).value};
    else if (holds_alternative<string>(*this) && holds_alternative<string>(right)) // string + string
        return {get<string>(*this) == get<string>(right)};
    else if (holds_alternative<TagType>(*this) && holds_alternative<TagType>(right)) // tag
        return {get<TagType>(*this) == get<TagType>(right)};
    else
        return False; // unequal types, return false
}

Fern::Boolean Fern::FernType::operator!=(Fern::FernType right) {
    if (holds_alternative<int>(*this) && holds_alternative<int>(right)) // int == int
        return {get<int>(*this) != get<int>(right)};
    else if (holds_alternative<Boolean>(*this) && holds_alternative<Boolean>(right)) // bool == bool
        return {get<Boolean>(*this).value != get<Boolean>(right).value};
    else if (holds_alternative<string>(*this) && holds_alternative<string>(right)) // string == string
        return {get<string>(*this) != get<string>(right)};
    else if (holds_alternative<TagType>(*this) && holds_alternative<TagType>(right)) // tag == tag
        return {get<TagType>(*this) != get<TagType>(right)};
    else
        return True; // unequal types, return true
}

Fern::Boolean Fern::FernType::operator<(Fern::FernType right) {
    if (holds_alternative<int>(*this) && holds_alternative<int>(right)) // int + int
        return {get<int>(*this) < get<int>(right)};
    else
        throw SemanticError("Bad typing for <");
}

Fern::Boolean Fern::FernType::operator>(Fern::FernType right) {
    if (holds_alternative<int>(*this) && holds_alternative<int>(right)) // int + int
        return {get<int>(*this) > get<int>(right)};
    else
        throw SemanticError("Bad typing for >");
}

Fern::Boolean Fern::FernType::operator<=(Fern::FernType right) {
    if (holds_alternative<int>(*this) && holds_alternative<int>(right)) // int + int
        return {get<int>(*this) <= get<int>(right)};
    else
        throw SemanticError("Bad typing for <=");
}

Fern::Boolean Fern::FernType::operator>=(Fern::FernType right) {
    if (holds_alternative<int>(*this) && holds_alternative<int>(right)) // int + int
        return {get<int>(*this) >= get<int>(right)};
    else
        throw SemanticError("Bad typing for >=");
}

Fern::Boolean Fern::FernType::operator&&(Fern::FernType right) {
    return {this->truthValue() && right.truthValue()};
}

Fern::Boolean Fern::FernType::operator||(Fern::FernType right) {
    return {this->truthValue() || right.truthValue()};
}
