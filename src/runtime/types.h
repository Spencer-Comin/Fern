//
// Created by Spencer Comin on 2021-04-01.
//

#ifndef FERN_TYPES_H
#define FERN_TYPES_H

//#include "AST.h"

#include <iostream>
#include <variant>

using std::string;
using std::ostream;

namespace Fern {
    // forward declaration of Literal to make stuff work
    class Literal;

    //typedef string TagType;
    class TagType : public string {
        using string::string;
    public:
        explicit TagType(const string &s) : string(s) {}
    };

    struct Boolean {
        bool value;

        Boolean &operator=(Boolean const &) = default;

        Boolean &operator=(bool const &b) {
            value = b;
            return *this;
        }

        explicit operator bool() const { return value; }
    };

    const Boolean True{true};
    const Boolean False{false};

    class FernType : public std::variant<std::monostate, Boolean, int, string, TagType> {
    public:

        using variant::variant;

        FernType &operator=(FernType const &) = default;

        explicit FernType(Literal &ast_literal);

        ~FernType() = default;

        virtual bool truthValue();

        // unary operators

        FernType operator+();

        FernType operator-();

        FernType operator~();

        FernType operator!();

        // binary operators

        FernType operator&(FernType right);

        FernType operator|(FernType right);

        FernType operator^(FernType right);

        FernType operator+(FernType right);

        FernType operator-(FernType right);

        FernType operator*(FernType right);

        FernType operator/(FernType right);

        FernType operator%(FernType right);

        Boolean operator==(FernType right);

        Boolean operator!=(FernType right);

        Boolean operator<(FernType right);

        Boolean operator>(FernType right);

        Boolean operator<=(FernType right);

        Boolean operator>=(FernType right);

        Boolean operator&&(FernType right);

        Boolean operator||(FernType right);

        friend ostream &operator<<(ostream &os, const FernType &v);

    private:

    };
}

#endif //FERN_TYPES_H
