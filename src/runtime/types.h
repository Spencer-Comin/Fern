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
        explicit TagType(const string& s) : string(s) {}
    };

    class FernType : public std::variant<int, bool, string, TagType> {
    public:

        using variant::variant;

        explicit FernType(Literal &ast_literal);

        FernType &operator=(const FernType &);

        FernType operator+();

        FernType operator-();

        FernType operator~();

        FernType operator!();

        FernType operator&(FernType left);
        FernType operator|(FernType left);
        FernType operator^(FernType left);
        FernType operator+(FernType left);
        FernType operator-(FernType left);
        FernType operator*(FernType left);
        FernType operator/(FernType left);
        FernType operator%(FernType left);
        FernType operator==(FernType left);
        FernType operator!=(FernType left);
        FernType operator<(FernType left);
        FernType operator>(FernType left);
        FernType operator<=(FernType left);
        FernType operator>=(FernType left);

        friend ostream& operator<<(ostream& os, const FernType& v);

        ~FernType() = default;

        bool truthValue();
    };
}

#endif //FERN_TYPES_H
