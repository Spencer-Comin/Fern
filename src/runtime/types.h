//
// Created by Spencer Comin on 2021-04-01.
//

#ifndef FERN_TYPES_H
#define FERN_TYPES_H

//#include "AST.h"

#include <string>
#include <variant>

using std::string;

namespace Fern {
    // forward declaration of Literal to make stuff work
    class Literal;

    typedef string tagType;

    class FernType {
    public:
        enum Type {
            STRING,
            NUMBER,
            BOOL,
            TAG_LITERAL
        } type;

//        union {
        int numberVal;
        bool boolVal;
        string stringVal;
        tagType tagVal;
//        };
        //std::variant<int, bool, string, tagType> val;

        explicit FernType(Literal &ast_literal);

        FernType &operator=(const FernType &);

        FernType operator+();

        FernType operator-();

        FernType operator~();

        FernType operator!();

        ~FernType() = default;

        bool truthValue() const;
    };
}

#endif //FERN_TYPES_H
