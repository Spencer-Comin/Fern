//
// Created by Spencer Comin on 2021-03-16.
//

#ifndef FERN_OPERATOR_H
#define FERN_OPERATOR_H

#include <string>

namespace Fern {
    enum class Operator {
        // bitwise operators
        AND, OR, XOR,

        // addition operators
        PLUS, MINUS,

        // multiplicative operators
        STAR, SLASH, MODULO,

        // comparison operators
        TRIPLE_EQUAL, DOUBLE_EQUAL, BANG_EQUAL,
        LT, GT, LT_EQUAL, GT_EQUAL, TILDE,

        // logic operators
        DOUBLE_AND, DOUBLE_OR, BANG,

        // utility operators
        EQUAL, WALRUS, DOT, DECISION, ITERATION, VISIT, AT
    };

    std::string opToString(Operator op);
}

#endif //FERN_OPERATOR_H
