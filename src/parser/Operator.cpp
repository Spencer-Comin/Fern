//
// Created by Spencer Comin on 2021-03-16.
//
#include "Operator.h"

namespace Fern {
    std::string opToString(Operator op) {
        switch (op) {
            case Operator::AND:
                return "AND &";
            case Operator::OR:
                return "OR |";
            case Operator::XOR:
                return "XOR ^";
            case Operator::PLUS:
                return "PLUS +";
            case Operator::MINUS:
                return "MINUS -";
            case Operator::STAR:
                return "STAR *";
            case Operator::SLASH:
                return "SLASH /";
            case Operator::MODULO:
                return "MODULO %";
            case Operator::TRIPLE_EQUAL:
                return "TRIPLE_EQUAL ===";
            case Operator::DOUBLE_EQUAL:
                return "DOUBLE_EQUAL ==";
            case Operator::BANG_EQUAL:
                return "BANG_EQUAL !=";
            case Operator::LT:
                return "LT <";
            case Operator::GT:
                return "GT >";
            case Operator::LT_EQUAL:
                return "LT_EQUAL <=";
            case Operator::GT_EQUAL:
                return "GT_EQUAL >=";
            case Operator::TILDE:
                return "TILDE ~";
            case Operator::DOUBLE_AND:
                return "DOUBLE_AND &&";
            case Operator::DOUBLE_OR:
                return "DOUBLE_OR ||";
            case Operator::BANG:
                return "BANG !";
            default:
                return "UNRECOGNIZED OPERATOR";
        }
    }
}