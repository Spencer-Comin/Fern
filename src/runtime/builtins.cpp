//
// Created by Spencer Comin on 2021-04-12.
//

#include "builtins.h"

Fern::Builtins::Output::Output(ostream &os) : ID("output"), out(os) {
    conditions.emplace_back("value");
    evaluationIndex = 1;
}

Fern::Builtins::Input::Input(istream &is) : ID("input"), in(is) {
    conditions.emplace_back("value");
    evaluationIndex = 1;
}
