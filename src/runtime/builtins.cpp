//
// Created by Spencer Comin on 2021-04-12.
//

#include "builtins.h"

Fern::Builtins::Output::Output(ostream &os) : out(os) {
    conditions.emplace_back("value");
}

Fern::Builtins::Input::Input(istream &is) : in(is) {
    conditions.emplace_back("value");
}
