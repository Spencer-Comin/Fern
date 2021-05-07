//
// Created by Spencer Comin on 2021-04-12.
//

#ifndef FERN_BUILTINS_H
#define FERN_BUILTINS_H

#include "AST.h"

#include <iostream>

using std::ostream;
using std::istream;

namespace Fern::Builtins {
        class Output : public Block {
            ostream &out;
            explicit Output(ostream&);
        };

        class Input : public Block {
            istream &in;
            explicit Input(istream&);
        };
}


#endif //FERN_BUILTINS_H
