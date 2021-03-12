//
// Created by Spencer Comin on 2021-03-10.
//

#ifndef FERN_SCANNER_H
#define FERN_SCANNER_H

#if !defined(yyFlexLexerOnce)

#include <FlexLexer.h>

#endif

#include "Fern.tab.hh"
#include "location.hh"


namespace Fern {

    class Scanner : public yyFlexLexer {
    public:
        explicit Scanner(std::istream *in) : yyFlexLexer(in) {
            loc = new Fern::Parser::location_type();
        }

        using FlexLexer::yylex;

        int yylex(Fern::Parser::semantic_type *const lval,
                  Fern::Parser::location_type *location);

    private:
        Fern::Parser::semantic_type *yyval = nullptr;
        Fern::Parser::location_type *loc = nullptr;
    };

}

#endif //FERN_SCANNER_H
