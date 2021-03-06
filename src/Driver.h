//
// Created by Spencer Comin on 2021-03-11.
//

#ifndef FERN_DRIVER_H
#define FERN_DRIVER_H

#include "AST.h"

#include "Scanner.h"
#include "Fern.tab.hh"
#include "Interpreter.h"
#include "ASTPrinter.h"

#include <string>
#include <cstddef>
#include <istream>

namespace Fern {

    class Driver {

    public:

        Driver() = default;

        virtual ~Driver();

        void parse(const char *filename);

        void parse(std::istream &iss);

        // build parse tree stuff


        void report(const std::string &msg);

        void report_error(const std::string &msg) const;

        void setASTRoot(Fern::ASTNode *root);

        void printAST();

        void interpret();

        void killAST();
    private:
        void parse_helper(std::istream &stream);

        Fern::Interpreter *interpreter = nullptr;
        Fern::Parser *parser = nullptr;
        Fern::Scanner *scanner = nullptr;
        Fern::ASTPrinter *printer = nullptr;
        Fern::ASTNode *ASTRoot = nullptr;
    };

}

#endif //FERN_DRIVER_H
