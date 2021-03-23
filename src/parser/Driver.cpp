//
// Created by Spencer Comin on 2021-03-11.
//

#include "Driver.h"

#include <fstream>
#include <cassert>
#include <iostream>
#include <stdexcept>

Fern::Driver::~Driver() {
    delete (scanner);
    scanner = nullptr;
    delete (parser);
    parser = nullptr;
}

void Fern::Driver::parse(const char *const filename) {
    assert(filename != nullptr);
    std::ifstream in_file(filename);
    if (in_file.good()) {
        parse_helper(in_file);
    }
}

void Fern::Driver::parse(std::istream &stream) {
    if (stream.good() || !stream.eof()) {
        parse_helper(stream);
    }
}

void Fern::Driver::parse_helper(std::istream &stream) {
    delete (scanner);
    try {
        scanner = new Fern::Scanner(&stream);
    } catch (std::bad_alloc &ba) {
        std::cerr << "Failed to allocate scanner: (" << ba.what()
                  << "), exiting!!!\n";
        exit(EXIT_FAILURE);
    }

    delete (parser);
    try {
        parser = new Fern::Parser(*scanner, *this);
    } catch (std::bad_alloc &ba) {
        std::cerr << "Failed to allocate parser: (" << ba.what()
                  << "), exiting!!!\n";
        exit(EXIT_FAILURE);
    }

    const int accept(0);
    if (parser->parse() != accept) {
        std::cerr << "Parse failed!!!\n";
        exit(EXIT_FAILURE);
    }
}

void Fern::Driver::report_error(const std::string &msg) const {
    std::cerr << msg << " at line " << scanner->lineno() << " \n";
}


void Fern::Driver::report(const std::string &msg) {
    std::cout << scanner->lineno() << ": " << msg << "\n";
}

void Fern::Driver::setASTRoot(Fern::ASTNode *new_root) {
    root = new_root;
}

void Fern::Driver::printAST() {
    std::cout << *root;
}

void Fern::Driver::killAST() {
    delete root;
}
