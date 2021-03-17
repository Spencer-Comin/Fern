//
// Created by Spencer Comin on 2021-03-11.
//

#include "Driver.h"

#include <cctype>
#include <fstream>
#include <cassert>
#include <iostream>

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
    }
}

void Fern::Driver::add_upper() {
    uppercase++;
    chars++;
    words++;
    std::cout << lines << ": UPPER\n";
}

void Fern::Driver::add_lower() {
    lowercase++;
    chars++;
    words++;
    std::cout << lines << ": LOWER\n";
}

void Fern::Driver::add_word(const std::string &word) {
    words++;
    chars += word.length();
    for (const char &c : word) {
        if (islower(c)) {
            lowercase++;
        } else if (isupper(c)) {
            uppercase++;
        }
    }
    std::cout << lines << ": WORD \"" << word << "\"\n";
}

void Fern::Driver::add_newline() {
    lines++;
    chars++;
}

void Fern::Driver::add_char() {
    chars++;
    std::cout << lines << ": CHAR\n";
}

std::ostream &Fern::Driver::print(std::ostream &stream) const {
    stream << "Results: " << "\n";
    stream << "Uppercase: " << uppercase << "\n";
    stream << "Lowercase: " << lowercase << "\n";
    stream << "Lines: " << lines << "\n";
    stream << "Words: " << words << "\n";
    stream << "Characters: " << chars << "\n";
    return stream;
}

void Fern::Driver::report_error(const std::string &msg) const {
    std::cerr << msg << " at line " << scanner->lineno() << " \n";
}


void Fern::Driver::report(const std::string &msg) {
    std::cout << scanner->lineno() << ": " << msg << "\n";
}
