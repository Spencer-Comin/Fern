//
// Created by Spencer Comin on 2021-03-11.
//

#ifndef FERN_DRIVER_H
#define FERN_DRIVER_H

#include <string>
#include <cstddef>
#include <istream>

#include "Scanner.h"
#include "Fern.tab.hh"

namespace Fern {

    class Driver {

    public:

        Driver() = default;

        virtual ~Driver();

        void parse(const char *filename);

        void parse(std::istream &iss);

        // build parse tree stuff
        void add_upper();

        void add_lower();

        void add_word(const std::string &word);

        void add_newline();

        void add_char();

        void report(const std::string &msg);

        void report_error(const std::string &msg) const;

        std::ostream &print(std::ostream &stream) const;

    private:
        void parse_helper(std::istream &stream);

        std::size_t chars = 0;
        std::size_t words = 0;
        std::size_t lines = 1;
        std::size_t uppercase = 0;
        std::size_t lowercase = 0;

        Fern::Parser *parser = nullptr;
        Fern::Scanner *scanner = nullptr;
    };

}

#endif //FERN_DRIVER_H
