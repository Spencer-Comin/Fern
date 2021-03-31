set(CMAKE_CXX_FLAGS  "${CMAKE_CXX_FLAGS} -Wall -Wno-deprecated-register")
cmake_minimum_required(VERSION 3.19)
project(Fern CXX)

set(CMAKE_CXX_STANDARD 20)
set(FLEX_INCLUDE_DIR /usr/local/opt/flex/include)

include_directories(src/parser src/scanner src/interpreter src/semantics)

find_package(BISON REQUIRED)
find_package(FLEX REQUIRED)

BISON_TARGET(parser
        src/parser/Fern.yy
        ${CMAKE_CURRENT_BINARY_DIR}/Fern.tab.cc
        COMPILE_FLAGS -Wcounterexamples
        VERBOSE)

FLEX_TARGET(scanner
        src/scanner/Fern.lex
        ${CMAKE_CURRENT_BINARY_DIR}/Fern.yy.cc)

ADD_FLEX_BISON_DEPENDENCY(scanner parser)

add_executable(Fern src/main.cpp
        src/Driver.cpp
        ${FLEX_scanner_OUTPUTS}
        ${BISON_parser_OUTPUTS} src/parser/Operator.h src/parser/Operator.cpp src/parser/AST.cpp src/parser/AST.h src/interpreter/Interpreter.cpp src/interpreter/Interpreter.h src/parser/ASTPrinter.cpp src/parser/ASTPrinter.h)

target_include_directories(Fern
        PRIVATE
            ${CMAKE_CURRENT_SOURCE_DIR}
            ${FLEX_INCLUDE_DIRS}
            ${CMAKE_CURRENT_BINARY_DIR})

set_property(TARGET Fern
        PROPERTY CXX_STANDARD 14)