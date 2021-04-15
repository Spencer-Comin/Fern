//
// Created by Spencer Comin on 2021-04-01.
//

#include "types.h"
#include "errors.h"

Fern::FernType &Fern::FernType::operator=(const Fern::FernType &other) {
    type = other.type;
    switch (other.type) {
        case STRING:
            stringVal = other.stringVal;
            break;
        case NUMBER:
            numberVal = other.numberVal;
            break;
        case TAG_LITERAL:
            tagVal = other.tagVal;
            break;
        case BOOL:
            boolVal = other.boolVal;
            break;
    }
    return *this;
}

bool Fern::FernType::truthValue() const {
    switch (type) {
        case STRING:
            return !stringVal.empty();
        case NUMBER:
            return numberVal != 0;
        case BOOL:
            return boolVal;
        case TAG_LITERAL:
            return true;
    }
}

Fern::FernType Fern::FernType::operator!() {
    boolVal = !truthValue();
    type = BOOL;
    return *this;
}

Fern::FernType Fern::FernType::operator~() {
    switch (type) {
        case STRING:
            throw SemanticError("~ cannot be used on a string");
        case NUMBER:
            numberVal = ~numberVal;
            return *this;
        case BOOL:
            boolVal = true;
            return *this;
        case TAG_LITERAL:
            throw SemanticError("~ cannot be used on a tag literal");
    }
}

Fern::FernType Fern::FernType::operator-() {
    switch (type) {
        case STRING:
            throw SemanticError("- cannot be used on a string");
        case NUMBER:
            numberVal = -numberVal;
            return *this;
        case BOOL:
            return *this;
        case TAG_LITERAL:
            throw SemanticError("- cannot be used on a tag literal");
    }
}

Fern::FernType Fern::FernType::operator+() {
    switch (type) {
        case STRING:
            throw SemanticError("+ cannot be used on a string");
        case NUMBER:
        case BOOL:
            return *this;
        case TAG_LITERAL:
            throw SemanticError("+ cannot be used on a tag literal");
    }
}


//Fern::FernType::FernType(Fern::FernType const &other) { // copy constructor
//    type = other.type;
//    switch (other.type) {
//        case STRING:
//            stringVal = other.stringVal;
//            break;
//        case NUMBER:
//            numberVal = other.numberVal;
//            break;
//        case TAG_LITERAL:
//            tagVal = other.tagVal;
//            break;
//    }
//}
