//
// Created by Spencer Comin on 2021-04-13.
//

#include "errors.h"

const char *Fern::FernError::what() const noexcept {
    return msg;
}
