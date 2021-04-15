//
// Created by Spencer Comin on 2021-04-13.
//

#ifndef FERN_ERRORS_H
#define FERN_ERRORS_H

#include <exception>
#include <string>


namespace Fern {
    class FernError : public std::exception {
    public:
        explicit FernError(const char *msg) : msg(msg) {}

    protected:
        const char *msg;

    private:
        const char *what() const noexcept override;
    };

    class RuntimeError : public FernError {
        using FernError::FernError;
    };

    class SemanticError : public FernError {
        using FernError::FernError;
    };

    class SyntaxError : public FernError {
        using FernError::FernError;
    };

    class DebugError : public FernError {
        using FernError::FernError;
    };
}


#endif //FERN_ERRORS_H
