#include <cstdint>
#include <iostream>

using IO       = void *;
using Int8     = int8_t;
using Int16    = int16_t;
using Int32    = int32_t;
using Int64    = int64_t;
using UInt8    = uint8_t;
using UInt16   = uint16_t;
using UInt32   = uint32_t;
using UInt64   = uint64_t;
using Float32  = float;
using Float64  = double;
using Float128 = long double;

template <class T>
struct __attribute__((packed)) put_param {
    void *stream;
    T x;
};

#define MAKE_PUT(T)                                                   \
    IO put##T(put_param<T> *p)                                        \
    {                                                                 \
        *static_cast<std::ostream *>(p->stream) << p->x << std::endl; \
        return p->stream;                                             \
    }

extern "C" {

// debug symbols
IO debug() { return static_cast<IO>(&std::cerr); }

// IO functions
MAKE_PUT(Int8)
MAKE_PUT(Int16)
MAKE_PUT(Int32)
MAKE_PUT(Int64)
MAKE_PUT(UInt8)
MAKE_PUT(UInt16)
MAKE_PUT(UInt32)
MAKE_PUT(UInt64)
MAKE_PUT(Float32)
MAKE_PUT(Float64)
MAKE_PUT(Float128)

}