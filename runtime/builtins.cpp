#include <cstdint>
#include <iostream>

#define MAKE_PUT(fern_type, c_type)                         \
    void *put ## fern_type (void *stream, c_type x) {       \
        *static_cast<std::ostream*>(stream) << x << '\n';   \
        return stream;                                      \
    }

extern "C" {

// debug symbols
void *debug() { return static_cast<void *>(&std::cerr); }

// IO functions
MAKE_PUT(Int8, int8_t)
MAKE_PUT(Int16, int16_t)
MAKE_PUT(Int32, int32_t)
MAKE_PUT(Int64, int64_t)

MAKE_PUT(UInt8, uint8_t)
MAKE_PUT(UInt16, uint16_t)
MAKE_PUT(UInt32, uint32_t)
MAKE_PUT(UInt64, uint64_t)

MAKE_PUT(Float32, float)
MAKE_PUT(Float64, double)
MAKE_PUT(Float128, long double)

}