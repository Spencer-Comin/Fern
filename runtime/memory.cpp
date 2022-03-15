#include <cstdlib>
#include <iostream>

extern "C" {

// maybe add some reference counting
// TODO: push all masking and unmasking into LLVM generation

void heap_free(void *ptr) {
    std::free(ptr);
}

// TODO: rename to heap_alloc since this only allocates space, not copies
void *heap_allocate(size_t size) {
    return std::malloc(size);
}

}