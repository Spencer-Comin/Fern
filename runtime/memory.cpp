#include <cstdlib>
#include <iostream>

extern "C" {

// maybe add some reference counting

void heap_free(void *ptr) {
    std::free(ptr);
}

void *heap_allocate(size_t size) {
    return std::malloc(size);
}

}