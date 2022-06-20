#include <cstdlib>
#include <iostream>
#include <sys/mman.h>
#include <stack>
#include <deque>
#include <unordered_map>
#include <libkern/OSCacheControl.h>

#define PAGE_SIZE 4096
#define BLOCK_MASK ~(PAGE_SIZE-1)

static class {
private:
    struct Trampoline
    {
        uint8_t tramp[128];
    };

    std::unordered_map<void *, unsigned> in_use_by_block {}; // could this be replaced by a reference count?
    std::stack<Trampoline *> available{};

    inline static void *get_block(Trampoline *tramp) {
        return reinterpret_cast<void *>(reinterpret_cast<uintptr_t>(tramp) & BLOCK_MASK);
    }

public:
    void allocate_block() {
        void *memory_block = mmap(nullptr, PAGE_SIZE, PROT_READ|PROT_WRITE, MAP_PRIVATE|MAP_ANON, -1, 0);
        in_use_by_block[memory_block] = 0;

        Trampoline *fencepost = static_cast<Trampoline *>(memory_block) + PAGE_SIZE/sizeof(Trampoline);
        for (Trampoline *current = static_cast<Trampoline *>(memory_block); current < fencepost; current++)
            available.push(current);
    }

    void *get_trampoline() {
        if (available.empty())
            allocate_block();

        // pop from available
        Trampoline *tramp = available.top();
        available.pop();

        // increment use count of block
        in_use_by_block[get_block(tramp)]++;
        
        pthread_jit_write_protect_np(false);
        mprotect(get_block(tramp), PAGE_SIZE, PROT_READ|PROT_WRITE);
        return static_cast<void *>(tramp);
    }

    void make_tramp_executable(void *tramp) {
        pthread_jit_write_protect_np(true);
        mprotect(get_block(reinterpret_cast<Trampoline *>(tramp)), PAGE_SIZE, PROT_READ|PROT_EXEC);
        sys_icache_invalidate(tramp, sizeof(Trampoline *));
    }

    void free_trampoline(void *tramp_ptr) {
        // find and decrement use count of block
        Trampoline *tramp = static_cast<Trampoline *>(tramp_ptr);
        void *block = get_block(tramp);
        in_use_by_block[block]--;

        // push onto available
        available.push(tramp);

        if (in_use_by_block[block] == 0)
        {
            in_use_by_block.erase(block);
            // remove all trampolines in unused_block from available
            std::deque<Trampoline *> temp {};
            while (!available.empty()) {
                tramp = available.top();
                available.pop();
                if (get_block(tramp) != block)
                    temp.push_front(tramp);
            }
            available = std::stack<Trampoline *>(std::move(temp));
            munmap(block, PAGE_SIZE); // error handling?
        }
    }
} trampoline_allocator;

extern "C" {

// maybe add some reference counting

void heap_free(void *ptr) {
    std::free(ptr);
}

void *heap_allocate(size_t size) {
    return std::malloc(size);
}

void trampoline_free(void *ptr) {
    trampoline_allocator.free_trampoline(ptr);
}

void *trampoline_allocate() {
    return trampoline_allocator.get_trampoline();
}

void make_tramp_executable(void *tramp) {
    trampoline_allocator.make_tramp_executable(tramp);
}
}