cmake_minimum_required(VERSION 3.0.0)
project(DummyLanguage VERSION 0.1.0)

set(CMAKE_CXX_STANDARD 14)

include(CTest)
enable_testing()


# SWI-Prolog setup
find_library(SWIPL_LIB swipl
             HINTS "/usr/local/Cellar/swi-prolog/8.4.2/libexec/lib/swipl/lib/x86_64-darwin"
             REQUIRED)
set(SWIPL_DIR "/usr/local/Cellar/swi-prolog/8.4.2/libexec/lib/cmake/swipl")
set(SWIPL_INCLUDE_DIRS "/usr/local/Cellar/swi-prolog/8.4.2/libexec/lib/swipl/include")
find_package(SWIPL REQUIRED CONFIG)

# LLVM setup
set(LLVM_DIR "/usr/local/Cellar/llvm/13.0.0_2/lib/cmake/llvm")
find_package(LLVM REQUIRED CONFIG)
message(STATUS "Found LLVM ${LLVM_PACKAGE_VERSION}")
message(STATUS "Using LLVMConfig.cmake in: ${LLVM_DIR}")
include_directories(${LLVM_INCLUDE_DIRS})
add_definitions(${LLVM_DEFINITIONS_LIST})
list(APPEND CMAKE_MODULE_PATH "${LLVM_CMAKE_DIR}")
include(AddLLVM)
set(LLVM_LINK_COMPONENTS all)  # This may come up later once I switch from JIT to AOT
llvm_map_components_to_libnames(LLVM_LIBS support core orcjit native all
    aarch64asmparser     aarch64codegen     aarch64desc     aarch64disassembler     aarch64info     aarch64utils
    amdgpuasmparser      amdgpucodegen      amdgpudesc      amdgpudisassembler      amdgpuinfo      amdgpuutils
    armasmparser         armcodegen         armdesc         armdisassembler         arminfo         armutils
    avrasmparser         avrcodegen         avrdesc         avrdisassembler         avrinfo
    bpfasmparser         bpfcodegen         bpfdesc         bpfdisassembler         bpfinfo
    hexagonasmparser     hexagoncodegen     hexagondesc     hexagondisassembler     hexagoninfo
    lanaiasmparser       lanaicodegen       lanaidesc       lanaidisassembler       lanaiinfo
    msp430asmparser      msp430codegen      msp430desc      msp430disassembler      msp430info
    mipsasmparser        mipscodegen        mipsdesc        mipsdisassembler        mipsinfo
    nvptxcodegen         nvptxdesc          nvptxinfo
    powerpcasmparser     powerpccodegen     powerpcdesc     powerpcdisassembler     powerpcinfo
    riscvasmparser       riscvcodegen       riscvdesc       riscvdisassembler       riscvinfo
    sparcasmparser       sparccodegen       sparcdesc       sparcdisassembler       sparcinfo
    systemzasmparser     systemzcodegen     systemzdesc     systemzdisassembler     systemzinfo
    webassemblyasmparser webassemblycodegen webassemblydesc webassemblydisassembler webassemblyinfo webassemblyutils
                         xcorecodegen       xcoredesc       xcoredisassembler       xcoreinfo)

set(CODEGEN_SOURCES codegen/interface.cpp codegen/codegen.cpp semantics/typehandler.cpp)
set(RUNTIME_SOURCES runtime/std.cpp runtime/memory.cpp)

add_library(codegen SHARED ${CODEGEN_SOURCES} ${RUNTIME_SOURCES})

set_target_properties(codegen PROPERTIES
                      LIBRARY_OUTPUT_DIRECTORY ${CMAKE_SOURCE_DIR}/codegen)
target_include_directories(codegen PUBLIC ${SWIPL_INCLUDE_DIRS} ${LLVM_INCLUDE_DIRS})
target_link_libraries(codegen ${SWIPL_LIB} ${LLVM_LIBS})

set(CPACK_PROJECT_NAME ${PROJECT_NAME})
set(CPACK_PROJECT_VERSION ${PROJECT_VERSION})
include(CPack)
