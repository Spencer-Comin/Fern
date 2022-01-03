#include "llvm/IR/Function.h"
#include "llvm/IR/Type.h"
#include "llvm/IR/DerivedTypes.h"
#include <unordered_map>
#include <string>
#include <vector>

using namespace llvm;

using std::string;
using std::vector;
using type_map = std::unordered_map<string, Type *>;

class TypeHandler {
    // TODO: move type_maps into Prolog?
    type_map typedefs;
	type_map type_assignments;
    LLVMContext *context;

public:
    TypeHandler() = default;
    ~TypeHandler() = default;
    void set_context(LLVMContext *ctx);

    Type *get_type(string name);
    void register_type(string name, Type *type);
    void assign_type(string name, Type *type);
    Type *build_morphism(Type *result, Type *param);
    Type *build_product(vector<Type *> members);
    Type *build_ref(Type *type);
};