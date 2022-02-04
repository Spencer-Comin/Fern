#include "typehandler.h"

void TypeHandler::set_context(LLVMContext *ctx) {
    context = ctx;
    typedefs["Float16"] = Type::getHalfTy(*ctx);
    typedefs["Float32"] = Type::getFloatTy(*ctx);
	typedefs["Float64"] = Type::getDoubleTy(*ctx);

	typedefs["Int8"] = IntegerType::getInt8Ty(*ctx);
	typedefs["Int16"] = IntegerType::getInt16Ty(*ctx);
	typedefs["Int32"] = IntegerType::getInt32Ty(*ctx);
	typedefs["Int64"] = IntegerType::getInt64Ty(*ctx);

	typedefs["Byte"] = IntegerType::getInt8Ty(*ctx);
	typedefs["Bool"] = IntegerType::getInt1Ty(*ctx);

    typedefs["IO"] = Type::getInt8PtrTy(*ctx);
    typedefs["End"] = Type::getVoidTy(*ctx);
}

Type *TypeHandler::get_type(string name) {
    return typedefs[name];
}

void TypeHandler::register_type(string name, Type *type) {
    typedefs[name] = type;
}

void TypeHandler::assign_type(string name, Type *type) {
    type_assignments[name] = type;
}

Type *TypeHandler::build_morphism(Type *result, Type *param) {
    // probably need some rules about destructuring based on param type size...
    // check max number of args that can be passed to C function
    if (param->isVoidTy()) {
        return FunctionType::get(result, false);
    } else if (param->isStructTy()) {
        return FunctionType::get(result, static_cast<StructType *>(param)->elements(), false);
    } else if (param->isArrayTy()) {
        vector<Type *> params(param->getArrayNumElements(), param->getArrayElementType());
        return FunctionType::get(result, params, false);
    } else {
        return FunctionType::get(result, {param}, false);
    }
}

Type *TypeHandler::build_product(vector<Type *> members) {
    return StructType::get(*context, members, true);
}

Type *TypeHandler::build_ref(Type *type) {
    return PointerType::get(type, 0);
}