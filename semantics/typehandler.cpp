#include "typehandler.h"
#include <iostream>

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

FunctionType *TypeHandler::build_morphism(Type *result, Type *param) {
    if (param->isVoidTy()) {
        return FunctionType::get(result, false);
    } else {
        return FunctionType::get(result, {param}, false);
    }
}

Type *TypeHandler::build_product(vector<Type *> members) {
    for (auto &member : members) {
        if (member->isFunctionTy())
            member = PointerType::getUnqual(member);
    }
    return StructType::get(*context, members, true);
}

Type *TypeHandler::build_product(vector<Type *> members, string &&name ) {
    for (auto &member : members) {
        if (member->isFunctionTy())
            member = PointerType::getUnqual(member);
    }
    return StructType::create(*context, members, name, true);
}

Type *TypeHandler::build_ref(Type *type) {
    return PointerType::getUnqual(type);
}