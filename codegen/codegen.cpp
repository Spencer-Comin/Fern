#include "codegen.h"
#include "llvm/Support/TargetRegistry.h"
#include "llvm/Transforms/Utils.h"
#include "llvm/Transforms/Vectorize.h"
#include "llvm/Transforms/AggressiveInstCombine/AggressiveInstCombine.h"
#include "llvm/Transforms/IPO.h"
#include <iostream>
#include <utility>

static ExitOnError ExitOnErr;

static inline Value *log_error_value(const std::string err_msg) {
	std::cerr << err_msg << std::endl;
	return nullptr;
}

void CodeGenerator::InitializeModuleAndPassManager() {
	// Open a new context and module.
	TheContext = std::make_unique<LLVMContext>();
	TheModule = std::make_unique<Module>("my cool jit", *TheContext);
	TheModule->setDataLayout(TheJIT->getDataLayout());
	types.set_context(TheContext.get());

	// Create a new builder for the module.
	Builder = std::make_unique<IRBuilder<>>(*TheContext);

	// Register all the basic analyses with the managers.
	PB.registerModuleAnalyses(MAM);
	PB.registerCGSCCAnalyses(CGAM);
	PB.registerFunctionAnalyses(FAM);
	PB.registerLoopAnalyses(LAM);
	PB.crossRegisterProxies(LAM, FAM, CGAM, MAM);

	MPM = PB.buildPerModuleDefaultPipeline(PassBuilder::OptimizationLevel::O2);

	// add declarations for memory runtime functions
	std::pair<string, FunctionType *> memory_funcs[] = {
		{"heap_free", types.build_morphism(Type::getVoidTy(*TheContext), Type::getInt8PtrTy(*TheContext))},
		{"heap_allocate", types.build_morphism(Type::getInt8PtrTy(*TheContext), Type::getInt64Ty(*TheContext))},
		{"trampoline_free", types.build_morphism(Type::getVoidTy(*TheContext), Type::getInt8PtrTy(*TheContext))},
		{"trampoline_allocate", types.build_morphism(Type::getInt8PtrTy(*TheContext), Type::getVoidTy(*TheContext))},
		{"make_tramp_executable", types.build_morphism(Type::getVoidTy(*TheContext), Type::getInt8PtrTy(*TheContext))}
	};
	for (auto &pair : memory_funcs) {
		string name = pair.first;
		FunctionType *ft = pair.second;
		Function *func = generate_func_declaration(std::move(name), ft);
		runtime_funcs[name] = FunctionCallee(ft, func);
	}

	// find and declare intrinsics
	string intrinsic_names[] {"llvm.init.trampoline", "llvm.adjust.trampoline"};
	for (const auto &name : intrinsic_names) {
		Intrinsic::ID id = Function::lookupIntrinsicID(name);
		Function *func = Intrinsic::getDeclaration(TheModule.get(), id);
		FunctionType *type = Intrinsic::getType(*TheContext, id);
		runtime_funcs[name] = FunctionCallee(type, func);
	}
}

FunctionCallee CodeGenerator::getFunction(std::string &&name, FunctionType *ft) {
	if (auto *func = TheModule->getFunction(name))
		return FunctionCallee(ft, func);

	// find some other way?

	return {};
}

CodeGenerator::CodeGenerator(/* args */) {
	InitializeNativeTarget();
	InitializeNativeTargetAsmPrinter();
	InitializeNativeTargetAsmParser();
	TheJIT = ExitOnErr(KaleidoscopeJIT::Create());
	InitializeModuleAndPassManager();
}

Value *CodeGenerator::generate_number(double val) {
	return ConstantFP::get(*TheContext, APFloat(val));
}

Value *CodeGenerator::generate_float(double val) {
	return ConstantFP::get(*TheContext, APFloat(val));
}

Value *CodeGenerator::generate_int(unsigned bitwidth, long val, bool issigned) {
	return ConstantInt::get(*TheContext, APInt(bitwidth, val, issigned));
}

Value *CodeGenerator::generate_fbinary(Value *left, Value *right, BinaryOp op) {
	if (!left || !right)
		return nullptr;	

	switch (op) {
	case ADD: return Builder->CreateFAdd(left, right, "sum");
	case SUB: return Builder->CreateFSub(left, right, "diff");
	case MUL: return Builder->CreateFMul(left, right, "prod");
	case DIV: return Builder->CreateFDiv(left, right, "quot");
	case LT: return Builder->CreateFCmpULT(left, right, "is_lt");
	case GT: return Builder->CreateFCmpUGT(left, right, "is_gt");

	default:
		return log_error_value("invalid binary operator");
	}
}

Value *CodeGenerator::generate_binary(Value *left, Value *right, BinaryOp op) {
	if (!left || !right)
		return nullptr;

	switch (op) {
	case ADD: return Builder->CreateAdd(left, right, "sum");
	case SUB: return Builder->CreateSub(left, right, "diff");
	case MUL: return Builder->CreateMul(left, right, "prod");
	case DIV: return Builder->CreateSDiv(left, right, "quot");
	case LT: return Builder->CreateICmpULT(left, right, "is_lt");
	case GT: return Builder->CreateICmpUGT(left, right, "is_gt");

	default:
		return log_error_value("invalid binary operator");
	}
}

std::pair<BasicBlock *, BasicBlock *> CodeGenerator::generate_if_cond(Value *condition) {
	Function *func = Builder->GetInsertBlock()->getParent();
	BasicBlock *then_bb = BasicBlock::Create(*TheContext, "then", func);
	BasicBlock *else_bb = BasicBlock::Create(*TheContext, "else");
	BasicBlock *merge_bb = BasicBlock::Create(*TheContext, "ifcont"); // will stick on end later

	Builder->CreateCondBr(condition, then_bb, else_bb);
	Builder->SetInsertPoint(then_bb);

	return {else_bb, merge_bb};
}

BasicBlock *CodeGenerator::start_if_else(BasicBlock *else_bb, BasicBlock *merge) {
	Builder->CreateBr(merge);
	Function *func = Builder->GetInsertBlock()->getParent();
	BasicBlock *then_bb = Builder->GetInsertBlock(); // this is important! we need to get again as stuff may change during codegen
	func->getBasicBlockList().push_back(else_bb);
	Builder->SetInsertPoint(else_bb);
	return then_bb;
}

PHINode *CodeGenerator::generate_if_merge(BasicBlock *merge, BasicBlock *then_bb, Value *then_v, Value *else_v) {
	Builder->CreateBr(merge);
	BasicBlock *else_bb = Builder->GetInsertBlock();

	Function *func = Builder->GetInsertBlock()->getParent();
	func->getBasicBlockList().push_back(merge);
	Builder->SetInsertPoint(merge);
	PHINode *pn = Builder->CreatePHI(then_v->getType(), 2, "end_if_phi");
	pn->addIncoming(then_v, then_bb);
	pn->addIncoming(else_v, else_bb);
	return pn;
}

Value *CodeGenerator::generate_func_call(FunctionCallee callee_f, Value *arg) {
	std::vector<Value *> args{};
	if (arg)
		args.push_back(arg);
	if (callee_f.getFunctionType()->getReturnType()->isFunctionTy()) {
		FunctionType *ft = callee_f.getFunctionType();
		ft = types.build_morphism(PointerType::get(*TheContext, 0), ft->getParamType(0));
		callee_f = FunctionCallee(ft, callee_f.getCallee());
	}
	return Builder->CreateCall(callee_f, args, callee_f.getCallee()->getName() + ".res");
}

std::pair<Function *, std::vector<Value *>> CodeGenerator::generate_func_head(std::string &&name, std::vector<std::string> params, FunctionType *ft) {
	if (ft->getReturnType()->isFunctionTy())
		ft = types.build_morphism(PointerType::get(*TheContext, 0), ft->getParamType(0));
	Function *func = Function::Create(ft, Function::ExternalLinkage, name, TheModule.get());

	BasicBlock *bb = BasicBlock::Create(*TheContext, "entry", func);
	Builder->SetInsertPoint(bb);
	std::vector<Value *> args{};

	if (params.size() == 1) {
		func->getArg(0)->setName(params[0]);
		args.push_back(func->getArg(0));
	} else if (params.size() > 1) {
		// destructure param
		Value *arg_ptr = generate_reference(func->getArg(0));
		Type *param_type = ft->getParamType(0);
		assert(param_type->isStructTy());
		Value *ptr, *arg;
		for (unsigned i = 0; i < params.size(); i++) {
			ptr = Builder->CreateStructGEP(param_type, arg_ptr, i, params[i] + ".address");
			arg = Builder->CreateLoad(param_type->getContainedType(i), ptr, params[i]);
			args.push_back(arg);
		}
	}

	return {func, args};
}

void CodeGenerator::generate_func_body(Function *func, Value *body) {
	Builder->CreateRet(body);
	verifyFunction(*func);
}

Value *CodeGenerator::generate_reference(Value *val) {
	// allocate space, do store
	BasicBlock &entry = Builder->GetInsertBlock()->getParent()->getEntryBlock();
	IRBuilder<> tmp(&entry, entry.begin());
	AllocaInst *bucket = tmp.CreateAlloca(val->getType(), nullptr, val->getName() + "_ref");
	Builder->CreateStore(val, bucket);
	return bucket;
}

Value *CodeGenerator::generate_dereference(Value *val, Type *val_points_to) {
	Value *val_as_int = Builder->CreateBitOrPointerCast(val, Type::getInt64Ty(*TheContext), val->getName() + "_as_int");
	// mask last bit and dereference
	Value *ptr = Builder->CreateAnd(val_as_int, ConstantInt::get(*TheContext, APInt(64, 0xffff'ffff'ffff'fffe)), val->getName() + "_as_int_masked");
	ptr = Builder->CreateBitOrPointerCast(ptr, val->getType(), val->getName() + "_masked");
	Value *deref = Builder->CreateLoad(val_points_to, ptr, val->getName() + "_deref");
	// if last bit 1, heap free
	Value *last_bit = Builder->CreateAnd(val_as_int, ConstantInt::get(*TheContext, APInt(64, 1)), val->getName() + "_last_bit");
	Function *func = Builder->GetInsertBlock()->getParent();
	BasicBlock *heap_free_bb = BasicBlock::Create(*TheContext, "heapfree", func);
	BasicBlock *merge_bb = BasicBlock::Create(*TheContext, "heapcont");
	Builder->CreateCondBr(last_bit, heap_free_bb, merge_bb);
	
	Builder->SetInsertPoint(heap_free_bb);
	Type *void_ptr_type = Type::getInt8PtrTy(*TheContext);
	ptr = Builder->CreatePointerCast(ptr, void_ptr_type, val->getName() + "_as_void_ptr");
	Builder->CreateCall(runtime_funcs["heap_free"], {ptr});

	Builder->CreateBr(merge_bb);
	func->getBasicBlockList().push_back(merge_bb);
	Builder->SetInsertPoint(merge_bb);
	return deref;
}

Value *CodeGenerator::generate_heap_copy(Value *val) {
	Type *ptr_type = PointerType::getUnqual(val->getType());
	Type *size_type = Type::getInt64Ty(*TheContext);
	Value *ptr = Builder->CreateCall(
		runtime_funcs["heap_allocate"],
		{ConstantInt::get(size_type,
	 					 TheModule->getDataLayout().getTypeAllocSize(val->getType()))},
		"heap_addr.as_void_ptr"
	);
	ptr = Builder->CreatePointerCast(ptr, ptr_type, "heap_addr");
	Builder->CreateStore(val, ptr);
	ptr = Builder->CreateBitOrPointerCast(ptr, Type::getInt64Ty(*TheContext), "heap_addr.as_int");
	ptr = Builder->CreateOr(ptr, ConstantInt::get(*TheContext, APInt(64, 1)), "heap_addr.as_int.masked");
	ptr = Builder->CreateBitOrPointerCast(ptr, ptr_type, val->getName() + ".ref");
	return ptr;
}

Function *CodeGenerator::generate_func_declaration(std::string &&name, FunctionType *ft) {
	return Function::Create(ft, Function::ExternalLinkage, name, TheModule.get());
}

Value *CodeGenerator::generate_struct(std::vector<Value *> components, StructType *type) {
	BasicBlock &entry = Builder->GetInsertBlock()->getParent()->getEntryBlock();
	IRBuilder<> tmp(&entry, entry.begin());
	AllocaInst *struct_ptr = tmp.CreateAlloca(type, nullptr, "structure_ptr");
	Value *ptr;
	for (unsigned i = 0; i < components.size(); i++) {
		ptr = Builder->CreateStructGEP(type, struct_ptr, i, "component");
		Builder->CreateStore(components[i], ptr);
	}
	Value *structure = Builder->CreateLoad(type, struct_ptr, "structure");
	return structure;
}

Value *CodeGenerator::generate_cast(Value *from, Type *to_type) {
	if (to_type->isIntegerTy())
		return Builder->CreateIntCast(from, to_type, true, from->getName() + ".cast");
	else if (to_type->isFloatTy())
		return Builder->CreateFPCast(from, to_type, from->getName() + ".cast");
	else if (to_type->isPointerTy())
		return Builder->CreatePointerCast(from, to_type, from->getName() + ".cast");
	else
		return Builder->CreateBitCast(from, to_type, from->getName() + ".cast");
}

std::tuple<Function *, map<string, Value *>, BasicBlock *, Value *>
CodeGenerator::generate_lambda_head(vector<string> params, vector<string> captures, vector<Type *> capture_types, FunctionType *ft) {
	if (ft->getReturnType()->isFunctionTy())
		ft = types.build_morphism(PointerType::get(*TheContext, 0), ft->getParamType(0));
	BasicBlock *save_point = Builder->GetInsertBlock();
	Function *parent = Builder->GetInsertBlock()->getParent();
	// modify ft to include captured context argument
	Type *context_type, *context_param_type;
	if (!capture_types.empty())
	{
		capture_types.push_back(Type::getInt8PtrTy(*TheContext));
		context_type = types.build_product(capture_types, parent->getName().str() + ".lambda.context");
		context_param_type = types.build_ref(context_type);
	}
	else
	{
		context_param_type = Type::getInt8PtrTy(*TheContext);
	}

	if (ft->getNumParams() == 1)
		ft = FunctionType::get(ft->getReturnType(), {context_param_type, ft->getParamType(0)}, false);
	else
		ft = FunctionType::get(ft->getReturnType(), {context_param_type}, false);

	Function *lambda = Function::Create(ft, Function::PrivateLinkage, parent->getName() + ".lambda", TheModule.get());
	lambda->addParamAttr(0, Attribute::get(*TheContext, Attribute::AttrKind::Nest));

	BasicBlock *bb = BasicBlock::Create(*TheContext, "entry", lambda);
	Builder->SetInsertPoint(bb);

	Value *arg_ptr, *param, *trampoline_ptr;
	Type *param_type;
	map<string, Value *> named_values{};

	// destructure context arg
	lambda->getArg(0)->setName("context");
	if (captures.size() > 0) {
		arg_ptr = lambda->getArg(0); // no need to generate reference since it is already a reference to the struct
		unsigned i;
		for (i = 0; i < captures.size(); i++) {
			param = Builder->CreateStructGEP(context_type, arg_ptr, i, captures[i] + ".address");
			param = Builder->CreateLoad(context_type->getContainedType(i), param, captures[i]);
			named_values[captures[i]] = param;
		}
		trampoline_ptr = Builder->CreateStructGEP(context_type, arg_ptr, i, "trampoline_ptr.address");
		trampoline_ptr = Builder->CreateLoad(context_type->getContainedType(i), trampoline_ptr, "trampoline_ptr");
		arg_ptr = Builder->CreatePointerCast(arg_ptr, Type::getInt8PtrTy(*TheContext), "context.void_ptr");
		Builder->CreateCall(runtime_funcs["heap_free"], {arg_ptr});
	} else {
		trampoline_ptr = lambda->getArg(0);
	}

	// destructure given arg
	if (params.size() == 1) {
		lambda->getArg(1)->setName(params[0]);
		named_values[params[0]] = lambda->getArg(1);
	} else if (params.size() > 1) {
		arg_ptr = generate_reference(lambda->getArg(1));
		param_type = ft->getParamType(1);
		assert(param_type->isStructTy());
		for (unsigned i = 0; i < params.size(); i++) {
			param = Builder->CreateStructGEP(param_type, arg_ptr, i, params[i] + ".address");
			param = Builder->CreateLoad(param_type->getContainedType(i), param, params[i]);
			named_values[params[i]] = param;
		}
	}

	return {lambda, named_values, save_point, trampoline_ptr};
}

Value *CodeGenerator::generate_lambda_body(Function *func, FunctionType *ft, Value *body, Value *trampoline_ptr, BasicBlock *save_point, vector<Value *> captures, vector<Type *> capture_types) {
	// finish lambda body
	Type *void_ptr_type = Type::getInt8PtrTy(*TheContext);
	Builder->CreateCall(runtime_funcs["trampoline_free"], {trampoline_ptr});
	Builder->CreateRet(body);

	verifyFunction(*func);

	Builder->SetInsertPoint(save_point);
	// create trampoline
	trampoline_ptr = Builder->CreateCall(runtime_funcs["trampoline_allocate"], {}, "tramp_ptr");

	// build context struct
	Value *context;
	if (captures.empty()) {
		context = trampoline_ptr;
	} else {
		captures.push_back(trampoline_ptr);
		capture_types.push_back(void_ptr_type);
		StructType *capture_type = StructType::getTypeByName(*TheContext, func->getName().str() + ".context");
		context = Builder->CreateCall(
			runtime_funcs["heap_allocate"],
			{ConstantInt::get(Type::getInt64Ty(*TheContext),
							  TheModule->getDataLayout().getTypeAllocSize(capture_type))},
			"context_ptr"
		);
		Value *cast_context = Builder->CreatePointerCast(context, types.build_ref(capture_type), "context_ptr");
		Value *ptr;
		for (unsigned i = 0; i < captures.size(); i++) {
			ptr = Builder->CreateStructGEP(capture_type, cast_context, i, "context" + captures[i]->getName());
			Builder->CreateStore(captures[i], ptr);
		}
	}
	
	context = Builder->CreateBitCast(context, void_ptr_type, "context_ptr");
	Value *lambda_ptr = Builder->CreateBitCast(func, void_ptr_type, "func_ptr");
	Builder->CreateCall(runtime_funcs["llvm.init.trampoline"], {trampoline_ptr, lambda_ptr, context});
	lambda_ptr = Builder->CreateCall(runtime_funcs["llvm.adjust.trampoline"], {trampoline_ptr}, "lambda_ptr");
	Value *lambda = Builder->CreateBitCast(lambda_ptr, PointerType::get(*TheContext, 0), "lambda");
	Builder->CreateCall(runtime_funcs["make_tramp_executable"], {lambda_ptr});

	return lambda;
}

void CodeGenerator::print_current_module() {
	MPM.run(*TheModule, MAM);
	TheModule->print(errs(), nullptr);
}

void CodeGenerator::jit_current_module() {
	ExitOnErr(TheJIT->addModule(
		ThreadSafeModule(std::move(TheModule), std::move(TheContext))));
	InitializeModuleAndPassManager();
}

void CodeGenerator::jit_call(std::string name) {
	auto rt = TheJIT->getMainJITDylib().createResourceTracker();
	MPM.run(*TheModule, MAM);
	auto tsm = ThreadSafeModule(std::move(TheModule), std::move(TheContext));
	ExitOnErr(TheJIT->addModule(std::move(tsm), rt));
	InitializeModuleAndPassManager();

	auto symbol = ExitOnErr(TheJIT->lookup(name));
	void (*FP)() = (void (*)())(intptr_t)symbol.getAddress();
	FP();

	ExitOnErr(rt->remove());
}

void CodeGenerator::dump_obj_file(std::string &&filename) {
	auto TargetTriple = LLVMGetDefaultTargetTriple();
	TheModule->setTargetTriple(TargetTriple);

	InitializeAllTargetInfos();
	InitializeAllTargets();
	InitializeAllTargetMCs();
	InitializeAllAsmParsers();
	InitializeAllAsmPrinters();

	std::string Error;
	auto Target = TargetRegistry::lookupTarget(TargetTriple, Error);

	// Print an error and exit if we couldn't find the requested target.
	// This generally occurs if we've forgotten to initialise the
	// TargetRegistry or we have a bogus target triple.
	if (!Target) {
		errs() << Error;
		return;
	}

	auto CPU = "generic";
	auto Features = "";

	TargetOptions opt;
	auto RM = Optional<Reloc::Model>();
	auto TargetMachine = Target->createTargetMachine(TargetTriple, CPU, Features, opt, RM);

	TheModule->setDataLayout(TargetMachine->createDataLayout());

	MPM.run(*TheModule, MAM);

	std::error_code EC;
	raw_fd_ostream dest(filename, EC, sys::fs::OF_None);

	if (EC) {
		errs() << "Could not open file: " << EC.message();
		return;
	}

	legacy::PassManager pass;
	auto FileType = CGFT_ObjectFile;

	if (TargetMachine->addPassesToEmitFile(pass, dest, nullptr, FileType)) {
		errs() << "TargetMachine can't emit a file of this type";
		return;
	}

	pass.run(*TheModule);
	dest.flush();
}
