#include "codegen.h"
#include <iostream>

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

	// Create a new pass manager attached to it.
	TheFPM = std::make_unique<legacy::FunctionPassManager>(TheModule.get());

	// Do simple "peephole" optimizations and bit-twiddling optzns.
	TheFPM->add(createInstructionCombiningPass());
	// Eliminate Common SubExpressions.
	TheFPM->add(createGVNPass());
	// Convert tail calls to loops
	TheFPM->add(createTailCallEliminationPass());
	// Simplify the control flow graph (deleting unreachable blocks, etc).
	TheFPM->add(createCFGSimplificationPass());
	// Reassociate expressions.
	TheFPM->add(createReassociatePass());

	TheFPM->doInitialization();
}

Function *CodeGenerator::getFunction(std::string &name) {
	if (auto *func = TheModule->getFunction(name))
		return func;

	// find some other way?

	return nullptr;
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

Value *CodeGenerator::generate_var(std::string name) {
	Value *v = NamedValues[name];
	if (!v)
		return log_error_value("Unknown variable name");
	return v;
}

Value *CodeGenerator::generate_fbinary(Value *left, Value *right, BinaryOp op) {
	if (!left || !right)
		return nullptr;

	switch (op) {
	case ADD: return Builder->CreateFAdd(left, right, "addtmp");
	case SUB: return Builder->CreateFSub(left, right, "subtmp");
	case MUL: return Builder->CreateFMul(left, right, "multmp");
	case DIV: return Builder->CreateFDiv(left, right, "divtmp");
	case LT: return Builder->CreateFCmpULT(left, right, "lttmp");
	case GT: return Builder->CreateFCmpUGT(left, right, "gttmp");

	default:
		return log_error_value("invalid binary operator");
	}
}

Value *CodeGenerator::generate_binary(Value *left, Value *right, BinaryOp op) {
	if (!left || !right)
		return nullptr;

	switch (op) {
	case ADD: return Builder->CreateAdd(left, right, "addtmp");
	case SUB: return Builder->CreateSub(left, right, "subtmp");
	case MUL: return Builder->CreateMul(left, right, "multmp");
	case DIV: return Builder->CreateSDiv(left, right, "divtmp");
	case LT: return Builder->CreateICmpULT(left, right, "lttmp");
	case GT: return Builder->CreateICmpUGT(left, right, "gttmp");

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
	PHINode *pn = Builder->CreatePHI(Type::getDoubleTy(*TheContext), 2, "iftmp");
	pn->addIncoming(then_v, then_bb);
	pn->addIncoming(else_v, else_bb);
	return pn;
}

Value *CodeGenerator::generate_func_call(std::string callee, std::vector<Value *> args) {
	Function *callee_f = getFunction(callee);

	if (callee_f->arg_size() != args.size())
		return log_error_value("Incorrect number of arguments passed");

	return Builder->CreateCall(callee_f, args, "calltmp");
}

Function *CodeGenerator::generate_func_head(std::string name, std::vector<std::string> params) {
	std::vector<Type *> doubles(params.size(), Type::getDoubleTy(*TheContext));
	FunctionType *ft = FunctionType::get(Type::getDoubleTy(*TheContext), doubles, false);
	Function *func = Function::Create(ft, Function::ExternalLinkage, name, TheModule.get());

	BasicBlock *bb = BasicBlock::Create(*TheContext, "entry", func);
	Builder->SetInsertPoint(bb);
	
	unsigned i = 0;
	std::string *param_name;
	NamedValues.clear();
	for (auto &param : func->args()) {
		param_name = &params[i++];
		param.setName(*param_name);
		NamedValues[*param_name] = &param;
	}

	return func;
}

Function *CodeGenerator::generate_func_head(std::string name, std::vector<std::string> params, FunctionType *ft) {
	Function *func = Function::Create(ft, Function::ExternalLinkage, name, TheModule.get());

	BasicBlock *bb = BasicBlock::Create(*TheContext, "entry", func);
	Builder->SetInsertPoint(bb);

	return func;
}

void CodeGenerator::generate_func_body(Function *func, Value *body) {
	if (body) {
		Builder->CreateRet(body);
		verifyFunction(*func);
		TheFPM->run(*func);
	} else {
		func->eraseFromParent();
	}
}

Function *CodeGenerator::generate_func_declaration(std::string name, FunctionType *ft) {
	return Function::Create(ft, Function::ExternalLinkage, name, TheModule.get());
}

void CodeGenerator::jit_current_module() {
	ExitOnErr(TheJIT->addModule(
		ThreadSafeModule(std::move(TheModule), std::move(TheContext))));
	InitializeModuleAndPassManager();
}

void CodeGenerator::jit_call(std::string name) {
	auto rt = TheJIT->getMainJITDylib().createResourceTracker();
	auto tsm = ThreadSafeModule(std::move(TheModule), std::move(TheContext));
	ExitOnErr(TheJIT->addModule(std::move(tsm), rt));
	InitializeModuleAndPassManager();

	auto symbol = ExitOnErr(TheJIT->lookup(name));
	void (*FP)() = (void (*)())(intptr_t)symbol.getAddress();
	FP();

	ExitOnErr(rt->remove());
}
