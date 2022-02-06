#include "../semantics/typehandler.h"

#include "KaleidoscopeJIT.h"
#include "llvm/ADT/APFloat.h"
#include "llvm/ADT/STLExtras.h"
#include "llvm/IR/BasicBlock.h"
#include "llvm/IR/Constants.h"
#include "llvm/IR/DerivedTypes.h"
#include "llvm/IR/Function.h"
#include "llvm/IR/IRBuilder.h"
#include "llvm/IR/LLVMContext.h"
#include "llvm/IR/LegacyPassManager.h"
#include "llvm/IR/Module.h"
#include "llvm/IR/Type.h"
#include "llvm/IR/Verifier.h"
#include "llvm/Support/TargetSelect.h"
#include "llvm/Target/TargetMachine.h"
#include "llvm/Transforms/InstCombine/InstCombine.h"
#include "llvm/Transforms/Scalar.h"
#include "llvm/Transforms/Scalar/GVN.h"
#include "llvm/Transforms/Scalar/TailRecursionElimination.h"
#include <algorithm>
#include <cassert>
#include <cctype>
#include <cstdint>
#include <cstdio>
#include <cstdlib>
#include <map>
#include <memory>
#include <string>
#include <vector>
#include <exception>

using namespace llvm;
using namespace orc;

using std::map;
using std::string;

enum BinaryOp { // TODO: add %, &, |, &&, ||, ==, !=
	ADD = '+',
	SUB = '-',
	MUL = '*',
	DIV = '/',
	LT = '<',
	GT = '>'
};

class CodeGenerator {
private:
	std::unique_ptr<LLVMContext> TheContext;
	std::unique_ptr<Module> TheModule;

	std::unique_ptr<legacy::FunctionPassManager> TheFPM;
	std::unique_ptr<IRBuilder<>> Builder;
	map<string, Value *> NamedValues;  // TODO: move to Prolog?
	std::unique_ptr<KaleidoscopeJIT> TheJIT;  // TODO: enable AOT

	void InitializeModuleAndPassManager();
	Function *getFunction(std::string &name);

public:
	CodeGenerator(/* args */);
	~CodeGenerator() = default;

	TypeHandler types;

	Value *generate_number(double val);
	Value *generate_float(double val);
	Value *generate_int(unsigned bitwidth, long val, bool issigned);
	Value *generate_var(std::string name);
	// TODO: add generate_fbinary
	Value *generate_binary(Value *left, Value *right, BinaryOp op);
	Value *generate_fbinary(Value *left, Value *right, BinaryOp op);
	Value *generate_func_call(std::string callee, std::vector<Value *> args);

	std::pair<BasicBlock*, BasicBlock*> generate_if_cond(Value *condition); // returns else block, merge block
	BasicBlock *start_if_else(BasicBlock *else_bb, BasicBlock *merge); // returns updated then block
	PHINode *generate_if_merge(BasicBlock *merge, BasicBlock *then_bb, Value *then_v, Value *else_v);

	Function *generate_func_head(std::string name, std::vector<std::string> params);
	Function *generate_func_head(std::string name, std::vector<std::string> params, FunctionType *ft);
	void generate_func_body(Function *func, Value *body);

	Function *generate_func_declaration(std::string name, FunctionType *ft);

	void jit_current_module();  // TODO: return new module & context
	void jit_call(std::string name);  // TODO: add support for args?
};
