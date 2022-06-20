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
#include "llvm/IR/PassManager.h"
#include "llvm/Analysis/LoopAnalysisManager.h"
#include "llvm/Analysis/CGSCCPassManager.h"
#include "llvm/Passes/PassBuilder.h"
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
#include <tuple>

using namespace llvm;
using namespace orc;

using std::map;
using std::string;
using std::vector;

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

	// Create the analysis managers.
	LoopAnalysisManager LAM;
	FunctionAnalysisManager FAM;
	CGSCCAnalysisManager CGAM;
	ModuleAnalysisManager MAM;

	PassBuilder PB;
	ModulePassManager MPM;

	std::unique_ptr<IRBuilder<>> Builder;
	std::unique_ptr<KaleidoscopeJIT> TheJIT;  // TODO: enable AOT

	void InitializeModuleAndPassManager();
	FunctionCallee getFunction(string &&name, FunctionType *ft);
	map<string, FunctionCallee> runtime_funcs {};

public:
	CodeGenerator(/* args */);
	~CodeGenerator() = default;

	TypeHandler types;

	Value *generate_number(double val);
	Value *generate_float(double val);
	Value *generate_int(unsigned bitwidth, long val, bool issigned);
	Value *generate_var(string &&name);
	Value *generate_binary(Value *left, Value *right, BinaryOp op);
	Value *generate_fbinary(Value *left, Value *right, BinaryOp op);
	Value *generate_func_call(FunctionCallee func, Value *arg);

	Value *generate_reference(Value *val);
	Value *generate_dereference(Value *val, Type *val_points_to);
	Value *generate_heap_copy(Value *val);
	Value *generate_struct(vector<Value *> components, StructType *type);
	Value *generate_cast(Value *from, Type *to_type);

	std::pair<BasicBlock*, BasicBlock*> generate_if_cond(Value *condition); // returns else block, merge block
	BasicBlock *start_if_else(BasicBlock *else_bb, BasicBlock *merge); // returns updated then block
	PHINode *generate_if_merge(BasicBlock *merge, BasicBlock *then_bb, Value *then_v, Value *else_v);

	std::pair<Function *, vector<Value *>> generate_func_head(string &&name, vector<string> params, FunctionType *ft);
	void generate_func_body(Function *func, Value *body);
	std::tuple<Function *, map<string, Value *>, BasicBlock *, Value *> generate_lambda_head(vector<string> params, vector<string> captures, vector<Type *> capture_types, FunctionType *ft);
	Value *generate_lambda_body(Function *func, FunctionType *ft, Value *body, Value *trampoline_ptr, BasicBlock *save_point, vector<Value *> captures, vector<Type *> capture_types);

	Function *generate_func_declaration(string &&name, FunctionType *ft);

	void print_current_module();
	void jit_current_module();  // TODO: return new module & context
	void jit_call(string name);  // TODO: add support for args?
	void dump_obj_file(string &&filename);
};
