#include "codegen.h"
#include <SWI-Prolog.h>
#include <iostream>
#include <memory>
#include <cstring>
#include <unordered_map>

// TODO: replace cerr stuff with PL_raise_exception(term_t exception)
// TODO: replace raw pointer terms with compound terms, ex: 0xdeadbeef -> llvm_value(0xdeadbeef)
// TODO: convert to SWI-Prolog C++ interface

template <typename T>
T *term_to_pointer(term_t term) {
	void *ptr;
	PL_get_pointer_ex(term, &ptr);
	return static_cast<T *>(ptr);
}

template <typename T> // T must be a LLVM namespace type with a print method
static foreign_t print(term_t term) {
	term_to_pointer<T>(term)->print(errs());
	PL_succeed;
}

static std::unique_ptr<CodeGenerator> generator{};

static foreign_t generate_number(term_t val, term_t node) {
	double fval;
	if (!PL_get_float(val, &fval))
		PL_fail;

	Value *n_val = generator->generate_number(fval);
	if (!n_val)
		PL_fail;

	auto val_ptr = static_cast<void *>(n_val);
	return PL_unify_pointer(node, val_ptr);
}

static foreign_t generate_float(term_t val, term_t node) {
	double fval;
	if (!PL_get_float(val, &fval))
		PL_fail;

	Value *n_val = generator->generate_float(fval);
	if (!n_val)
		PL_fail;

	auto val_ptr = static_cast<void *>(n_val);
	return PL_unify_pointer(node, val_ptr);
}

static foreign_t generate_int(term_t width, term_t val, term_t issigned, term_t node) {
	long lval;
	long lwidth;
	int lsigned;
	if (!PL_get_long(val, &lval) || !PL_get_long(width, &lwidth) || !PL_get_bool(issigned, &lsigned))
		PL_fail;

	Value *n_val = generator->generate_int(lwidth, lval, static_cast<bool>(lsigned));
	if (!n_val)
		PL_fail;

	return PL_unify_pointer(node, static_cast<void *>(n_val));
}

static foreign_t generate_binary(term_t left, term_t right, term_t op, term_t node) {
	void *n_left_ptr, *n_right_ptr;
	char *n_op_ptr;

	if (!PL_get_pointer_ex(left, &n_left_ptr) ||
		!PL_get_pointer_ex(right, &n_right_ptr) ||
		!PL_get_atom_chars(op, &n_op_ptr))
		PL_fail;

	auto n_left = static_cast<Value *>(n_left_ptr);
	auto n_right = static_cast<Value *>(n_right_ptr);
	auto n_op = static_cast<BinaryOp>(*n_op_ptr);

	Value *n_val = generator->generate_binary(n_left, n_right, n_op);
	if (!n_val)
		PL_fail;

	auto val_ptr = static_cast<void *>(n_val);
	return PL_unify_pointer(node, val_ptr);
}

static foreign_t generate_fbinary(term_t left, term_t right, term_t op, term_t node) {
	void *n_left_ptr, *n_right_ptr;
	char *n_op_ptr;

	if (!PL_get_pointer_ex(left, &n_left_ptr) ||
		!PL_get_pointer_ex(right, &n_right_ptr) ||
		!PL_get_atom_chars(op, &n_op_ptr))
		PL_fail;

	auto n_left = static_cast<Value *>(n_left_ptr);
	auto n_right = static_cast<Value *>(n_right_ptr);
	auto n_op = static_cast<BinaryOp>(*n_op_ptr);

	Value *n_val = generator->generate_fbinary(n_left, n_right, n_op);
	if (!n_val)
		PL_fail;

	auto val_ptr = static_cast<void *>(n_val);
	return PL_unify_pointer(node, val_ptr);
}

static foreign_t generate_func_call(term_t callee, term_t arg, term_t node) {
	Value *n_val = generator->generate_func_call(term_to_pointer<Function>(callee), term_to_pointer<Value>(arg));
	if (!n_val)
		PL_fail;

	return PL_unify_pointer(node, static_cast<void *>(n_val));
}

static foreign_t generate_func_head(term_t name, term_t param_list, term_t type,
										  term_t node, term_t arg_values) {
	term_t head = PL_new_term_ref();
	term_t list = PL_copy_term_ref(param_list);
	char *func_name;
	size_t len;
	if (!PL_get_string(name, &func_name, &len))
		PL_fail;

	std::vector<std::string> params;
	char *param_name;
	while (PL_get_list_ex(list, head, list)) {
		if (!PL_get_string(head, &param_name, &len))
			PL_fail;
		params.push_back(std::string(param_name));
	}

	FunctionType *ft = term_to_pointer<FunctionType>(type);

	auto func_args = generator->generate_func_head(std::string(func_name), params, ft);
	Function *func{func_args.first};
	std::vector<Value *> args{func_args.second};
	if (!func)
		PL_fail;

	// build pair list of name, function arg value
	functor_t pair;
	string arg_name;
	head = PL_new_term_ref();
	list = PL_copy_term_ref(arg_values);
	for (unsigned i = 0; i < args.size(); i++)
	{
		
		// put name-arg onto list
		pair = PL_new_functor(PL_new_atom("-"), 2);
		if (!PL_unify_list(list, head, list) ||
			!PL_unify_term(head,
						   PL_FUNCTOR, pair,
						   PL_STRING, params[i].c_str(),
						   PL_POINTER, static_cast<void *>(args[i])))
			PL_fail;
	}
	PL_unify_nil_ex(list);

	return PL_unify_pointer(node, static_cast<void *>(func));
}

static foreign_t generate_func_body(term_t head, term_t body) {
	void *head_ptr, *body_ptr;
	if (!PL_get_pointer_ex(head, &head_ptr) ||
		!PL_get_pointer_ex(body, &body_ptr))
		PL_fail;

	auto func_head = static_cast<Function *>(head_ptr);
	auto func_body = static_cast<Value *>(body_ptr);
	
	if (!func_head || !func_body)
		PL_fail;

	generator->generate_func_body(func_head, func_body);

	PL_succeed;
}

static foreign_t generate_declaration(term_t name, term_t type, term_t node) {
	char *name_ptr;
	size_t len;
	if (!PL_get_string(name, &name_ptr, &len))
		PL_fail;
	FunctionType *ft = term_to_pointer<FunctionType>(type);
	Function *func = generator->generate_func_declaration(std::string(name_ptr), ft);
	return PL_unify_pointer(node, static_cast<void *>(func));
}

static foreign_t jit_current_module() {
	generator->jit_current_module();
	PL_succeed;
}

static foreign_t jit_call(term_t name) {
	char *name_ptr;
	size_t len;
	if (!PL_get_string(name, &name_ptr, &len)) {
		std::cerr << "couldn't find function by name " << name_ptr << '\n';
		PL_fail;
	}

	generator->jit_call(std::string(name_ptr));
	PL_succeed;
}

static foreign_t reset_generator() {
	generator.reset(new CodeGenerator());
	PL_succeed;
}

static foreign_t generate_if_cond(term_t condition, term_t else_block, term_t merge_block) {
	void *cond_ptr;
	if (!PL_get_pointer_ex(condition, &cond_ptr)) {
		std::cerr << "couldn't get pointer from term\n";
		PL_fail;
	}
	if (!cond_ptr) {
		std::cerr << "invalid condition pointer\n";
		PL_fail;
	}

	std::pair<BasicBlock*, BasicBlock*> else_merge = generator->generate_if_cond(static_cast<Value *>(cond_ptr));
	auto else_ptr = static_cast<void *>(else_merge.first);
	auto merge_ptr = static_cast<void *>(else_merge.second);

	return PL_unify_pointer(merge_block, merge_ptr) && PL_unify_pointer(else_block, else_ptr);
}

static foreign_t start_if_else(term_t else_bb, term_t merge, term_t then) {
	void *else_ptr, *merge_ptr;
	if (!PL_get_pointer_ex(else_bb, &else_ptr) || !PL_get_pointer_ex(merge, &merge_ptr)) {
		std::cerr << "couldn't get pointer from term\n";
		PL_fail;
	}
	if (!else_ptr || !merge_ptr) {
		std::cerr << "invalid pointer\n";
		PL_fail;
	}

	BasicBlock *then_block = generator->start_if_else(static_cast<BasicBlock *>(else_ptr), static_cast<BasicBlock *>(merge_ptr));
	
	if (!then_block) {
		std::cerr << "bad then block pointer\n";
		PL_fail;
	}

	auto then_ptr = static_cast<void *>(then_block);

	return PL_unify_pointer(then, then_ptr);
}

static foreign_t generate_if_merge(term_t merge, term_t then_bb, term_t then_v, term_t else_v, term_t phi_node) {
	void *merge_ptr, *then_bb_ptr, *then_v_ptr, *else_v_ptr;
	if (!PL_get_pointer_ex(merge, &merge_ptr) || !PL_get_pointer_ex(then_bb, &then_bb_ptr)
		|| !PL_get_pointer_ex(then_v, &then_v_ptr) || !PL_get_pointer_ex(else_v, &else_v_ptr)) {
		std::cerr << "can't get pointer from term\n";
		PL_fail;
	}
	if (!merge_ptr || !then_bb_ptr || !then_v_ptr || !else_v_ptr) {
		std::cerr << "invalid pointer\n";
		PL_fail;
	}

	PHINode *pn = generator->generate_if_merge(static_cast<BasicBlock *>(merge_ptr), static_cast<BasicBlock *>(then_bb_ptr),
		static_cast<Value *>(then_v_ptr), static_cast<Value *>(else_v_ptr));

	auto phi_node_ptr = static_cast<void *>(pn);
	return PL_unify_pointer(phi_node, phi_node_ptr);
}

static foreign_t build_type_product(term_t types, term_t node) {
	term_t head = PL_new_term_ref();
	term_t list = PL_copy_term_ref(types);

	vector<Type *> members;
	while (PL_get_list(list, head, list))
	{
		// process head as list element
		Type *member = term_to_pointer<Type>(head);
		members.push_back(member);
	}
	if (!PL_get_nil(list))
		PL_fail;
	Type *type_ptr = generator->types.build_product(members);

	return PL_unify_pointer(node, static_cast<void *>(type_ptr));
}

static foreign_t build_morphism(term_t source, term_t target, term_t morphism) {
	Type *type_ptr = generator->types.build_morphism(term_to_pointer<Type>(target), term_to_pointer<Type>(source));
	return PL_unify_pointer(morphism, static_cast<void *>(type_ptr));
}

static foreign_t build_reference(term_t type, term_t reference) {
	Type *type_ptr = generator->types.build_ref(term_to_pointer<Type>(type));
	return PL_unify_pointer(reference, static_cast<void *>(type_ptr));
}

static foreign_t get_type(term_t type_name, term_t node) {
	char *name_ptr;
	size_t len;
	if (!PL_get_string(type_name, &name_ptr, &len)) {
		PL_fail;
	}

	Type *type = generator->types.get_type(std::string(name_ptr));

	if (type) {
		return PL_unify_pointer(node, static_cast<void *>(type));
	} else {
		PL_fail;
	}
}

static foreign_t register_type(term_t name, term_t type) {
	char *name_ptr;
	size_t len;
	if (!PL_get_string(name, &name_ptr, &len)) {
		PL_fail;
	}
	generator->types.register_type(std::string(name_ptr), term_to_pointer<Type>(type));
	PL_succeed;
}

static foreign_t assign_type(term_t name, term_t type) {
	char *name_ptr;
	size_t len;
	if (!PL_get_string(name, &name_ptr, &len)) {
		PL_fail;
	}
	generator->types.assign_type(std::string(name_ptr), term_to_pointer<Type>(type));
	PL_succeed;
}

// either +val -reference or -val +reference
static foreign_t handle_reference(term_t val, term_t type, term_t reference) {
	if (!PL_is_variable(val) && PL_is_variable(reference))
	{ // +val -reference
		Value *node = generator->generate_reference(term_to_pointer<Value>(val));
		return PL_unify_pointer(reference, static_cast<void *>(node));
	}
	else if (PL_is_variable(val) && !PL_is_variable(reference))
	{ // -val +reference
		Value *node = generator->generate_dereference(term_to_pointer<Value>(reference), term_to_pointer<Type>(type));
		return PL_unify_pointer(val, static_cast<void *>(node));
	}
	else
	{
		PL_fail;
	}
}

static foreign_t generate_heap_copy(term_t val, term_t reference) {
	Value *node = generator->generate_heap_copy(term_to_pointer<Value>(val));
	return PL_unify_pointer(reference, static_cast<void *>(node));
}

static foreign_t generate_struct(term_t components, term_t type, term_t node) {
	term_t head = PL_new_term_ref();
	term_t list = PL_copy_term_ref(components);

	vector<Value *> members;
	while (PL_get_list(list, head, list))
	{
		// process head as list element
		Value *member = term_to_pointer<Value>(head);
		members.push_back(member);
	}
	if (!PL_get_nil(list))
		PL_fail;

	Value *structure = generator->generate_struct(members, term_to_pointer<StructType>(type));
	return PL_unify_pointer(node, static_cast<void *>(structure));
}

extern "C" install_t install() {
	PL_register_foreign("codegen_number", 2, reinterpret_cast<pl_function_t>(generate_number), 0);
	PL_register_foreign("codegen_float", 2, reinterpret_cast<pl_function_t>(generate_float), 0);
	PL_register_foreign("codegen_int", 4, reinterpret_cast<pl_function_t>(generate_int), 0);
	PL_register_foreign("codegen_binary", 4, reinterpret_cast<pl_function_t>(generate_binary), 0);
	PL_register_foreign("codegen_fbinary", 4, reinterpret_cast<pl_function_t>(generate_fbinary), 0);
	PL_register_foreign("codegen_func_call", 3, reinterpret_cast<pl_function_t>(generate_func_call), 0);
	PL_register_foreign("codegen_struct", 3, reinterpret_cast<pl_function_t>(generate_struct), 0);

	PL_register_foreign("codegen_func_head", 5, reinterpret_cast<pl_function_t>(generate_func_head), 0);
	PL_register_foreign("codegen_func_body", 2, reinterpret_cast<pl_function_t>(generate_func_body), 0);

	PL_register_foreign("codegen_declaration", 3, reinterpret_cast<pl_function_t>(generate_declaration), 0);

	PL_register_foreign("codegen_if_cond", 3, reinterpret_cast<pl_function_t>(generate_if_cond), 0);
	PL_register_foreign("codegen_start_if_else", 3, reinterpret_cast<pl_function_t>(start_if_else), 0);
	PL_register_foreign("codegen_if_merge", 5, reinterpret_cast<pl_function_t>(generate_if_merge), 0);

	PL_register_foreign("codegen_reference", 3, reinterpret_cast<pl_function_t>(handle_reference), 0);
	PL_register_foreign("codegen_heap_copy", 2, reinterpret_cast<pl_function_t>(generate_heap_copy), 0);

	PL_register_foreign("build_type_product", 2, reinterpret_cast<pl_function_t>(build_type_product), 0);
	PL_register_foreign("build_morphism", 3, reinterpret_cast<pl_function_t>(build_morphism), 0);
	PL_register_foreign("build_reference", 2, reinterpret_cast<pl_function_t>(build_reference), 0);
	PL_register_foreign("get_llvm_type", 2, reinterpret_cast<pl_function_t>(get_type), 0);
	PL_register_foreign("set_llvm_type", 2, reinterpret_cast<pl_function_t>(register_type), 0);
	PL_register_foreign("assign_llvm_type", 2, reinterpret_cast<pl_function_t>(assign_type), 0);

	PL_register_foreign("print_expression", 1, reinterpret_cast<pl_function_t>(print<Value>), 0);
	PL_register_foreign("print_function", 1, reinterpret_cast<pl_function_t>(print<Function>), 0);
	PL_register_foreign("print_type", 1, reinterpret_cast<pl_function_t>(print<Type>), 0);

	PL_register_foreign("jit_current_module", 0, reinterpret_cast<pl_function_t>(jit_current_module), 0);
	PL_register_foreign("jit_call", 1, reinterpret_cast<pl_function_t>(jit_call), 0);

	PL_register_foreign("reset_generator", 0, reinterpret_cast<pl_function_t>(reset_generator), 0);
}
