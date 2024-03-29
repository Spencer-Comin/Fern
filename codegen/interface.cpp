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
	return PL_get_pointer(term, &ptr) ? static_cast<T *>(ptr) : nullptr;
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

static foreign_t generate_func_call(term_t callee, term_t callee_type, term_t arg, term_t node) {
	if (PL_is_functor(callee, PL_new_functor(PL_new_atom("func_ptr"), 2))) {
		term_t temp_type = PL_new_term_ref(), temp_callee = PL_new_term_ref();
		if (!PL_get_arg(2, callee, temp_type) || !PL_get_arg(1, callee, temp_callee))
			PL_fail;
		callee_type = temp_type;
		callee = temp_callee;
	}

	FunctionType *ft = term_to_pointer<FunctionType>(callee_type);
	FunctionCallee fc{ft, term_to_pointer<Value>(callee)};
	Value *n_val = generator->generate_func_call(fc, term_to_pointer<Value>(arg));
	if (!n_val)
		PL_fail;

	// unify with func_ptr if the call returns a function
	if (ft->getReturnType()->isFunctionTy())
		return PL_unify_term(node,
							 PL_FUNCTOR_CHARS, "func_ptr", 2,
							 PL_POINTER, n_val,
							 PL_POINTER, ft->getReturnType());
	else
		return PL_unify_pointer(node, static_cast<void *>(n_val));
}

static foreign_t generate_func_head(term_t name, term_t param_list, term_t param_types, term_t type,
									term_t func, term_t node, term_t arg_values) {
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

	std::vector<Type *> types;
	head = PL_new_term_ref();
	list = PL_copy_term_ref(param_types);
	while (PL_get_list_ex(list, head, list)) {
		types.push_back(term_to_pointer<Type>(head));
	}

	FunctionType *ft = term_to_pointer<FunctionType>(type);

	auto func_args = generator->generate_func_head(std::string(func_name), params, ft);
	Function *func_ptr{func_args.first};
	std::vector<Value *> args{func_args.second};
	if (!func)
		PL_fail;

	// build list of name, function arg value
	head = PL_new_term_ref();
	list = PL_copy_term_ref(arg_values);
	for (unsigned i = 0; i < args.size(); i++)
	{
		// put name onto list
		if (!PL_unify_list(list, head, list) || !PL_unify_string_chars(head, params[i].c_str()))
			PL_fail;
		
		// put arg onto list
		if (!PL_unify_list(list, head, list))
			PL_fail;
		if (types[i]->isFunctionTy()) {
			if (!PL_unify_term(head,
							   PL_FUNCTOR_CHARS, "func_ptr", 2,
							   PL_POINTER, args[i],
							   PL_POINTER, types[i]))
				PL_fail;
		} else if (!PL_unify_pointer(head, args[i]))
			PL_fail;
	}
	PL_unify_nil_ex(list);

	return PL_unify_pointer(func, static_cast<void *>(func_ptr))
		&& PL_unify_term(node,
						 PL_FUNCTOR_CHARS, "func_ptr", 2,
						 PL_POINTER, func_ptr,
						 PL_POINTER, ft);
}

static foreign_t generate_func_body(term_t head, term_t body) {
	if (PL_is_functor(body, PL_new_functor(PL_new_atom("func_ptr"), 2))) {
		term_t temp_body = PL_new_term_ref();
		if (!PL_get_arg(1, body, temp_body))
			PL_fail;
		body = temp_body;
	}
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
	return PL_unify_term(node,
						 PL_FUNCTOR_CHARS, "func_ptr", 2,
						 PL_POINTER, func,
						 PL_POINTER, ft);
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
		if (PL_is_functor(val, PL_new_functor(PL_new_atom("func_ptr"), 2))) {
			term_t temp = PL_new_term_ref();
			if (!PL_get_arg(1, val, temp))
				PL_fail;
			val = temp;
		}
		Value *v = term_to_pointer<Value>(val);
		Value *node = generator->generate_reference(v);
		return PL_unify_pointer(reference, static_cast<void *>(node));
	}
	else if (PL_is_variable(val) && !PL_is_variable(reference))
	{ // -val +reference
		Type *t = term_to_pointer<Type>(type);
		Value *node = generator->generate_dereference(term_to_pointer<Value>(reference), t->isFunctionTy() ? PointerType::getUnqual(t) : t);
		if (t->isFunctionTy()) {
			return PL_unify_term(val,
								 PL_FUNCTOR, PL_new_functor(PL_new_atom("func_ptr"), 2),
								 PL_POINTER, node,
								 PL_POINTER, t);
		}
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
		if (PL_is_functor(head, PL_new_functor(PL_new_atom("func_ptr"), 2))) {
			term_t temp = PL_new_term_ref();
			if (!PL_get_arg(1, head, temp))
				PL_fail;
			head = temp;
		}
		members.push_back(term_to_pointer<Value>(head));
	}
	if (!PL_get_nil(list))
		PL_fail;

	Value *structure = generator->generate_struct(members, term_to_pointer<StructType>(type));
	return PL_unify_pointer(node, static_cast<void *>(structure));
}

static foreign_t generate_lambda_head(term_t params, term_t param_types, term_t captures, term_t capture_types, term_t func_type,
									  term_t lambda, term_t named_values, term_t save_point, term_t trampoline) {
	term_t head = PL_new_term_ref();
	term_t list = PL_copy_term_ref(params);

	std::vector<std::string> param_vec {};
	char *name;
	size_t len;
	while (PL_get_list_ex(list, head, list)) {
		if (!PL_get_string(head, &name, &len))
			PL_fail;
		param_vec.push_back(std::string(name));
	}

	std::unordered_map<std::string, Type *> name_types {};
	head = PL_new_term_ref();
	list = PL_copy_term_ref(param_types);
	for (unsigned i = 0; PL_get_list_ex(list, head, list); i++) {
		name_types[param_vec[i]] = term_to_pointer<Type>(head);
	}

	std::vector<std::string> capture_vec;
	head = PL_new_term_ref();
	list = PL_copy_term_ref(captures);
	while (PL_get_list_ex(list, head, list)) {
		if (!PL_get_string(head, &name, &len))
			PL_fail;
		capture_vec.push_back(std::string(name));
	}

	std::vector<Type *> capture_type_vec;
	head = PL_new_term_ref();
	list = PL_copy_term_ref(capture_types);
	for (unsigned i = 0; PL_get_list_ex(list, head, list); i++) {
		Type *t = term_to_pointer<Type>(head);
		capture_type_vec.push_back(t);
		name_types[capture_vec[i]] = t;
	}

	Function *lambda_ptr;
	map<string, Value *> name_value_map;
	BasicBlock *save;
	Value *tramp_ptr;
	std::tie(
		lambda_ptr,
		name_value_map,
		save,
		tramp_ptr) = generator->generate_lambda_head(param_vec, capture_vec, capture_type_vec, term_to_pointer<FunctionType>(func_type));
	
	// build list of name, value
	head = PL_new_term_ref();
	list = PL_copy_term_ref(named_values);
	for (auto const &name_value : name_value_map) {
		std::string name = name_value.first;
		Value *value = name_value.second;
		// put name onto list
		if (!PL_unify_list(list, head, list) || !PL_unify_string_chars(head, name.c_str()))
			PL_fail;
		
		// put arg onto list
		if (!PL_unify_list(list, head, list))
			PL_fail;
		if (name_types[name]->isFunctionTy()) {
			if (!PL_unify_term(head,
							   PL_FUNCTOR_CHARS, "func_ptr", 2,
							   PL_POINTER, value,
							   PL_POINTER, name_types[name]))
				PL_fail;
		} else if (!PL_unify_pointer(head, value))
			PL_fail;
	}
	PL_unify_nil_ex(list);

	return PL_unify_pointer(lambda, lambda_ptr)
		&& PL_unify_pointer(save_point, save)
		&& PL_unify_pointer(trampoline, tramp_ptr);
}

static foreign_t generate_lambda_body(term_t unexcised_lambda, term_t func_type, term_t body, term_t tramp, term_t save_point, term_t captures, term_t capture_types, term_t lambda) {
	if (PL_is_functor(body, PL_new_functor(PL_new_atom("func_ptr"), 2))) {
		term_t temp_body = PL_new_term_ref();
		if (!PL_get_arg(1, body, temp_body))
			PL_fail;
		body = temp_body;
	}
	
	std::vector<Value *> captures_vec{};
	term_t head = PL_new_term_ref();
	term_t list = PL_copy_term_ref(captures);
	while (PL_get_list_ex(list, head, list)) {
		captures_vec.push_back(term_to_pointer<Value>(head));
	}

	std::vector<Type *> capture_types_vec{};
	head = PL_new_term_ref();
	list = PL_copy_term_ref(capture_types);
	while (PL_get_list_ex(list, head, list)) {
		capture_types_vec.push_back(term_to_pointer<Type>(head));
	}

	Value *lambda_pointer = generator->generate_lambda_body(term_to_pointer<Function>(unexcised_lambda),
															term_to_pointer<FunctionType>(func_type),
															term_to_pointer<Value>(body),
															term_to_pointer<Value>(tramp),
															term_to_pointer<BasicBlock>(save_point),
															captures_vec, capture_types_vec);
	// return PL_unify_pointer(lambda, lambda_pointer);
	return PL_unify_term(lambda,
						 PL_FUNCTOR_CHARS, "func_ptr", 2,
						 PL_POINTER, lambda_pointer,
						 PL_POINTER, term_to_pointer<FunctionType>(func_type));
}

static foreign_t generate_cast(term_t from, term_t to_type, term_t to) {
	Value *casted = generator->generate_cast(term_to_pointer<Value>(from), term_to_pointer<Type>(to_type));
	return PL_unify_pointer(to, casted);
}

static foreign_t dump_obj_file(term_t name) {
	char *name_ptr;
	if (!PL_get_atom_chars(name, &name_ptr)) {
		PL_fail;
	}
	generator->dump_obj_file(std::string(name_ptr));
	PL_succeed;
}

static foreign_t print_current_module() {
	generator->print_current_module();
	PL_succeed;
}

extern "C" install_t install() {
	PL_register_foreign("codegen_number", 2, reinterpret_cast<pl_function_t>(generate_number), 0);
	PL_register_foreign("codegen_float", 2, reinterpret_cast<pl_function_t>(generate_float), 0);
	PL_register_foreign("codegen_int", 4, reinterpret_cast<pl_function_t>(generate_int), 0);
	PL_register_foreign("codegen_binary", 4, reinterpret_cast<pl_function_t>(generate_binary), 0);
	PL_register_foreign("codegen_fbinary", 4, reinterpret_cast<pl_function_t>(generate_fbinary), 0);
	PL_register_foreign("codegen_func_call", 4, reinterpret_cast<pl_function_t>(generate_func_call), 0);
	PL_register_foreign("codegen_struct", 3, reinterpret_cast<pl_function_t>(generate_struct), 0);
	PL_register_foreign("codegen_cast", 3, reinterpret_cast<pl_function_t>(generate_cast), 0);

	PL_register_foreign("codegen_func_head", 7, reinterpret_cast<pl_function_t>(generate_func_head), 0);
	PL_register_foreign("codegen_func_body", 2, reinterpret_cast<pl_function_t>(generate_func_body), 0);
	PL_register_foreign("codegen_lambda_head", 9, reinterpret_cast<pl_function_t>(generate_lambda_head), 0);
	PL_register_foreign("codegen_lambda_body", 8, reinterpret_cast<pl_function_t>(generate_lambda_body), 0);

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
	PL_register_foreign("print_current_module", 0, reinterpret_cast<pl_function_t>(print_current_module), 0);

	PL_register_foreign("jit_current_module", 0, reinterpret_cast<pl_function_t>(jit_current_module), 0);
	PL_register_foreign("jit_call", 1, reinterpret_cast<pl_function_t>(jit_call), 0);
	PL_register_foreign("dump_obj_file", 1, reinterpret_cast<pl_function_t>(dump_obj_file), 0);

	PL_register_foreign("reset_generator", 0, reinterpret_cast<pl_function_t>(reset_generator), 0);
}
