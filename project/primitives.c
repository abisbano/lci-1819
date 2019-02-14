#include <stdlib.h>
#include <stdio.h>

#include "primitives.h"

const char* names[] = {
  [COPY_I32_ARR] = "copy.i32.arr",
  [COPY_I1_ARR] = "copy.i1.arr",
  [ADD_ARR_ARR] = "add.arr.arr",
  [ADD_ARR_I32] = "add.arr.i32",
  [SUB_ARR_ARR] = "sub.arr.arr",
  [SUB_I32_ARR] = "sub.i32.arr",
  [SUB_ARR_I32] = "sub.arr.i32",
  [MUL_ARR_I32] = "mul.arr.i32",
  [DIV_ARR_I32] = "div.arr.i32",
  [DIV_I32_ARR] = "div.i32.arr",
  [EQ_ARR] = "eq.arr",
};

LLVMValueRef get_primitive_result_int(LLVMModuleRef module,
                                      LLVMBuilderRef builder,
                                      enum primitive p,
                                      LLVMValueRef lhs,
                                      LLVMValueRef rhs,
                                      unsigned size,
                                      const char *result_name) {
  LLVMTypeRef ty = LLVMArrayType(LLVMInt32Type(), size);
  LLVMValueRef res = LLVMBuildArrayAlloca(builder, ty, 0, result_name);
  LLVMValueRef res_ptr = LLVMBuildStructGEP(builder, res, 0, "");
  LLVMValueRef args[] = {res_ptr, lhs, rhs, LLVMConstInt(LLVMInt32Type(), size, 0)};
  LLVMValueRef foo = get_primitive(p, module, builder);
  LLVMBuildCall(builder, foo, args, 4, "");
  return res;
}

LLVMValueRef get_primitive_result_bool(LLVMModuleRef module,
                                       LLVMBuilderRef builder,
                                       enum primitive p,
                                       LLVMValueRef lhs,
                                       LLVMValueRef rhs,
                                       unsigned size,
                                       const char *result_name) {
  LLVMValueRef args[] = {lhs, rhs, LLVMConstInt(LLVMInt32Type(), size, 0)};
  LLVMValueRef foo = get_primitive(p, module, builder);
  return LLVMBuildCall(builder, foo, args, 3, result_name);
}

LLVMValueRef get_primitive(enum primitive p, LLVMModuleRef module, LLVMBuilderRef builder) {
  const char *name = names[p];
  LLVMValueRef func = LLVMGetNamedFunction(module, name);
  if (func) return func;

  switch (p) {
  case COPY_I32_ARR:
    return generate_move_i32_arr(p, module, builder);
  case COPY_I1_ARR:
    return generate_move_i1_arr(p, module, builder);
  case ADD_ARR_ARR:
    return generate_add_arr_arr(p, module, builder);
  case SUB_ARR_ARR:
    return generate_sub_arr_arr(p, module, builder);
  case ADD_ARR_I32:
    return generate_add_arr_i32(p, module, builder);
  case SUB_I32_ARR:
    return generate_sub_i32_arr(p, module, builder);
  case SUB_ARR_I32:
    return generate_sub_arr_i32(p, module, builder);
  case MUL_ARR_I32:
    return generate_mul_arr_i32(p, module, builder);
  case DIV_I32_ARR:
    return generate_div_i32_arr(p, module, builder);
  case DIV_ARR_I32:
    return generate_div_arr_i32(p, module, builder);
  case EQ_ARR:
    return generate_eq_arr(p, module, builder);
  default:
    printf("yay! %i\n", p);
    return NULL;
  }
}

LLVMValueRef generate_add_arr_arr(enum primitive p, LLVMModuleRef module, LLVMBuilderRef builder) {
  LLVMValueRef (*foo)(LLVMBuilderRef, LLVMValueRef, LLVMValueRef, const char *) = LLVMBuildAdd;
  return generate_arith_arr_arr(p, module, builder, foo);
}

LLVMValueRef generate_sub_arr_arr(enum primitive p, LLVMModuleRef module, LLVMBuilderRef builder) {
  LLVMValueRef (*foo)(LLVMBuilderRef, LLVMValueRef, LLVMValueRef, const char *) = LLVMBuildSub;
  return generate_arith_arr_arr(p, module, builder, foo);
}

LLVMValueRef generate_add_arr_i32(enum primitive p, LLVMModuleRef module, LLVMBuilderRef builder) {
  LLVMValueRef (*foo)(LLVMBuilderRef, LLVMValueRef, LLVMValueRef, const char *) = LLVMBuildAdd;
  return generate_arith_arr_i32(p, module, builder, foo);
}

LLVMValueRef generate_sub_arr_i32(enum primitive p, LLVMModuleRef module, LLVMBuilderRef builder) {
  LLVMValueRef (*foo)(LLVMBuilderRef, LLVMValueRef, LLVMValueRef, const char *) = LLVMBuildSub;
  return generate_arith_arr_i32(p, module, builder, foo);
}

LLVMValueRef generate_sub_i32_arr(enum primitive p, LLVMModuleRef module, LLVMBuilderRef builder) {
  LLVMValueRef (*foo)(LLVMBuilderRef, LLVMValueRef, LLVMValueRef, const char *) = LLVMBuildSub;
  return generate_arith_i32_arr(p, module, builder, foo);
}

LLVMValueRef generate_mul_arr_i32(enum primitive p, LLVMModuleRef module, LLVMBuilderRef builder) {
  LLVMValueRef (*foo)(LLVMBuilderRef, LLVMValueRef, LLVMValueRef, const char *) = LLVMBuildMul;
  return generate_arith_arr_i32(p, module, builder, foo);
}

LLVMValueRef generate_div_arr_i32(enum primitive p, LLVMModuleRef module, LLVMBuilderRef builder) {
  LLVMValueRef (*foo)(LLVMBuilderRef, LLVMValueRef, LLVMValueRef, const char *) = LLVMBuildSDiv;
  return generate_arith_arr_i32(p, module, builder, foo);
}

LLVMValueRef generate_div_i32_arr(enum primitive p, LLVMModuleRef module, LLVMBuilderRef builder) {
  LLVMValueRef (*foo)(LLVMBuilderRef, LLVMValueRef, LLVMValueRef, const char *) = LLVMBuildSDiv;
  return generate_arith_i32_arr(p, module, builder, foo);
}

LLVMValueRef generate_move_i32_arr(enum primitive p, LLVMModuleRef module, LLVMBuilderRef builder) {
  LLVMTypeRef pointer_type = LLVMPointerType(LLVMInt32Type(), 0);
  return generate_move_arr(p, pointer_type, module, builder);
}

LLVMValueRef generate_move_i1_arr(enum primitive p, LLVMModuleRef module, LLVMBuilderRef builder) {
  LLVMTypeRef pointer_type = LLVMPointerType(LLVMInt1Type(), 0);
  return generate_move_arr(p, pointer_type, module, builder);
}

LLVMValueRef generate_arith_arr_arr(enum primitive p, LLVMModuleRef module, LLVMBuilderRef builder,
                                    LLVMValueRef (*cmp)
                                    (LLVMBuilderRef, LLVMValueRef, LLVMValueRef, const char *)) {
  LLVMTypeRef ptr_type = LLVMPointerType(LLVMInt32Type(), 0);
  LLVMTypeRef params[] = { ptr_type, ptr_type, ptr_type, LLVMInt32Type() };
  LLVMTypeRef ret_type = LLVMVoidType();
  LLVMValueRef func = LLVMAddFunction(module, names[p],
                                      LLVMFunctionType(ret_type, params, 4, 0));

  LLVMBasicBlockRef bb_func = LLVMAppendBasicBlock(func, "entry");
  LLVMBasicBlockRef bb_guard = LLVMAppendBasicBlock(func, "guard");
  LLVMBasicBlockRef bb_body = LLVMAppendBasicBlock(func, "body");
  LLVMBasicBlockRef bb_cont = LLVMAppendBasicBlock(func, "cont");

  LLVMBuilderRef builder_func = LLVMCreateBuilder();

  // retrieve parameters
  LLVMValueRef p_res = LLVMGetParam(func, 0);
  LLVMValueRef p_fst = LLVMGetParam(func, 1);
  LLVMValueRef p_snd = LLVMGetParam(func, 2);
  LLVMValueRef p_size = LLVMGetParam(func, 3);

  // Initialize values
  LLVMPositionBuilderAtEnd(builder_func, bb_func);
  LLVMValueRef res = LLVMBuildAlloca(builder_func, ptr_type, "res");
  LLVMValueRef fst = LLVMBuildAlloca(builder_func, ptr_type, "fst");
  LLVMValueRef snd = LLVMBuildAlloca(builder_func, ptr_type, "snd");
  LLVMValueRef size = LLVMBuildAlloca(builder_func, LLVMInt32Type(), "size");
  LLVMValueRef index = LLVMBuildAlloca(builder_func, LLVMInt32Type(), "i");
  LLVMBuildStore(builder_func, p_res, res);
  LLVMBuildStore(builder_func, p_fst, fst);
  LLVMBuildStore(builder_func, p_snd, snd);
  LLVMBuildStore(builder_func, p_size, size);
  LLVMBuildStore(builder_func, LLVMConstInt(LLVMInt32Type(), 0, 0), index);
  LLVMBuildBr(builder_func, bb_guard);

  // guard
  LLVMPositionBuilderAtEnd(builder_func, bb_guard);
  LLVMValueRef comp_lhs = LLVMBuildLoad(builder_func, index, "");
  LLVMValueRef comp_rhs = LLVMBuildLoad(builder_func, size, "");
  LLVMValueRef comp = LLVMBuildICmp(builder_func, LLVMIntSLT, comp_lhs, comp_rhs, "");
  LLVMBuildCondBr(builder_func, comp, bb_body, bb_cont);

  // body
  LLVMPositionBuilderAtEnd(builder_func, bb_body);

  LLVMValueRef b_index = LLVMBuildLoad(builder_func, index, "");
  LLVMValueRef b_index_value[] = { LLVMBuildSExt(builder_func, b_index, LLVMInt32Type(), "") };
  LLVMValueRef b_fst = LLVMBuildLoad(builder_func, fst, "");
  LLVMValueRef b_fst_gep = LLVMBuildInBoundsGEP(builder_func, b_fst,
                                                b_index_value, 1, "");
  LLVMValueRef b_fst_val = LLVMBuildLoad(builder_func, b_fst_gep, "");

  LLVMValueRef b_snd = LLVMBuildLoad(builder_func, snd, "");
  LLVMValueRef b_snd_gep = LLVMBuildInBoundsGEP(builder_func, b_snd,
                                                b_index_value, 1, "");
  LLVMValueRef b_snd_val = LLVMBuildLoad(builder_func, b_snd_gep, "");


  LLVMValueRef b_result = cmp(builder_func, b_fst_val, b_snd_val, "addtmp");

  LLVMValueRef b_res = LLVMBuildLoad(builder_func, res, "");
  LLVMValueRef b_res_gep = LLVMBuildInBoundsGEP(builder_func, b_res,
                                                b_index_value, 1, "");
  LLVMBuildStore(builder_func, b_result, b_res_gep);

  LLVMValueRef b_index_new = LLVMBuildAdd(builder_func, b_index,
                                          LLVMConstInt(LLVMInt32Type(), 1, 0), "");
  LLVMBuildStore(builder_func, b_index_new, index);
  LLVMBuildBr(builder_func, bb_guard);

  // cont
  LLVMPositionBuilderAtEnd(builder_func, bb_cont);
  LLVMBuildRet(builder_func, 0);

  return func;
}

LLVMValueRef generate_arith_arr_i32(enum primitive p, LLVMModuleRef module, LLVMBuilderRef builder,
                                    LLVMValueRef (*cmp)
                                    (LLVMBuilderRef, LLVMValueRef, LLVMValueRef, const char *)) {
  LLVMTypeRef ptr_type = LLVMPointerType(LLVMInt32Type(), 0);
  LLVMTypeRef params[] = { ptr_type, ptr_type, LLVMInt32Type(), LLVMInt32Type() };
  LLVMTypeRef ret_type = LLVMVoidType();
  LLVMValueRef func = LLVMAddFunction(module, names[p],
                                      LLVMFunctionType(ret_type, params, 4, 0));

  LLVMBasicBlockRef bb_func = LLVMAppendBasicBlock(func, "entry");
  LLVMBasicBlockRef bb_guard = LLVMAppendBasicBlock(func, "guard");
  LLVMBasicBlockRef bb_body = LLVMAppendBasicBlock(func, "body");
  LLVMBasicBlockRef bb_cont = LLVMAppendBasicBlock(func, "cont");

  LLVMBuilderRef builder_func = LLVMCreateBuilder();

  // retrieve parameters
  LLVMValueRef p_res = LLVMGetParam(func, 0);
  LLVMValueRef p_fst = LLVMGetParam(func, 1);
  LLVMValueRef p_snd = LLVMGetParam(func, 2);
  LLVMValueRef p_size = LLVMGetParam(func, 3);

  // Initialize values
  LLVMPositionBuilderAtEnd(builder_func, bb_func);
  LLVMValueRef res = LLVMBuildAlloca(builder_func, ptr_type, "res");
  LLVMValueRef fst = LLVMBuildAlloca(builder_func, ptr_type, "fst");
  LLVMValueRef snd = LLVMBuildAlloca(builder_func, LLVMInt32Type(), "snd");
  LLVMValueRef size = LLVMBuildAlloca(builder_func, LLVMInt32Type(), "size");
  LLVMValueRef index = LLVMBuildAlloca(builder_func, LLVMInt32Type(), "i");
  LLVMBuildStore(builder_func, p_res, res);
  LLVMBuildStore(builder_func, p_fst, fst);
  LLVMBuildStore(builder_func, p_snd, snd);
  LLVMBuildStore(builder_func, p_size, size);
  LLVMBuildStore(builder_func, LLVMConstInt(LLVMInt32Type(), 0, 0), index);
  LLVMValueRef b_snd_val = LLVMBuildLoad(builder_func, snd, "");
  LLVMBuildBr(builder_func, bb_guard);

  // guard
  LLVMPositionBuilderAtEnd(builder_func, bb_guard);
  LLVMValueRef comp_lhs = LLVMBuildLoad(builder_func, index, "");
  LLVMValueRef comp_rhs = LLVMBuildLoad(builder_func, size, "");
  LLVMValueRef comp = LLVMBuildICmp(builder_func, LLVMIntSLT, comp_lhs, comp_rhs, "");
  LLVMBuildCondBr(builder_func, comp, bb_body, bb_cont);

  // body
  LLVMPositionBuilderAtEnd(builder_func, bb_body);

  LLVMValueRef b_index = LLVMBuildLoad(builder_func, index, "");
  LLVMValueRef b_index_value[] = { LLVMBuildSExt(builder_func, b_index, LLVMInt32Type(), "") };
  LLVMValueRef b_fst = LLVMBuildLoad(builder_func, fst, "");
  LLVMValueRef b_fst_gep = LLVMBuildInBoundsGEP(builder_func, b_fst,
                                                b_index_value, 1, "");
  LLVMValueRef b_fst_val = LLVMBuildLoad(builder_func, b_fst_gep, "");

  LLVMValueRef b_result = cmp(builder_func, b_fst_val, b_snd_val, "tmp");

  LLVMValueRef b_res = LLVMBuildLoad(builder_func, res, "");
  LLVMValueRef b_res_gep = LLVMBuildInBoundsGEP(builder_func, b_res,
                                                b_index_value, 1, "");
  LLVMBuildStore(builder_func, b_result, b_res_gep);

  LLVMValueRef b_index_new = LLVMBuildAdd(builder_func, b_index,
                                          LLVMConstInt(LLVMInt32Type(), 1, 0), "");
  LLVMBuildStore(builder_func, b_index_new, index);
  LLVMBuildBr(builder_func, bb_guard);

  // cont
  LLVMPositionBuilderAtEnd(builder_func, bb_cont);
  LLVMBuildRet(builder_func, 0);

  return func;
}

LLVMValueRef generate_arith_i32_arr(enum primitive p, LLVMModuleRef module, LLVMBuilderRef builder,
                                    LLVMValueRef (*cmp)
                                    (LLVMBuilderRef, LLVMValueRef, LLVMValueRef, const char *)) {
  LLVMTypeRef ptr_type = LLVMPointerType(LLVMInt32Type(), 0);
  LLVMTypeRef params[] = { ptr_type, ptr_type, LLVMInt32Type(), LLVMInt32Type() };
  LLVMTypeRef ret_type = LLVMVoidType();
  LLVMValueRef func = LLVMAddFunction(module, names[p],
                                      LLVMFunctionType(ret_type, params, 4, 0));

  LLVMBasicBlockRef bb_func = LLVMAppendBasicBlock(func, "entry");
  LLVMBasicBlockRef bb_guard = LLVMAppendBasicBlock(func, "guard");
  LLVMBasicBlockRef bb_body = LLVMAppendBasicBlock(func, "body");
  LLVMBasicBlockRef bb_cont = LLVMAppendBasicBlock(func, "cont");

  LLVMBuilderRef builder_func = LLVMCreateBuilder();

  // retrieve parameters
  LLVMValueRef p_res = LLVMGetParam(func, 0);
  LLVMValueRef p_fst = LLVMGetParam(func, 1);
  LLVMValueRef p_snd = LLVMGetParam(func, 2);
  LLVMValueRef p_size = LLVMGetParam(func, 3);

  // Initialize values
  LLVMPositionBuilderAtEnd(builder_func, bb_func);
  LLVMValueRef res = LLVMBuildAlloca(builder_func, ptr_type, "res");
  LLVMValueRef fst = LLVMBuildAlloca(builder_func, ptr_type, "fst");
  LLVMValueRef snd = LLVMBuildAlloca(builder_func, LLVMInt32Type(), "snd");
  LLVMValueRef size = LLVMBuildAlloca(builder_func, LLVMInt32Type(), "size");
  LLVMValueRef index = LLVMBuildAlloca(builder_func, LLVMInt32Type(), "i");
  LLVMBuildStore(builder_func, p_res, res);
  LLVMBuildStore(builder_func, p_fst, fst);
  LLVMBuildStore(builder_func, p_snd, snd);
  LLVMBuildStore(builder_func, p_size, size);
  LLVMBuildStore(builder_func, LLVMConstInt(LLVMInt32Type(), 0, 0), index);
  LLVMValueRef b_snd_val = LLVMBuildLoad(builder_func, snd, "");
  LLVMBuildBr(builder_func, bb_guard);

  // guard
  LLVMPositionBuilderAtEnd(builder_func, bb_guard);
  LLVMValueRef comp_lhs = LLVMBuildLoad(builder_func, index, "");
  LLVMValueRef comp_rhs = LLVMBuildLoad(builder_func, size, "");
  LLVMValueRef comp = LLVMBuildICmp(builder_func, LLVMIntSLT, comp_lhs, comp_rhs, "");
  LLVMBuildCondBr(builder_func, comp, bb_body, bb_cont);

  // body
  LLVMPositionBuilderAtEnd(builder_func, bb_body);

  LLVMValueRef b_index = LLVMBuildLoad(builder_func, index, "");
  LLVMValueRef b_index_value[] = { LLVMBuildSExt(builder_func, b_index, LLVMInt32Type(), "") };
  LLVMValueRef b_fst = LLVMBuildLoad(builder_func, fst, "");
  LLVMValueRef b_fst_gep = LLVMBuildInBoundsGEP(builder_func, b_fst,
                                                b_index_value, 1, "");
  LLVMValueRef b_fst_val = LLVMBuildLoad(builder_func, b_fst_gep, "");

  LLVMValueRef b_result = cmp(builder_func, b_snd_val, b_fst_val, "tmp");

  LLVMValueRef b_res = LLVMBuildLoad(builder_func, res, "");
  LLVMValueRef b_res_gep = LLVMBuildInBoundsGEP(builder_func, b_res,
                                                b_index_value, 1, "");
  LLVMBuildStore(builder_func, b_result, b_res_gep);

  LLVMValueRef b_index_new = LLVMBuildAdd(builder_func, b_index,
                                          LLVMConstInt(LLVMInt32Type(), 1, 0), "");
  LLVMBuildStore(builder_func, b_index_new, index);
  LLVMBuildBr(builder_func, bb_guard);

  // cont
  LLVMPositionBuilderAtEnd(builder_func, bb_cont);
  LLVMBuildRet(builder_func, 0);

  return func;
}

LLVMValueRef generate_move_arr(enum primitive p, LLVMTypeRef ptr_type,
                               LLVMModuleRef module, LLVMBuilderRef builder) {
  LLVMTypeRef params[] = { ptr_type, ptr_type, LLVMInt32Type() };
  LLVMTypeRef ret_type = LLVMVoidType();
  LLVMValueRef func = LLVMAddFunction(module, names[p],
                                      LLVMFunctionType(ret_type, params, 3, 0));

  LLVMBasicBlockRef bb_func = LLVMAppendBasicBlock(func, "entry");
  LLVMBasicBlockRef bb_guard = LLVMAppendBasicBlock(func, "guard");
  LLVMBasicBlockRef bb_body = LLVMAppendBasicBlock(func, "body");
  LLVMBasicBlockRef bb_cont = LLVMAppendBasicBlock(func, "cont");

  LLVMBuilderRef builder_func = LLVMCreateBuilder();

  // retrieve parameters
  LLVMValueRef p_fst = LLVMGetParam(func, 0);
  LLVMValueRef p_snd = LLVMGetParam(func, 1);
  LLVMValueRef p_size = LLVMGetParam(func, 2);

  // Initialize values
  LLVMPositionBuilderAtEnd(builder_func, bb_func);
  LLVMValueRef fst = LLVMBuildAlloca(builder_func, ptr_type, "fst");
  LLVMValueRef snd = LLVMBuildAlloca(builder_func, ptr_type, "snd");
  LLVMValueRef size = LLVMBuildAlloca(builder_func, LLVMInt32Type(), "size");
  LLVMValueRef index = LLVMBuildAlloca(builder_func, LLVMInt32Type(), "i");
  LLVMBuildStore(builder_func, p_fst, fst);
  LLVMBuildStore(builder_func, p_snd, snd);
  LLVMBuildStore(builder_func, p_size, size);
  LLVMBuildStore(builder_func, LLVMConstInt(LLVMInt32Type(), 0, 0), index);
  LLVMBuildBr(builder_func, bb_guard);

  // guard
  LLVMPositionBuilderAtEnd(builder_func, bb_guard);
  LLVMValueRef comp_lhs = LLVMBuildLoad(builder_func, index, "");
  LLVMValueRef comp_rhs = LLVMBuildLoad(builder_func, size, "");
  LLVMValueRef comp = LLVMBuildICmp(builder_func, LLVMIntSLT, comp_lhs, comp_rhs, "");
  LLVMBuildCondBr(builder_func, comp, bb_body, bb_cont);

  // body
  LLVMPositionBuilderAtEnd(builder_func, bb_body);

  LLVMValueRef b_index = LLVMBuildLoad(builder_func, index, "");
  LLVMValueRef b_index_value[] = { LLVMBuildSExt(builder_func, b_index, LLVMInt32Type(), "") };
  LLVMValueRef b_snd = LLVMBuildLoad(builder_func, snd, "");
  LLVMValueRef b_snd_gep = LLVMBuildInBoundsGEP(builder_func, b_snd,
                                                b_index_value, 1, "");
  LLVMValueRef b_value = LLVMBuildLoad(builder_func, b_snd_gep, "");
  LLVMValueRef b_fst = LLVMBuildLoad(builder_func, fst, "");
  LLVMValueRef b_fst_gep = LLVMBuildInBoundsGEP(builder_func, b_fst,
                                                b_index_value, 1, "");
  LLVMBuildStore(builder_func, b_value, b_fst_gep);

  LLVMValueRef b_index_new = LLVMBuildAdd(builder_func, b_index,
                                          LLVMConstInt(LLVMInt32Type(), 1, 0), "");
  LLVMBuildStore(builder_func, b_index_new, index);
  LLVMBuildBr(builder_func, bb_guard);

  // cont
  LLVMPositionBuilderAtEnd(builder_func, bb_cont);
  LLVMBuildRet(builder_func, 0);

  return func;
}

LLVMValueRef generate_eq_arr(enum primitive p, LLVMModuleRef module, LLVMBuilderRef builder) {
  return NULL;
}
