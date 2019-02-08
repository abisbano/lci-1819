#include <stdlib.h>

#include "primitives.h"

const char* names[] = {
  [COPY_I32_ARR] = "copy.i32.arr",
  [COPY_I1_ARR] = "copy.i1.arr",
};

LLVMValueRef get_primitive(enum primitive p, LLVMModuleRef module, LLVMBuilderRef builder) {
  const char *name = names[p];
  LLVMValueRef func = LLVMGetNamedFunction(module, name);
  if (func) return func;

  switch (p) {
  case COPY_I32_ARR:
    return generate_move_i32_arr(p, module, builder);
  case COPY_I1_ARR:
    return generate_move_i1_arr(p, module, builder);
  }
}

LLVMValueRef generate_move_i32_arr(enum primitive p, LLVMModuleRef module, LLVMBuilderRef builder) {
  LLVMTypeRef pointer_type = LLVMPointerType(LLVMInt32Type(), 0);
  return generate_move_arr(p, pointer_type, module, builder);
}

LLVMValueRef generate_move_i1_arr(enum primitive p, LLVMModuleRef module, LLVMBuilderRef builder) {
  LLVMTypeRef pointer_type = LLVMPointerType(LLVMInt1Type(), 0);
  return generate_move_arr(p, pointer_type, module, builder);
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
