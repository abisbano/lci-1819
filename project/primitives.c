#include <stdlib.h>

#include "primitives.h"

const char* names[100] = {
  [MOVE_I32_ARR] = "move_i32_arr",
  [MOVE_I1_ARR] = "move_i1_arr"
};

LLVMValueRef get_primitive(enum primitive p, LLVMModuleRef module, LLVMBuilderRef builder) {
  const char *name = names[p];
  LLVMValueRef func = LLVMGetNamedFunction(module, name);
  if (func) return func;

  switch (p) {
  case MOVE_I32_ARR:
    return generate_move_i32_arr(p, module, builder);
  case MOVE_I1_ARR:
    return generate_move_i1_arr(p, module, builder);
  }
}

LLVMValueRef generate_move_i32_arr(enum primitive p, LLVMModuleRef module, LLVMBuilderRef builder) {
  LLVMTypeRef params[] = { LLVMPointerType(LLVMInt32Type(), 0),
                           LLVMPointerType(LLVMInt32Type(), 0),
                           LLVMInt32Type() };
  LLVMTypeRef ret_type = LLVMVoidType();
  LLVMValueRef func = LLVMAddFunction(module, names[p],
                                      LLVMFunctionType(ret_type, params, 3, 0));
  /* TODO: generate body */
  return func;
}

LLVMValueRef generate_move_i1_arr(enum primitive p, LLVMModuleRef module, LLVMBuilderRef builder) {
  LLVMTypeRef params[] = { LLVMPointerType(LLVMInt1Type(), 0),
                           LLVMPointerType(LLVMInt1Type(), 0),
                           LLVMInt32Type() };
  LLVMTypeRef ret_type = LLVMVoidType();
  LLVMValueRef func =  LLVMAddFunction(module, names[p],
                                       LLVMFunctionType(ret_type, params, 3, 0));
  /* TODO: generate body */
  return func;
}
