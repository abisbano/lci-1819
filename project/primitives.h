#include <stdlib.h>
#include <llvm-c/Core.h>

#include "y.tab.h"

enum primitive {
  COPY_I32_ARR,
  COPY_I1_ARR,
  ADD_ARR_ARR,
  ADD_I32_ARR,
  SUB_ARR_ARR,
  SUB_I32_ARR,
  MUL_I32_ARR,
  DIV_I32_ARR,
  EQ_ARR,
};

LLVMValueRef get_primitive_result_int(LLVMModuleRef module,
                                      LLVMBuilderRef builder,
                                      enum primitive p,
                                      LLVMValueRef lhs,
                                      LLVMValueRef rhs,
                                      unsigned size,
                                      const char *result_name);

LLVMValueRef get_primitive_result_bool(LLVMModuleRef module,
                                      LLVMBuilderRef builder,
                                      enum primitive p,
                                      LLVMValueRef lhs,
                                      LLVMValueRef rhs,
                                      unsigned size,
                                      const char *result_name);

LLVMValueRef get_primitive(enum primitive p, LLVMModuleRef module, LLVMBuilderRef builder);

LLVMValueRef generate_move_i32_arr(enum primitive p, LLVMModuleRef module, LLVMBuilderRef builder);
LLVMValueRef generate_move_i1_arr(enum primitive p, LLVMModuleRef module, LLVMBuilderRef builder);

LLVMValueRef generate_move_arr(enum primitive p, LLVMTypeRef ptr_type,
                               LLVMModuleRef module, LLVMBuilderRef builder);
