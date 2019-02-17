#include <stdlib.h>
#include <llvm-c/Core.h>

#include "y.tab.h"

enum primitive {
  COPY_I32_ARR,
  COPY_I1_ARR,
  ADD_ARR_ARR,
  ADD_ARR_I32,
  SUB_ARR_ARR,
  SUB_I32_ARR,
  SUB_ARR_I32,
  MUL_ARR_I32,
  DIV_ARR_I32,
  DIV_I32_ARR,
  EQ_ARR,
  NE_ARR,
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

LLVMValueRef generate_arith_arr_arr(enum primitive p, LLVMModuleRef module, LLVMBuilderRef builder,
                                    LLVMValueRef (*)
                                    (LLVMBuilderRef, LLVMValueRef, LLVMValueRef, const char *));
LLVMValueRef generate_arith_arr_i32(enum primitive p, LLVMModuleRef module, LLVMBuilderRef builder,
                                    LLVMValueRef (*)
                                    (LLVMBuilderRef, LLVMValueRef, LLVMValueRef, const char *));
LLVMValueRef generate_arith_i32_arr(enum primitive p, LLVMModuleRef module, LLVMBuilderRef builder,
                                    LLVMValueRef (*)
                                    (LLVMBuilderRef, LLVMValueRef, LLVMValueRef, const char *));
LLVMValueRef generate_move_arr(enum primitive p, LLVMTypeRef ptr_type,
                               LLVMModuleRef module, LLVMBuilderRef builder);
LLVMValueRef generate_cmp_arr(enum primitive p, LLVMModuleRef module, LLVMBuilderRef builder,
                              LLVMIntPredicate pred);
