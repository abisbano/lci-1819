#include <stdlib.h>
#include <llvm-c/Core.h>

#include "y.tab.h"

enum primitive {
  COPY_I32_ARR,
  COPY_I1_ARR,
};

LLVMValueRef get_primitive(enum primitive p, LLVMModuleRef module, LLVMBuilderRef builder);

LLVMValueRef generate_move_i32_arr(enum primitive p, LLVMModuleRef module, LLVMBuilderRef builder);
LLVMValueRef generate_move_i1_arr(enum primitive p, LLVMModuleRef module, LLVMBuilderRef builder);

LLVMValueRef generate_move_arr(enum primitive p, LLVMTypeRef ptr_type,
                               LLVMModuleRef module, LLVMBuilderRef builder);
