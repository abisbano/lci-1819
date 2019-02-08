#include <stdlib.h>
#include <llvm-c/Core.h>

#include "y.tab.h"

enum primitive {
  MOVE_I32_ARR = 0,
  MOVE_I1_ARR = 1,
};

LLVMValueRef get_primitive(enum primitive, LLVMModuleRef module, LLVMBuilderRef builder);

LLVMValueRef generate_move_i32_arr(enum primitive, LLVMModuleRef module, LLVMBuilderRef builder);
LLVMValueRef generate_move_i1_arr(enum primitive, LLVMModuleRef module, LLVMBuilderRef builder);
