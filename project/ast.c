#include <stdio.h>
#include <stdlib.h>

#include "ast.h"
#include "y.tab.h"
#include "utils.h"
#include "primitives.h"

const char *type_name(enum value_type t) {
  switch (t) {
  case INTEGER: return "int";
  case BOOLEAN: return "bool";
  case INT_ARRAY: return "int[]";
  case BOOL_ARRAY: return "bool[]";
  case ERROR: return "error";
  default: return "not-a-type";
  }
}

struct expr* bool_lit(int v) {
  struct expr* r = malloc(sizeof(struct expr));
  r->type = BOOL_LIT;
  r->value = v;
  return r;
}

struct expr* literal(int v) {
  struct expr* r = malloc(sizeof(struct expr));
  r->type = LITERAL;
  r->value = v;
  return r;
}

struct expr* variable(size_t id) {
  struct expr* r = malloc(sizeof(struct expr));
  r->type = VARIABLE;
  r->var.id = id;
  r->var.type = get_type(id);
  return r;
}

struct expr* elem_access(size_t id, struct expr *index) {
  struct expr* r = malloc(sizeof(struct expr));
  r->type = ELEM;
  r->elem.id = id;
  r->elem.index = index;
  return r;
}

struct expr* binop(struct expr *lhs, int op, struct expr *rhs) {
  struct expr* r = malloc(sizeof(struct expr));
  r->type = BIN_OP;
  r->binop.lhs = lhs;
  r->binop.op = op;
  r->binop.rhs = rhs;
  return r;
}

struct expr* const_array(struct queue *queue) {
  struct expr* r = malloc(sizeof(struct expr));
  r->type = (queue->type == INTEGER) ? LIT_ARR : LIT_BOOL_ARR;
  int length = queue->length;
  r->c_array.length = length;
  r->c_array.value = calloc(length, sizeof(int));

  struct queue_element *current = queue->first;
  for (int i = 0; i < length; ++i, current = current->next) {
    r->c_array.value[i] = current->element->value;
  }

  free_queue(queue);
  return r;
}

struct queue* make_queue(struct expr *first) {
  if (first->type != LITERAL && first->type != BOOL_LIT) {
    printf("Error: wrong type in array constant.\n");
    return 0;
  }
  struct queue *q = malloc(sizeof(struct queue));
  q->type = (first->type == LITERAL) ? INTEGER : BOOLEAN;
  q->length = 1;
  q->first = q->last = encapsulate(first);
  return q;
}

struct queue* enqueue(struct queue *queue, struct expr *elem) {
  if ((elem->type == LITERAL && queue->type != INTEGER)
      || (elem->type == BOOL_LIT && queue->type != BOOLEAN)) {
    printf("Error: wrong type in array constant.\n");
    return queue;
  }
  struct queue_element *next_el = encapsulate(elem);
  ++(queue->length);
  queue->last->next = next_el;
  queue->last = next_el;

  return queue;
}

struct queue_element *encapsulate(struct expr *expr) {
  // We assume that encapsulate is only called after type checking
  struct queue_element *r = malloc(sizeof(struct queue_element));
  r->element = expr;
  r->next = 0;
  return r;
}

void free_queue(struct queue *queue) {
  struct queue_element *curr = queue->first;
  struct queue_element *next = 0;
  for (int i = 0; i < queue->length; ++i, curr = next) {
    next = curr->next;
    free_expr(curr->element);
    free(curr);
  }
  free(queue);
}

unsigned get_array_size(LLVMTypeRef type) {
  unsigned result = -1;
  while (LLVMGetIntTypeWidth(type) == 0) {
    result = LLVMGetArrayLength(type);
    type = LLVMGetElementType(type);
  }
  return result;
}

enum value_type get_type(size_t id) {
  LLVMValueRef ptr = vector_get(&global_types, id);
  if (!ptr) {
    return UNTYPED;
  }
  LLVMTypeRef t = LLVMGetElementType(LLVMTypeOf(ptr));
  if (LLVMGetIntTypeWidth(t) == 0) {
    LLVMTypeRef inner_type = LLVMGetElementType(t);
    return LLVMGetIntTypeWidth(inner_type) == 1 ? BOOL_ARRAY : INT_ARRAY;
  }
  return LLVMGetIntTypeWidth(t) == 1 ? BOOLEAN : INTEGER;
}

void print_expr(struct expr *expr) {
  switch (expr->type) {
    case BOOL_LIT:
      printf("%s", expr->value ? "true" : "false");
      break;

    case LITERAL:
      printf("%d", expr->value);
      break;

    case VARIABLE:
      printf("%s", string_int_rev(&global_ids, expr->var.id));
      break;

    case ELEM:
      //      printf("%s[%i]", string_int_rev(&global_ids, expr->elem.id), expr->elem.index);
      break;

    case BIN_OP:
      printf("(");
      print_expr(expr->binop.lhs);
      switch (expr->binop.op) {
        case EQ: printf(" == "); break;
        case NE: printf(" != "); break;
        case GE: printf(" >= "); break;
        case LE: printf(" <= "); break;
        default: printf(" %c ", expr->binop.op); break;
      }
      print_expr(expr->binop.rhs);
      printf(")");
      break;

    case LIT_BOOL_ARR:
      printf("[");
      printf("%s", expr->c_array.value[0] ? "true" : "false");
      for (int i = 1; i < expr->c_array.length; ++i) {
        printf(", %s", expr->c_array.value[i] ? "true" : "false");
      }
      printf("]");
      break;

    case LIT_ARR:
      printf("[");
      printf("%i", expr->c_array.value[0]);
      for (int i = 1; i < expr->c_array.length; ++i) {
        printf(", %i", expr->c_array.value[i]);
      }
      printf("]");
      break;
  }
}

static void print_indent(int indent) {
  while (indent--) {
    printf("  ");
  }
}

void print_stmt(struct stmt *stmt, int indent) {
  switch (stmt->type) {
    case STMT_SEQ:
      print_stmt(stmt->seq.fst, indent);
      print_stmt(stmt->seq.snd, indent);
      break;

    case STMT_ASSIGN:
      print_indent(indent);
      print_expr(stmt->assign.lhs);
      printf(" = ");
      print_expr(stmt->assign.rhs);
      printf(";\n");
      break;

    case STMT_PRINT:
      print_indent(indent);
      printf("print ");
      print_expr(stmt->print.expr);
      printf(";\n");
      break;

    case STMT_WHILE:
      print_indent(indent);
      printf("while (");
      print_expr(stmt->while_.cond);
      printf(") {\n");
      print_stmt(stmt->while_.body, indent + 1);
      print_indent(indent);
      printf("}\n");
      break;

    case STMT_FOR:
      print_indent(indent);
      printf("for (");
      /* x */
      printf(": ");
      /* collection */
      printf(") {\n");
      print_stmt(stmt->for_.body, indent + 1);
      print_indent(indent);
      printf("}\n");
      break;

    case STMT_IF:
      print_indent(indent);
      printf("if (");
      print_expr(stmt->ifelse.cond);
      printf(") {\n");
      print_stmt(stmt->ifelse.if_body, indent + 1);
      if (stmt->ifelse.else_body) {
        print_indent(indent);
        printf("} else {\n");
        print_stmt(stmt->ifelse.else_body, indent + 1);
      }
      print_indent(indent);
      printf("}\n");
      break;
  }
}

static int next_reg = 0;

static int gen_reg() {
  return next_reg++;
}

enum value_type check_types(struct expr *expr) {
  switch (expr->type) {
  case BOOL_LIT:
    return BOOLEAN;

  case LITERAL:
    return INTEGER;

  case VARIABLE:
    return expr->var.type;

  case ELEM: {
    if (check_types(expr->elem.index) != INTEGER)
      return ERROR;
    LLVMValueRef ptr = vector_get(&global_types, expr->elem.id);
    LLVMTypeRef t = LLVMGetElementType(LLVMTypeOf(ptr));
    unsigned size = LLVMGetIntTypeWidth(t);
    if (size != 0)
      return ERROR;
    LLVMTypeRef inner_type = LLVMGetElementType(t);
    return LLVMGetIntTypeWidth(inner_type) == 1 ? BOOLEAN : INTEGER;
  }

  case BIN_OP: {
    enum value_type lhs = check_types(expr->binop.lhs);
    enum value_type rhs = check_types(expr->binop.rhs);
    switch (expr->binop.op) {
    case '+':
    case '-':
      if (lhs == INT_ARRAY && rhs == INT_ARRAY)
        return INT_ARRAY;
    case '*':
    case '/':
      if (lhs == INTEGER && rhs == INTEGER)
        return INTEGER;
      else if ((lhs == INT_ARRAY && rhs == INTEGER) ||
               (lhs == INTEGER && rhs == INT_ARRAY))
        return INT_ARRAY;
      else
        return ERROR;

    case EQ:
    case NE:
      if (lhs == rhs && lhs != ERROR)
        return BOOLEAN;
      else
        return ERROR;

    case GE:
    case LE:
    case '>':
    case '<':
      if (lhs == INTEGER && rhs == INTEGER)
        return BOOLEAN;
      else
        return ERROR;

    }
  }
  case LIT_BOOL_ARR:
    return BOOL_ARRAY;
  case LIT_ARR:
    return INT_ARRAY;
  default:
    return ERROR;
  }
}

void free_expr(struct expr *expr) {
  switch (expr->type) {
  case BOOL_LIT:
  case LITERAL:
  case VARIABLE:
    free(expr);
    break;

  case ELEM:
    free_expr(expr->elem.index);
    free(expr);
    break;

  case BIN_OP:
    free_expr(expr->binop.lhs);
    free_expr(expr->binop.rhs);
    free(expr);
    break;
  case LIT_BOOL_ARR:
  case LIT_ARR:
    free(expr->c_array.value);
    free(expr);
    break;
  }
}

struct stmt* make_seq(struct stmt *fst, struct stmt *snd) {
  struct stmt* r = malloc(sizeof(struct stmt));
  r->type = STMT_SEQ;
  r->seq.fst = fst;
  r->seq.snd = snd;
  return r;
}

struct stmt* make_assign(struct expr *lhs, struct expr *rhs) {
  struct stmt* r = malloc(sizeof(struct stmt));
  r->type = STMT_ASSIGN;
  r->assign.lhs = lhs;
  r->assign.rhs = rhs;
  // assignment kind is determined only by lhs
  // type consistency is verified in valid_stmt function.
  if (lhs->type == ELEM) {
    r->assign.kind = A_ELEM;
  } else if (lhs->type == VARIABLE) {
    if (lhs->var.type == INTEGER || lhs->var.type == BOOLEAN) {
      r->assign.kind = A_VAR;
    } else if (lhs->var.type == INT_ARRAY || lhs->var.type == BOOL_ARRAY) {
      r->assign.kind = A_ARR;
    }
  } else {
    r->assign.kind = A_UNDEF;
  }
  return r;
}

struct stmt* make_while(struct expr *e, struct stmt *body) {
  struct stmt* r = malloc(sizeof(struct stmt));
  r->type = STMT_WHILE;
  r->while_.cond = e;
  r->while_.body = body;
  return r;
}

struct stmt* make_for(size_t id, size_t collection, struct stmt *body) {
  struct stmt* r = malloc(sizeof(struct stmt));
  r->type = STMT_FOR;
  r->for_.id = id;
  r->for_.collection = collection;
  r->for_.body = body;
  return r;
}

struct stmt* make_ifelse(struct expr *e, struct stmt *if_body, struct stmt *else_body) {
  struct stmt* r = malloc(sizeof(struct stmt));
  r->type = STMT_IF;
  r->ifelse.cond = e;
  r->ifelse.if_body = if_body;
  r->ifelse.else_body = else_body;
  return r;
}

struct stmt* make_if(struct expr *e, struct stmt *body) {
  return make_ifelse(e, body, NULL);
}

struct stmt* make_print(struct expr *e) {
  struct stmt* r = malloc(sizeof(struct stmt));
  r->type = STMT_PRINT;
  r->print.expr = e;
  return r;
}

void free_stmt(struct stmt *stmt) {
  switch (stmt->type) {
  case STMT_SEQ:
    free_stmt(stmt->seq.fst);
    free_stmt(stmt->seq.snd);
    break;

  case STMT_ASSIGN:
    free_expr(stmt->assign.lhs);
    free_expr(stmt->assign.rhs);
    break;

  case STMT_PRINT:
    free_expr(stmt->print.expr);
    break;

  case STMT_WHILE:
    free_expr(stmt->while_.cond);
    free_stmt(stmt->while_.body);
    break;

  case STMT_FOR:
    free_stmt(stmt->for_.body);
    break;

  case STMT_IF:
    free_expr(stmt->ifelse.cond);
    free_stmt(stmt->ifelse.if_body);
    if (stmt->ifelse.else_body)
      free_stmt(stmt->ifelse.else_body);
    break;
  }

  free(stmt);
}

int valid_stmt(struct stmt *stmt) {
  switch (stmt->type) {
  case STMT_SEQ:
    return valid_stmt(stmt->seq.fst) && valid_stmt(stmt->seq.snd);

  case STMT_ASSIGN:
    // should the language/compiler forbid accessing uninitialized variables?
    // maybe also warn about dead assignments?
    /* Due to grammar, we can only have a variable or an access to array element in the
       left-hand side of the statement, so we only need to check that types
       of the two expressions are the same. */
    return check_types(stmt->assign.lhs) != ERROR &&
      (check_types(stmt->assign.lhs) == check_types(stmt->assign.rhs));

  case STMT_PRINT:
    return check_types(stmt->print.expr) != ERROR && check_types(stmt->print.expr) != UNTYPED;

  case STMT_WHILE:
    return check_types(stmt->while_.cond) == BOOLEAN && valid_stmt(stmt->while_.body);

  case STMT_FOR:
    return /*(check_types(stmt->for_.collection) == BOOL_ARRAY
             || check_types(stmt->for_.collection) == BOOL_ARRAY) && */
      valid_stmt(stmt->for_.body);

  case STMT_IF:
    return
      check_types(stmt->ifelse.cond) == BOOLEAN &&
      valid_stmt(stmt->ifelse.if_body) &&
      (stmt->ifelse.else_body == NULL || valid_stmt(stmt->ifelse.else_body));
  }
}

LLVMValueRef codegen_expr(struct expr *expr, LLVMModuleRef module,
                          LLVMBuilderRef builder, int *error_status) {
  switch (expr->type) {
  case BOOL_LIT:
    return LLVMConstInt(LLVMInt1Type(), expr->value, 0);

  case LITERAL:
    return LLVMConstInt(LLVMInt32Type(), expr->value, 0);

  case VARIABLE: {
    switch (expr->var.type) {
    case INTEGER:
    case BOOLEAN:
      return LLVMBuildLoad(builder, vector_get(&global_types, expr->var.id),
                           "loadtmp");
    case INT_ARRAY:
    case BOOL_ARRAY:
      return vector_get(&global_types, expr->var.id);
    default:
      return NULL;
    }

  }

  case ELEM: {
    LLVMValueRef index[] = { LLVMConstInt(LLVMInt32Type(), 0, 0),
                             codegen_expr(expr->elem.index, module, builder, error_status) };
    LLVMValueRef ptr = LLVMBuildInBoundsGEP(builder, vector_get(&global_types, expr->elem.id),
                                            index, 2, "ptrtmp");
    return LLVMBuildLoad(builder, ptr, "loadtmp");
  }

  case BIN_OP: {
    enum value_type type_lhs = check_types(expr->binop.lhs);
    enum value_type type_rhs = check_types(expr->binop.rhs);
    LLVMValueRef lhs = codegen_expr(expr->binop.lhs, module, builder, error_status);
    LLVMValueRef rhs = codegen_expr(expr->binop.rhs, module, builder, error_status);

    if (type_lhs == INTEGER && type_lhs == type_rhs) {
      switch (expr->binop.op) {
      case '+': return LLVMBuildAdd(builder, lhs, rhs, "addtmp");
      case '-': return LLVMBuildSub(builder, lhs, rhs, "subtmp");
      case '*': return LLVMBuildMul(builder, lhs, rhs, "multmp");
      case '/': return LLVMBuildSDiv(builder, lhs, rhs, "divtmp");

      case EQ: return LLVMBuildICmp(builder, LLVMIntEQ, lhs, rhs, "eqtmp");
      case NE: return LLVMBuildICmp(builder, LLVMIntNE, lhs, rhs, "netmp");

      case GE: return LLVMBuildICmp(builder, LLVMIntSGE, lhs, rhs, "getmp");
      case LE: return LLVMBuildICmp(builder, LLVMIntSLE, lhs, rhs, "letmp");
      case '>': return LLVMBuildICmp(builder, LLVMIntSGT, lhs, rhs, "gttmp");
      case '<': return LLVMBuildICmp(builder, LLVMIntSLT, lhs, rhs, "lttmp");
      }
    } else if (type_lhs == INT_ARRAY && type_lhs == type_rhs) {
      unsigned size_lhs = get_array_size(LLVMTypeOf(lhs));
      unsigned size_rhs = get_array_size(LLVMTypeOf(rhs));
      if (size_lhs != size_rhs) {
        printf("Error: operation on arrays of dirrefent sizes\n");
        *error_status = 1;
        return NULL;
      }
      LLVMValueRef lhs_ptr = LLVMBuildStructGEP(builder, lhs, 0, "ptr");
      LLVMValueRef rhs_ptr = LLVMBuildStructGEP(builder, rhs, 0, "ptr");
      switch (expr->binop.op) {
      case '+': {
        return get_primitive_result_int(module, builder,
                                        ADD_ARR_ARR, lhs_ptr, rhs_ptr,
                                        size_lhs, "addtmp");
      }
      case '-': {
        return get_primitive_result_int(module, builder,
                                        SUB_ARR_ARR, lhs_ptr, rhs_ptr,
                                        size_lhs, "subtmp");
      }
      case EQ: {
        return get_primitive_result_bool(module, builder,
                                        EQ_ARR, lhs_ptr, rhs_ptr,
                                        size_lhs, "eqtmp");
      }
      case NE: {
        return get_primitive_result_bool(module, builder,
                                        NE_ARR, lhs_ptr, rhs_ptr,
                                        size_lhs, "netmp");
      }
      default:
        /* unsupported */
        return NULL;
      }
    } else if ((type_lhs == INT_ARRAY && type_rhs == INTEGER) ||
               (type_lhs == INTEGER && type_rhs == INT_ARRAY)) {
      unsigned size;
      LLVMValueRef ptr;
      LLVMValueRef scalar;
      if (type_lhs == INT_ARRAY) {
        size = get_array_size(LLVMTypeOf(lhs));
        ptr = LLVMBuildStructGEP(builder, lhs, 0, "ptr");
        scalar = rhs;
      } else {
        size = get_array_size(LLVMTypeOf(rhs));
        ptr = LLVMBuildStructGEP(builder, rhs, 0, "ptr");
        scalar = lhs;
      }

      switch (expr->binop.op) {
      case '+': {
        return get_primitive_result_int(module, builder,
                                        ADD_ARR_I32, ptr, scalar,
                                        size, "addtmp");
      }
      case '-': {
        if (type_lhs == INT_ARRAY)
          return get_primitive_result_int(module, builder,
                                          SUB_ARR_I32, ptr, scalar,
                                          size, "subtmp");
        else
          return get_primitive_result_int(module, builder,
                                          SUB_I32_ARR, ptr, scalar,
                                          size, "subtmp");
      }
      case '*': {
        return get_primitive_result_int(module, builder,
                                        MUL_ARR_I32, ptr, scalar,
                                        size, "multmp");
      }
      case '/': {
        if (type_lhs == INT_ARRAY)
          return get_primitive_result_int(module, builder,
                                          DIV_ARR_I32, ptr, scalar,
                                          size, "divtmp");
        else
          return get_primitive_result_int(module, builder,
                                          DIV_I32_ARR, ptr, scalar,
                                          size, "divtmp");
      }
      default:
        /* unsupported */
        return NULL;
      }
    }
  }
  case LIT_ARR: {
    LLVMValueRef *values = calloc(expr->c_array.length, sizeof(LLVMValueRef));
    for (int i = 0; i < expr->c_array.length; ++i) {
      values[i] = LLVMConstInt(LLVMInt32Type(), expr->c_array.value[i], 0);
    }
    LLVMValueRef array = LLVMConstArray(LLVMInt32Type(), values, expr->c_array.length);
    LLVMValueRef global = LLVMAddGlobal(module, LLVMTypeOf(array), "array");
    LLVMSetInitializer(global, array);
    return global;
  }
  case LIT_BOOL_ARR: {
    LLVMValueRef *values = calloc(expr->c_array.length, sizeof(LLVMValueRef));
    for (int i = 0; i < expr->c_array.length; ++i) {
      values[i] = LLVMConstInt(LLVMInt1Type(), expr->c_array.value[i], 0);
    }
    LLVMValueRef array = LLVMConstArray(LLVMInt1Type(), values, expr->c_array.length);
    LLVMValueRef global = LLVMAddGlobal(module, LLVMTypeOf(array), "array");
    LLVMSetInitializer(global, array);
    return global;
  }
  default: break;
  }
  return NULL;
}

void codegen_stmt(struct stmt *stmt, LLVMModuleRef module,
                  LLVMBuilderRef builder, int *error_status) {
  switch (stmt->type) {
  case STMT_SEQ: {
    codegen_stmt(stmt->seq.fst, module, builder, error_status);
    codegen_stmt(stmt->seq.snd, module, builder, error_status);
    break;
  }

  case STMT_ASSIGN: {
    LLVMValueRef lhs;
    LLVMValueRef rhs = codegen_expr(stmt->assign.rhs, module, builder, error_status);
    switch (stmt->assign.kind) {
    case A_ELEM: {
      LLVMValueRef index[] = { LLVMConstInt(LLVMInt32Type(), 0, 0),
                               codegen_expr(stmt->assign.lhs->elem.index, module,
                                            builder, error_status) };
      lhs = LLVMBuildInBoundsGEP(builder, vector_get(&global_types, stmt->assign.lhs->elem.id),
                                 index, 2, "ref");
      LLVMBuildStore(builder, rhs, lhs);
      break;
    }
    case A_VAR:
      lhs = vector_get(&global_types, stmt->assign.lhs->var.id);
      LLVMBuildStore(builder, rhs, lhs);
      break;
    case A_ARR: {
      lhs = vector_get(&global_types, stmt->assign.lhs->var.id);
      LLVMValueRef lhs_ptr = LLVMBuildStructGEP(builder, lhs, 0, "lhs");
      unsigned size_lhs = get_array_size(LLVMTypeOf(lhs));
      unsigned size_rhs = get_array_size(LLVMTypeOf(rhs));
      if (size_lhs != size_rhs) {
        printf("Error: operation on arrays of dirrefent sizes");
        *error_status = 1;
        return;
      }

      // retrieve primitive to call
      LLVMValueRef func;
      switch (stmt->assign.lhs->var.type) {
      case INT_ARRAY:
        func = get_primitive(COPY_I32_ARR, module, builder);
        break;
      case BOOL_ARRAY:
        func = get_primitive(COPY_I1_ARR, module, builder);
        break;
      default:
        func = NULL;
        return;
      }

      // create arguments array
      LLVMValueRef rhs_ptr = LLVMBuildStructGEP(builder, rhs, 0, "rhs");
      LLVMValueRef args[] = { lhs_ptr, rhs_ptr, LLVMConstInt(LLVMInt32Type(), size_lhs, 0) };
      LLVMBuildCall(builder, func, args, 3, "");
      break;
      }
    default:
      break;
    }
    break;
  }

  case STMT_PRINT: {
    enum value_type arg_type = check_types(stmt->print.expr);
    LLVMValueRef print_fn;
    LLVMValueRef args[2];
    int args_num;
    switch (arg_type) {
    case INTEGER: {
      print_fn = LLVMGetNamedFunction(module, "print_i32");
      args[0] = codegen_expr(stmt->print.expr, module, builder, error_status);
      args_num = 1;
      break;
    }
    case BOOLEAN: {
      print_fn = LLVMGetNamedFunction(module, "print_i1");
      args[0] = codegen_expr(stmt->print.expr, module, builder, error_status);
      args_num = 1;
      break;
    }
    case INT_ARRAY: {
      print_fn = LLVMGetNamedFunction(module, "print_i32_arr");
      LLVMValueRef arr = codegen_expr(stmt->print.expr, module, builder, error_status);
      if (!arr) {
        return;
      }
      args[0] = LLVMBuildStructGEP(builder, arr, 0, "");
      args[1] = LLVMConstInt(LLVMInt32Type() , get_array_size(LLVMTypeOf(arr)), 0);
      args_num = 2;
      break;
    }
    case BOOL_ARRAY: {
      print_fn = LLVMGetNamedFunction(module, "print_i1_arr");
      LLVMValueRef arr = codegen_expr(stmt->print.expr, module, builder, error_status);
      if (!arr) {
        return;
      }
      args[0] = LLVMBuildStructGEP(builder, arr, 0, "");
      args[1] = LLVMConstInt(LLVMInt32Type() , get_array_size(LLVMTypeOf(arr)), 0);
      args_num = 2;
      break;
    }
    default:
      return;
    }
    LLVMBuildCall(builder, print_fn, args, args_num, "");
    break;
  }

  case STMT_WHILE: {
    LLVMValueRef func = LLVMGetBasicBlockParent(LLVMGetInsertBlock(builder));
    LLVMBasicBlockRef cond_bb = LLVMAppendBasicBlock(func, "cond");
    LLVMBasicBlockRef body_bb = LLVMAppendBasicBlock(func, "body");
    LLVMBasicBlockRef cont_bb = LLVMAppendBasicBlock(func, "cont");

    LLVMBuildBr(builder, cond_bb);

    LLVMPositionBuilderAtEnd(builder, cond_bb);
    LLVMValueRef cond = codegen_expr(stmt->while_.cond, module, builder, error_status);
    LLVMBuildCondBr(builder, cond, body_bb, cont_bb);

    LLVMPositionBuilderAtEnd(builder, body_bb);
    codegen_stmt(stmt->while_.body, module, builder, error_status);
    LLVMBuildBr(builder, cond_bb);

    LLVMPositionBuilderAtEnd(builder, cont_bb);
    break;
  }

  case STMT_FOR: {
    LLVMValueRef array = vector_get(&global_types, stmt->for_.collection);
    LLVMTypeRef array_type = LLVMGetElementType(LLVMTypeOf(array));
    LLVMTypeRef base_type = LLVMGetElementType(array_type);
    unsigned size = LLVMGetArrayLength(array_type);

    LLVMValueRef element = vector_get(&global_types, stmt->for_.id);

    for (unsigned i = 0; i < size; ++i) {
      LLVMValueRef ptr = LLVMBuildStructGEP(builder, array, i, "ptr");
      LLVMValueRef val = LLVMBuildLoad(builder, ptr, "x");
      LLVMBuildStore(builder, val, element);

      codegen_stmt(stmt->for_.body, module, builder, error_status);

      LLVMValueRef new_val = LLVMBuildLoad(builder, element, "new_x");
      LLVMBuildStore(builder, new_val, ptr);
    }

    break;
  }

  case STMT_IF: {
    LLVMValueRef func = LLVMGetBasicBlockParent(LLVMGetInsertBlock(builder));
    LLVMBasicBlockRef body_bb = LLVMAppendBasicBlock(func, "body");
    LLVMBasicBlockRef else_bb = LLVMAppendBasicBlock(func, "else");
    LLVMBasicBlockRef cont_bb = LLVMAppendBasicBlock(func, "cont");

    LLVMValueRef cond = codegen_expr(stmt->ifelse.cond, module, builder, error_status);
    LLVMBuildCondBr(builder, cond, body_bb, else_bb);

    LLVMPositionBuilderAtEnd(builder, body_bb);
    codegen_stmt(stmt->ifelse.if_body, module, builder, error_status);
    LLVMBuildBr(builder, cont_bb);

    LLVMPositionBuilderAtEnd(builder, else_bb);
    if (stmt->ifelse.else_body) {
      codegen_stmt(stmt->ifelse.else_body, module, builder, error_status);
    }
    LLVMBuildBr(builder, cont_bb);

    LLVMPositionBuilderAtEnd(builder, cont_bb);
    break;
  }
  }
}

struct decl_type* make_decl_type(enum value_type t, int s) {
  struct decl_type* d = malloc(sizeof(struct decl_type));
  d->type = t;
  d->size = s;
  return d;
}

void free_decl_type(struct decl_type *decl) {
  free(decl);
}

