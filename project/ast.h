#include <stdlib.h>
#include <llvm-c/Core.h>

#include "y.tab.h"

enum value_type {
  ERROR = -1,
  UNTYPED = 0,
  INTEGER = 1,
  BOOLEAN = 2,
  INT_ARRAY = 3,
  BOOL_ARRAY = 4,
};

const char *type_name(enum value_type t);

enum expr_type {
  BOOL_LIT,
  LITERAL,
  VARIABLE,
  ELEM,
  BIN_OP,
  ARRAY,
  ARRAY_ELEM,
};

struct expr {
  enum expr_type type;
  union {
    int value; // for type == LITERAL || type == BOOL_LIT
    size_t id; // for type == VARIABLE
    struct {
      size_t id;
      int index;
    } elem;
    struct {
      struct expr *lhs;
      struct expr *rhs;
      int op;
    } binop; // for type == BIN_OP
    struct {
      int length;
      struct expr *first;
      struct expr *last;
    } array; // for type == ARRAY
    struct {
      struct expr *elem;
      struct expr *next;
    } array_elem; // for type == ARRAY_ELEM
  };
};

struct expr* bool_lit(int v);
struct expr* literal(int v);
struct expr* variable(size_t id);
struct expr* elem_access(size_t id, int index);
struct expr* binop(struct expr *lhs, int op, struct expr *rhs);
struct expr* array(struct expr *fst);
struct expr* enqueue(struct expr *arr, struct expr *el);
struct expr* encapsulate(struct expr *e);

void print_expr(struct expr *expr);
void emit_stack_machine(struct expr *expr);
int emit_reg_machine(struct expr *expr);

enum value_type check_types(struct expr *expr);

void free_expr(struct expr *expr);

enum stmt_type {
  STMT_SEQ,
  STMT_ASSIGN,
  STMT_ASSIGN_ARR_ELEM,
  STMT_IF,
  STMT_WHILE,
  STMT_PRINT,
};

struct stmt {
  enum stmt_type type;
  union {
    struct {
      size_t id;
      struct expr *expr;
    } assign; // for type == STMT_ASSIGN
    struct {
      size_t id;
      int index;
      struct expr *expr;
    } assign_arr_elem; // for type == STMT_ASSIGN_ARR_ELEM
    struct {
      struct stmt *fst, *snd;
    } seq; // for type == STMT_SEQ
    struct {
      struct expr *cond;
      struct stmt *if_body, *else_body;
    } ifelse; // for type == STMT_IF
    struct {
      struct expr *cond;
      struct stmt *body;
    } while_; // for type == STMT_WHILE
    struct {
      struct expr *expr;
    } print; // for type == STMT_PRINT
  };
};

struct stmt* make_seq(struct stmt *fst, struct stmt *snd);
struct stmt* make_assign(size_t id, struct expr *e);
struct stmt* make_assign_array_elem(size_t id, int index, struct expr *e);
struct stmt* make_while(struct expr *e, struct stmt *body);
struct stmt* make_ifelse(struct expr *e, struct stmt *if_body, struct stmt *else_body);
struct stmt* make_if(struct expr *e, struct stmt *body);
struct stmt* make_print(struct expr *e);
void free_stmt(struct stmt *stmt);
void print_stmt(struct stmt *stmt, int indent);
int valid_stmt(struct stmt *stmt);

LLVMValueRef codegen_expr(struct expr *expr, LLVMModuleRef module, LLVMBuilderRef builder);
void codegen_stmt(struct stmt *stmt, LLVMModuleRef module, LLVMBuilderRef builder);

struct decl_type {
  enum value_type type;
  int size;
};

struct decl_type* make_decl_type(enum value_type t, int s);
void print_decl_type(struct decl_type *decl);
void free_decl_type(struct decl_type *decl);


