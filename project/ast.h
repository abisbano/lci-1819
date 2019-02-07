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
  LIT_BOOL_ARR,
  LIT_ARR,
};

struct expr {
  enum expr_type type;
  union {
    int value; // for type == LITERAL || type == BOOL_LIT
    struct {
      size_t id; // for type == VARIABLE
      enum value_type type;
    } var;
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
      int *value;
      int length;
    } c_array; // for type == LIT_ARR || type == LIT_BOOL_ARR
  };
};

struct expr* bool_lit(int v);
struct expr* literal(int v);
struct expr* variable(size_t id);
struct expr* elem_access(size_t id, int index);
struct expr* binop(struct expr *lhs, int op, struct expr *rhs);

LLVMValueRef get_array_size(struct expr *e);
enum value_type get_type(size_t id);

void print_expr(struct expr *expr);
void emit_stack_machine(struct expr *expr);
int emit_reg_machine(struct expr *expr);

enum value_type check_types(struct expr *expr);

void free_expr(struct expr *expr);

/* support struct for generating constant arrays */
struct queue_element {
  struct queue_element *next;
  struct expr *element;
};

struct queue {
  enum value_type type; // can be INTEGER or BOOLEAN
  int length;
  struct queue_element *first;
  struct queue_element *last;
};

struct expr* const_array(struct queue *queue);
struct queue* make_queue(struct expr *first);
struct queue* enqueue(struct queue *queue, struct expr *elem);
struct queue_element *encapsulate(struct expr *expr);

void free_queue(struct queue *queue);

enum stmt_type {
  STMT_SEQ,
  STMT_ASSIGN,
  //  STMT_ASSIGN_ARR_ELEM,
  STMT_IF,
  STMT_WHILE,
  STMT_FOR,
  STMT_PRINT,
};

enum assign_kind {
  A_ELEM,
  A_VAR,
  A_ARR,
  A_UNDEF,
};

struct stmt {
  enum stmt_type type;
  union {
    struct {
      struct expr *lhs, *rhs;
      enum assign_kind kind;
    } assign; // for type == STMT_ASSIGN
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
      size_t id;
      size_t collection;
      struct stmt *body;
    } for_; // for type == STMT_FOR
    struct {
      struct expr *expr;
    } print; // for type == STMT_PRINT
  };
};

struct stmt* make_seq(struct stmt *fst, struct stmt *snd);
struct stmt* make_assign(struct expr *lhs, struct expr *rhs);
struct stmt* make_assign_array_elem(size_t id, int index, struct expr *e);
struct stmt* make_while(struct expr *e, struct stmt *body);
struct stmt* make_for(size_t id, size_t collection, struct stmt *body);
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


