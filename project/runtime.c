#include "stdint.h"
#include "stdio.h"

void print_i32(int32_t x) {
  printf("%d\n", x);
}

void print_i1(int x) {
  if (x) {
    printf("true\n");
  } else {
    printf("false\n");
  }
}

void print_i32_arr(int32_t *x, size_t length) {
  printf("ok\n");
  printf("[%d", x[0]);
  for (size_t i = 1; i < length; ++i) {
    printf(", %d", x[i]);
  }
  printf("]\n");
}

void print_i1_arr(int *x, size_t length) {
  // FIXME: this prints always "true"
  printf("[");
  printf(x[0] ? "true" : "false");
  for (size_t i = 1; i < length; ++i) {
    printf(x[i] ? ", true" : ", false");
  }
  printf("]\n");
}







