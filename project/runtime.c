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
  printf("[%d", x[0]);
  for (size_t i = 1; i < length; ++i) {
    printf(", %d", x[i]);
  }
  printf("]\n");
}

void print_i1_arr(int32_t *x, size_t length) {
  int access = length/4;
  int reminder = length %4;

  printf("[");
  size_t i = 0;
  for (i = 0; i < access; ++i) {
    if (i > 0) printf(", ");
    printf((x[i]) & (1) ? "true, " : "false, ");
    printf((x[i]) & (1 << 8) ? "true, " : "false, ");
    printf((x[i]) & (1 << 16) ? "true, " : "false, ");
    printf((x[i]) & (1 << 24) ? "true" : "false");
  }
  if (reminder > 0) {
    if (i > 0) {
      printf(", ");
    }
    printf((x[0]) & (1) ? "true" : "false");
    for (size_t j = 1; j < reminder; ++j) {
      printf((x[i]) & (1 << (8*j)) ? ", true" : ", false");
    }
  }
  printf("]\n");
}
