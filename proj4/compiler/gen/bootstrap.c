#include <stdio.h>
#include <stdlib.h>

// assembly function that we are compiling
int entry_point(void* heap);

int main() {
  void* heap = malloc(1000 * 8);
  printf("Result: %d\n", entry_point(heap));
  return 0;
}
