#include <sys/mman.h>
#include <stdio.h>
#include <stdlib.h>

void die(const char* name) {
  printf("die: %s\n", name);
  exit(1);
}

void* setup_stack() {
 void* m = mmap(NULL, 512*1024*1024, PROT_READ | PROT_WRITE, MAP_PRIVATE | MAP_ANON, -1, 0);
  if (m == MAP_FAILED) {
    die("setup_stack");
  }
  return m;
}
