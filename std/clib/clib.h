#ifndef __CLIB_H
#define __CLIB_H

#include <inttypes.h>
#include <stdlib.h>
#include <stdio.h>
#include <stdbool.h>
#include <string.h>

#define f64 double
#define i64 uint64_t
#define i32 uint32_t
#define i16 uint16_t
#define i8 uint8_t
#define i1 bool

#define DEBUG

#ifdef DEBUG
#define DEBUG_PRINTF(...) if (true) { fprintf(stderr, " > " __VA_ARGS__); }
#else
#define DEBUG_PRINTF(...) {}
#endif

#define PANIC(...) { printf(" PANIC: " __VA_ARGS__); exit(-1); }
#define NOINLINE __attribute__((noinline))
#define INLINE __attribute__((always_inline))

struct array {
  i64 length;
  i64 element_size;
  i8 payload[];
};

struct string {
  i64 length;
  i8 payload[];
};

struct closure {
  void(*closure_fn_ptr)();
  i64 closure_env_ty;
  i8* closure_env_ptr;
};

inline void ensure_bounds_or_panic(const char* type, i64 size, i64 idx) {
  if (idx < 0 || idx >= size) {
    PANIC("Tried to deref %s (size=%"PRId64") at index %"PRId64"\n",
            type, size, idx);
    exit(1);
  }
}

void gc_register_slot(i8** lval, i16 type);
void gc_pop_slot(void);
void* gc_alloc_block(i64 size, i16 type, i8* cheshire_stack_root);
void gc(i8* cheshire_stack_root);

#endif // __CLIB_H
