#include "clib.h"

NOINLINE struct string* gc_alloc_string(i8* string, i64 length) {
  i64 block_size = (i64) (sizeof(struct string) + (length + 1) * sizeof(i8));
  struct string* string_ptr = gc_alloc_block(block_size, 0 /* string type */, __builtin_frame_address(0));

  string_ptr->length = length;
  snprintf((char*) string_ptr->payload, length+1, "%s", string);

  return string_ptr;
}

NOINLINE i8* gc_alloc_array(i64 element_size, i64 elements, i16 type) {
  // TODO: I should not allocate any array if the # of elements is 0.

  i64 bytes = (i64) (sizeof(struct array) + sizeof(i8) * element_size * elements);
  struct array* array_ptr = gc_alloc_block(bytes, type, __builtin_frame_address(0));

  array_ptr->element_size = element_size;
  array_ptr->length = elements;
  DEBUG_PRINTF("Allocating array: ELEM_SIZE=%"PRId64", LENGTH=%"PRId64"\n", element_size, elements);

  return (i8*) array_ptr;
}

NOINLINE i8* gc_alloc_object(i64 size, i16 type) {
  i8* ptr = gc_alloc_block(size, type, __builtin_frame_address(0));
  return ptr;
}

i1 string_eq_literal(struct string* string, i8* literal, i64 literal_length) {
    if (string == NULL) {
        return false;
    }

    if (string->length != literal_length) {
        return false;
    }

    return strncmp((char*) string->payload, (char*) literal, literal_length) == 0;
}

i8 match_panic() {
    fprintf(stderr, "PANIC: Failed to find successful match branch\n");
    exit(1);
}