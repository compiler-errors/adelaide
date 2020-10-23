#include "clib.h"

void fp5print(struct string* str) {
  if (str == NULL) {
    DEBUG_PRINTF("String is NULL in std::print");
    printf("<NULL>");
  } else {
    printf("%s", str->payload);
  }
}

NOINLINE struct string* fpP8internalP16transmute_string15int_into_string(i64 i) {
  i64 length = snprintf(NULL, 0, "%"PRId64"", i);

  i64 block_size = (i64) (sizeof(struct string) + (length + 1) * sizeof(i8));
  struct string* string_ptr = gc_alloc_block(block_size, 0 /* string type */, __builtin_frame_address(0));

  string_ptr->length = length;
  snprintf((char*) string_ptr->payload, length + 1, "%"PRId64"", i);

  return string_ptr;
}

NOINLINE struct string* fpP8internalP16transmute_string17float_into_string(f64 f) {
  i64 length = snprintf(NULL, 0, "%f", f);

  i64 block_size = (i64) (sizeof(struct string) + (length + 1) * sizeof(i8));
  struct string* string_ptr = gc_alloc_block(block_size, 0 /* string type */, __builtin_frame_address(0));

  string_ptr->length = length;
  snprintf((char*) string_ptr->payload, length + 1, "%f", f);

  return string_ptr;
}

NOINLINE struct string* fpP8internalP16transmute_string16char_into_string(i8 c) {
  i64 block_size = (i64) (sizeof(struct string) + 2 * sizeof(i8));
  struct string* string_ptr = gc_alloc_block(block_size, 0 /* string type */, __builtin_frame_address(0));

  string_ptr->length = 1;
  string_ptr->payload[0] = c;
  string_ptr->payload[1] = '\0';

  return string_ptr;
}

NOINLINE struct string* fpP9operators19add_string_internal(struct string* a,
                                                   struct string* b) {
  if (a == NULL || b == NULL) {
    PANIC("String is NULL in std::operators::add_string_internal");
  }

  i64 length = strlen((char*) a->payload) + strlen((char*) b->payload);

  i64 block_size = (i64) (sizeof(struct string) + (length + 1) * sizeof(i8));

  gc_register_slot((i8**) &a, 0 /* string type */);
  gc_register_slot((i8**) &b, 0 /* string type */);
  struct string* string_ptr = gc_alloc_block(block_size, 0 /* string type */, __builtin_frame_address(0));
  gc_pop_slot();
  gc_pop_slot();

  string_ptr->length = length;
  snprintf((char*) string_ptr->payload, length + 1, "%s%s", a->payload, b->payload);

  return string_ptr;
}

i1 fpP9operators18eq_string_internal(struct string* a, struct string* b) {
  if (a->length != b->length) {
    return false;
  }

  return strncmp((char*) a->payload, (char*) b->payload, a->length) == 0;
}