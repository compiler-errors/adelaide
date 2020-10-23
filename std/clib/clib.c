#include "clib.h"

// Stuff having to do with garbage collection
#include "gc.c"

// Functions explicitly used by the tr codegen
#include "builtin.c"

// Functions that are exported in std/internal/**.ch
// These are (by virtue of their being exported by cheshire itself) also decorated.
#include "internal.c"

// Used by LLVM GC strategy, stack walking
#include "statepoint.c"