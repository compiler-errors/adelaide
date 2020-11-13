// fail

// Try to redefine Option so it conflicts with std::Option.
// Since this is an _EXPLICIT_ import, then die.

use std::*.

enum Option {}