/*
 * Proxy file to help loading with GHCi. These versions must match what is
 * prescribed in the .stack.yaml.
 */

/* package llvm-general-3.8.0.2 */
#define VERSION_llvm_general "3.8.0.2"
#define MIN_VERSION_llvm_general(major1,major2,minor) (\
  (major1) <  3 || \
  (major1) == 3 && (major2) <  8 || \
  (major1) == 3 && (major2) == 8 && (minor) <= 0)

/* package llvm-general-pure-3.8.0.0 */
#define VERSION_llvm_general_pure "3.8.0.0"
#define MIN_VERSION_llvm_general_pure(major1,major2,minor) (\
  (major1) <  3 || \
  (major1) == 3 && (major2) <  8 || \
  (major1) == 3 && (major2) == 8 && (minor) <= 0)

