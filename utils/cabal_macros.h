/*
 * Proxy file to help loading with GHCi. These versions must match what is
 * prescribed in the .stack.yaml.
 */

/* package llvm-general-3.5.1.2 */
#define VERSION_llvm_general "3.5.1.2"
#define MIN_VERSION_llvm_general(major1,major2,minor) (\
  (major1) <  3 || \
  (major1) == 3 && (major2) <  5 || \
  (major1) == 3 && (major2) == 5 && (minor) <= 1)

/* package llvm-general-pure-3.5.1.0 */
#define VERSION_llvm_general_pure "3.5.1.0"
#define MIN_VERSION_llvm_general_pure(major1,major2,minor) (\
  (major1) <  3 || \
  (major1) == 3 && (major2) <  5 || \
  (major1) == 3 && (major2) == 5 && (minor) <= 1)

