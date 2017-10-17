/*
 * Proxy file to help loading with GHCi. These versions must match what is
 * prescribed in the stack.yaml.
 */

#if __GLASGOW_HASKELL__ < 800

/* LLVM-4.1 */
#if 0
/* package llvm-hs-4.1.0.0 */
#define VERSION_llvm_hs "4.1.0.0"
#define MIN_VERSION_llvm_hs(major1,major2,minor) (\
  (major1) <  4 || \
  (major1) == 4 && (major2) <  1 || \
  (major1) == 4 && (major2) == 1 && (minor) <= 0)

/* package llvm-hs-pure-4.1.0.0 */
#define VERSION_llvm_hs_pure "4.1.0.0"
#define MIN_VERSION_llvm_hs_pure(major1,major2,minor) (\
  (major1) <  4 || \
  (major1) == 4 && (major2) <  1 || \
  (major1) == 4 && (major2) == 1 && (minor) <= 0)
#endif

/* LLVM-3.9 */
#if 0
/* package llvm-hs-3.9.0.0 */
#define VERSION_llvm_hs "3.9.0.0"
#define MIN_VERSION_llvm_hs(major1,major2,minor) (\
  (major1) <  3 || \
  (major1) == 3 && (major2) <  9 || \
  (major1) == 3 && (major2) == 9 && (minor) <= 0)

/* package llvm-hs-pure-3.9.0.0 */
#define VERSION_llvm_hs_pure "3.9.0.0"
#define MIN_VERSION_llvm_hs_pure(major1,major2,minor) (\
  (major1) <  3 || \
  (major1) == 3 && (major2) <  9 || \
  (major1) == 3 && (major2) == 9 && (minor) <= 0)
#endif

#endif

