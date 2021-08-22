#ifndef JIKKA_DIVMOD_HPP
#define JIKKA_DIVMOD_HPP
/**
 * @file jikka/divmod.hpp
 * @author Kimiyuki Onaka
 * @copyright Apache License 2.0
 */
#include <cassert>
#include <cstdint>

namespace jikka {

inline int64_t floordiv(int64_t n, int64_t d) {
  assert(d != 0);
  return n / d - ((n ^ d) < 0 && n % d);
}

inline int64_t floormod(int64_t n, int64_t d) {
  assert(d != 0);
  n %= d;
  return (n < 0 ? n + d : n);
}

inline int64_t ceildiv(int64_t n, int64_t d) {
  assert(d != 0);
  return n / d + ((n ^ d) >= 0 && n % d);
}

inline int64_t ceilmod(int64_t n, int64_t d) {
  assert(d != 0);
  return n - ceildiv(n, d) * d;
}

inline int64_t justdiv(int64_t n, int64_t d) {
  assert(d != 0);
  assert(n % d == 0);
  return n / d;
}

} // namespace jikka

#endif // JIKKA_DIVMOD_HPP
