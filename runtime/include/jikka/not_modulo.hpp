#ifndef JIKKA_NOT_MODULO_HPP
#define JIKKA_NOT_MODULO_HPP
/**
 * @file jikka/not_modulo.hpp
 * @author Kimiyuki Onaka
 * @copyright Apache License 2.0
 */
#include <cassert>
#include <cstdint>

namespace jikka {

namespace notmod {

inline int64_t pow(int64_t x, int64_t k) {
  assert(k >= 0);
  int64_t y = 1;
  for (; k > 0; k >>= 1) {
    if (k & 1) {
      y *= x;
    }
    x *= x;
  }
  return y;
}

inline int64_t fact(int64_t n) {
  assert(0 <= n);
  int64_t ans = 1;
  for (int i = 0; i < n; ++i) {
    ans *= i + 1;
  }
  return ans;
}

inline int64_t choose(int64_t n, int64_t r) {
  assert(0 <= r and r <= n);
  int64_t ans = 1;
  for (int i = 0; i < r; ++i) {
    ans *= n - i;
    ans /= i + 1;
  }
  return ans;
}

inline int64_t permute(int64_t n, int64_t r) {
  assert(0 <= r and r <= n);
  int64_t ans = 1;
  for (int i = 0; i < r; ++i) {
    ans *= n - i;
  }
  return ans;
}

inline int64_t multichoose(int64_t n, int64_t r) {
  assert(0 <= r and r <= n);
  if (n == 0 and r == 0) {
    return 1;
  }
  return choose(n + r - 1, r);
}

} // namespace notmod

} // namespace jikka

#endif // JIKKA_NOT_MODULO_HPP
