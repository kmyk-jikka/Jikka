#ifndef JIKKA_MODULO_HPP
#define JIKKA_MODULO_HPP
/**
 * @file jikka/modulo.hpp
 * @author Kimiyuki Onaka
 * @copyright Apache License 2.0
 */
#include "jikka/divmod.hpp"
#include <algorithm>
#include <cassert>
#include <cstdint>
#include <unordered_map>

namespace jikka {

namespace mod {

inline int64_t negate(int64_t a, int64_t MOD) { return floormod(-a, MOD); }

inline int64_t plus(int64_t a, int64_t b, int64_t MOD) {
  return floormod(a + b, MOD);
}

inline int64_t minus(int64_t a, int64_t b, int64_t MOD) {
  return floormod(a - b, MOD);
}

inline int64_t mult(int64_t a, int64_t b, int64_t MOD) {
  return floormod(a * b, MOD);
}

inline int64_t inv(int64_t value, int64_t MOD) {
  assert(0 < value and value < MOD);
  int64_t a = value, b = MOD;
  int64_t x = 0, y = 1;
  for (int64_t u = 1, v = 0; a;) {
    int64_t q = b / a;
    x -= q * u;
    std::swap(x, u);
    y -= q * v;
    std::swap(y, v);
    b -= q * a;
    std::swap(b, a);
  }
  assert(value * x + MOD * y == b and b == 1);
  if (x < 0) {
    x += MOD;
  }
  assert(0 <= x and x < MOD);
  return x;
}

inline int64_t pow(int64_t x, int64_t k, int64_t MOD) {
  assert(k >= 0);
  int64_t y = 1;
  for (; k > 0; k >>= 1) {
    if (k & 1) {
      y = y * x % MOD;
    }
    x = x * x % MOD;
  }
  if (y < 0) {
    y += MOD;
  }
  return y;
}

inline int64_t fact(int64_t n, int64_t MOD) {
  assert(0 <= n);
  assert(1 <= MOD);
  static std::unordered_map<int64_t, std::vector<int64_t>> memos;
  auto &memo = memos[MOD];
  while (static_cast<int64_t>(memo.size()) <= n) {
    if (memo.empty()) {
      memo.push_back(1);
    }
    memo.push_back(memo.size() * memo.back() % MOD);
  }
  return memo[n];
}

inline int64_t choose(int64_t n, int64_t r, int64_t MOD) {
  assert(0 <= r and r <= n);
  assert(1 <= MOD);
  return fact(n, MOD) * inv(fact(r, MOD), MOD) % MOD;
}

inline int64_t permute(int64_t n, int64_t r, int64_t MOD) {
  assert(0 <= r and r <= n);
  assert(1 <= MOD);
  return fact(n, MOD) * inv(fact(n - r, MOD) * fact(r, MOD) % MOD, MOD) % MOD;
}

inline int64_t multichoose(int64_t n, int64_t r, int64_t MOD) {
  assert(0 <= r and r <= n);
  assert(1 <= MOD);
  if (n == 0 and r == 0) {
    return 1;
  }
  return choose(n + r - 1, r, MOD);
}

} // namespace mod

} // namespace jikka

#endif // JIKKA_MODULO_HPP
