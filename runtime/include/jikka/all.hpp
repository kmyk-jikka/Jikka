#ifndef JIKKA_ALL_HPP
#define JIKKA_ALL_HPP
#include <algorithm>
#include <array>
#include <cassert>
#include <cstdint>
#include <functional>
#include <iostream>
#include <numeric>
#include <unordered_map>
#include <vector>

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

template <typename T, size_t H, size_t W>
using matrix = std::array<std::array<T, W>, H>;

template <class T, class... Args>
std::array<T, sizeof...(Args)> make_array(Args... args) {
  return {args...};
}

template <size_t H, size_t W>
std::array<int64_t, H> matap(const matrix<int64_t, H, W> &a,
                             const std::array<int64_t, W> &b) {
  std::array<int64_t, H> c = {};
  for (size_t y = 0; y < H; ++y) {
    for (size_t x = 0; x < W; ++x) {
      c[y] += a[y][x] * b[x];
    }
  }
  return c;
}

template <size_t N> matrix<int64_t, N, N> matzero() { return {}; }

template <size_t N> matrix<int64_t, N, N> matone() {
  matrix<int64_t, N, N> a = {};
  for (size_t i = 0; i < N; ++i) {
    a[i][i] = 1;
  }
  return a;
}

template <size_t H, size_t W>
matrix<int64_t, H, W> matadd(const matrix<int64_t, H, W> &a,
                             const matrix<int64_t, H, W> &b) {
  matrix<int64_t, H, W> c;
  for (size_t y = 0; y < H; ++y) {
    for (size_t x = 0; x < W; ++x) {
      c[y][x] = a[y][x] + b[y][x];
    }
  }
  return c;
}

template <size_t H, size_t N, size_t W>
matrix<int64_t, H, W> matmul(const matrix<int64_t, H, N> &a,
                             const matrix<int64_t, N, W> &b) {
  matrix<int64_t, H, W> c = {};
  for (size_t y = 0; y < H; ++y) {
    for (size_t z = 0; z < N; ++z) {
      for (size_t x = 0; x < W; ++x) {
        c[y][x] += a[y][z] * b[z][x];
      }
    }
  }
  return c;
}

template <size_t N>
matrix<int64_t, N, N> matpow(matrix<int64_t, N, N> x, int64_t k) {
  matrix<int64_t, N, N> y = matone<N>();
  for (; k; k >>= 1) {
    if (k & 1) {
      y = matmul(y, x);
    }
    x = matmul(x, x);
  }
  return y;
}

inline int64_t modnegate(int64_t a, int64_t MOD) { return floormod(-a, MOD); }

inline int64_t modplus(int64_t a, int64_t b, int64_t MOD) {
  return floormod(a + b, MOD);
}

inline int64_t modminus(int64_t a, int64_t b, int64_t MOD) {
  return floormod(a - b, MOD);
}

inline int64_t modmult(int64_t a, int64_t b, int64_t MOD) {
  return floormod(a * b, MOD);
}

inline int64_t modinv(int64_t value, int64_t MOD) {
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

inline int64_t modpow(int64_t x, int64_t k, int64_t MOD) {
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

template <size_t H, size_t W>
std::array<int64_t, H> modmatap(const matrix<int64_t, H, W> &a,
                                const std::array<int64_t, W> &b, int64_t MOD) {
  std::array<int64_t, H> c = {};
  for (size_t y = 0; y < H; ++y) {
    for (size_t x = 0; x < W; ++x) {
      c[y] += a[y][x] * b[x] % MOD;
    }
    c[y] = floormod(c[y], MOD);
  }
  return c;
}

template <size_t H, size_t W>
matrix<int64_t, H, W> modmatadd(const matrix<int64_t, H, W> &a,
                                const matrix<int64_t, H, W> &b, int64_t MOD) {
  matrix<int64_t, H, W> c;
  for (size_t y = 0; y < H; ++y) {
    for (size_t x = 0; x < W; ++x) {
      c[y][x] = floormod(a[y][x] + b[y][x], MOD);
    }
  }
  return c;
}

template <size_t H, size_t N, size_t W>
matrix<int64_t, H, W> modmatmul(const matrix<int64_t, H, N> &a,
                                const matrix<int64_t, N, W> &b, int64_t MOD) {
  matrix<int64_t, H, W> c = {};
  for (size_t y = 0; y < H; ++y) {
    for (size_t z = 0; z < N; ++z) {
      for (size_t x = 0; x < W; ++x) {
        c[y][x] += a[y][z] * b[z][x] % MOD;
      }
    }
  }
  for (size_t y = 0; y < H; ++y) {
    for (size_t x = 0; x < W; ++x) {
      c[y][x] = floormod(c[y][x], MOD);
    }
  }
  return c;
}

template <size_t N>
matrix<int64_t, N, N> modmatpow(matrix<int64_t, N, N> x, int64_t k,
                                int64_t MOD) {
  matrix<int64_t, N, N> y = matone<N>();
  for (; k; k >>= 1) {
    if (k & 1) {
      y = modmatmul(y, x, MOD);
    }
    x = modmatmul(x, x, MOD);
  }
  return y;
}

inline std::vector<int64_t> range(int64_t n) {
  if (n < 0) {
    return std::vector<int64_t>();
  }
  std::vector<int64_t> xs(n);
  std::iota(xs.begin(), xs.end(), 0);
  return xs;
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

inline int64_t modfact(int64_t n, int64_t MOD) {
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

inline int64_t modchoose(int64_t n, int64_t r, int64_t MOD) {
  assert(0 <= r and r <= n);
  assert(1 <= MOD);
  return modfact(n, MOD) * modinv(modfact(r, MOD), MOD) % MOD;
}

inline int64_t modpermute(int64_t n, int64_t r, int64_t MOD) {
  assert(0 <= r and r <= n);
  assert(1 <= MOD);
  return modfact(n, MOD) *
         modinv(modfact(n - r, MOD) * modfact(r, MOD) % MOD, MOD) % MOD;
}

inline int64_t modmultichoose(int64_t n, int64_t r, int64_t MOD) {
  assert(0 <= r and r <= n);
  assert(1 <= MOD);
  if (n == 0 and r == 0) {
    return 1;
  }
  return modchoose(n + r - 1, r, MOD);
}

template <class T> inline T error(const std::string &message) {
  std::cerr << message << std::endl;
  assert(false);
}

} // namespace jikka

#endif // JIKKA_ALL_HPP
