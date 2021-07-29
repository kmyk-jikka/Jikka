#ifndef JIKKA_MODULO_MATRIX_HPP
#define JIKKA_MODULO_MATRIX_HPP
/**
 * @file jikka/modulo_matrix.hpp
 * @author Kimiyuki Onaka
 * @copyright Apache License 2.0
 */
#include "jikka/divmod.hpp"
#include "jikka/matrix.hpp"
#include <array>

namespace jikka {

namespace modmat {

using jikka::floormod;

template <size_t N>
std::array<int64_t, N> floormod(std::array<int64_t, N> x, int64_t MOD) {
  for (size_t i = 0; i < N; ++i) {
    x[i] = floormod(x[i], MOD);
  }
  return x;
}

template <size_t H, size_t W>
matrix<int64_t, H, W> floormod(matrix<int64_t, H, W> a, int64_t MOD) {
  for (size_t y = 0; y < H; ++y) {
    for (size_t x = 0; x < W; ++x) {
      a[y][x] = floormod(a[y][x], MOD);
    }
  }
  return a;
}

template <size_t H, size_t W>
std::array<int64_t, H> ap(const matrix<int64_t, H, W> &a,
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
matrix<int64_t, H, W> add(const matrix<int64_t, H, W> &a,
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
matrix<int64_t, H, W> mul(const matrix<int64_t, H, N> &a,
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
matrix<int64_t, N, N> pow(matrix<int64_t, N, N> x, int64_t k, int64_t MOD) {
  matrix<int64_t, N, N> y = mat::one<N>();
  for (; k; k >>= 1) {
    if (k & 1) {
      y = mul(y, x, MOD);
    }
    x = mul(x, x, MOD);
  }
  return y;
}

} // namespace modmat

} // namespace jikka

#endif // JIKKA_MODULO_MATRIX_HPP
