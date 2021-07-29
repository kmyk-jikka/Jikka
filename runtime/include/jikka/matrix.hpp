#ifndef JIKKA_MATRIX_HPP
#define JIKKA_MATRIX_HPP
/**
 * @file jikka/matrix.hpp
 * @author Kimiyuki Onaka
 * @copyright Apache License 2.0
 */
#include <array>

namespace jikka {

template <typename T, size_t H, size_t W>
using matrix = std::array<std::array<T, W>, H>;

namespace mat {

template <size_t H, size_t W>
std::array<int64_t, H> ap(const matrix<int64_t, H, W> &a,
                          const std::array<int64_t, W> &b) {
  std::array<int64_t, H> c = {};
  for (size_t y = 0; y < H; ++y) {
    for (size_t x = 0; x < W; ++x) {
      c[y] += a[y][x] * b[x];
    }
  }
  return c;
}

template <size_t N> matrix<int64_t, N, N> zero() { return {}; }

template <size_t N> matrix<int64_t, N, N> one() {
  matrix<int64_t, N, N> a = {};
  for (size_t i = 0; i < N; ++i) {
    a[i][i] = 1;
  }
  return a;
}

template <size_t H, size_t W>
matrix<int64_t, H, W> add(const matrix<int64_t, H, W> &a,
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
matrix<int64_t, H, W> mul(const matrix<int64_t, H, N> &a,
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
matrix<int64_t, N, N> pow(matrix<int64_t, N, N> x, int64_t k) {
  matrix<int64_t, N, N> y = one<N>();
  for (; k; k >>= 1) {
    if (k & 1) {
      y = mul(y, x);
    }
    x = mul(x, x);
  }
  return y;
}

} // namespace mat

} // namespace jikka

#endif // JIKKA_MATRIX_HPP
