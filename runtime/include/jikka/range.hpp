#ifndef JIKKA_RANGE_HPP
#define JIKKA_RANGE_HPP
/**
 * @file jikka/range.hpp
 * @author Kimiyuki Onaka
 * @copyright Apache License 2.0
 */
#include <cstdint>
#include <numeric>
#include <vector>

namespace jikka {

inline std::vector<int64_t> range(int64_t n) {
  if (n < 0) {
    return std::vector<int64_t>();
  }
  std::vector<int64_t> xs(n);
  std::iota(xs.begin(), xs.end(), 0);
  return xs;
}

} // namespace jikka

#endif // JIKKA_RANGE_HPP
