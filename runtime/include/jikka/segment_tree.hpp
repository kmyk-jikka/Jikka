#ifndef JIKKA_SEGMENT_TREE_HPP
#define JIKKA_SEGMENT_TREE_HPP
/**
 * @file jikka/base.hpp
 * @brief utilities for segment trees of AtCoder Library
 * @author Kimiyuki Onaka
 * @copyright Apache License 2.0
 */
#include <climits>
#include <functional>

namespace jikka {

inline int64_t plus_int64_t(int64_t a, int64_t b) { return a + b; }

inline int64_t min_int64_t(int64_t a, int64_t b) { return std::min(a, b); }

inline int64_t max_int64_t(int64_t a, int64_t b) { return std::max(a, b); }

inline int64_t const_zero() { return 0; }

inline int64_t const_int64_min() { return INT64_MIN; }

inline int64_t const_int64_max() { return INT64_MAX; }

} // namespace jikka

#endif // JIKKA_SEGMENT_TREE_HPP
