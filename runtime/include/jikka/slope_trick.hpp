#ifndef JIKKA_SLOPE_TRICK
#define JIKKA_SLOPE_TRICK
/**
 * @brief Slope-Trick
 * @author ei1333 (original author)
 * @author Kimiyuki Onaka (maintainer)
 * @copyright The Unlicense
 * @see https://maspypy.com/slope-trick-1-%E8%A7%A3%E8%AA%AC%E7%B7%A8
 */
#include <algorithm>
#include <functional>
#include <limits>
#include <queue>
#include <vector>

namespace jikka {

template <typename T> struct slope_trick {

  const T INF = std::numeric_limits<T>::max() / 3;

  T min_f;
  std::priority_queue<T, std::vector<T>, std::less<>> L;
  std::priority_queue<T, std::vector<T>, std::greater<>> R;
  T add_l, add_r;

private:
  void push_R(const T &a) { R.push(a - add_r); }

  T top_R() const {
    if (R.empty()) {
      return INF;
    } else {
      return R.top() + add_r;
    }
  }

  T pop_R() {
    T val = top_R();
    if (not R.empty()) {
      R.pop();
    }
    return val;
  }

  void push_L(const T &a) { L.push(a - add_l); }

  T top_L() const {
    if (L.empty()) {
      return -INF;
    } else {
      return L.top() + add_l;
    }
  }

  T pop_L() {
    T val = top_L();
    if (not L.empty()) {
      L.pop();
    }
    return val;
  }

  size_t size() { return L.size() + R.size(); }

public:
  slope_trick() : min_f(0), add_l(0), add_r(0) {}

  struct query_t {
    T lx, rx, min_f;
  };

  // return min f(x)
  query_t query() const { return (query_t){top_L(), top_R(), min_f}; }

  // f(x) += a
  void add_all(const T &a) { min_f += a; }

  // add \_
  // f(x) += max(a - x, 0)
  void add_a_minus_x(const T &a) {
    min_f += std::max(T(0), a - top_R());
    push_R(a);
    push_L(pop_R());
  }

  // add _/
  // f(x) += max(x - a, 0)
  void add_x_minus_a(const T &a) {
    min_f += std::max(T(0), top_L() - a);
    push_L(a);
    push_R(pop_L());
  }

  // add \/
  // f(x) += abs(x - a)
  void add_abs(const T &a) {
    add_a_minus_x(a);
    add_x_minus_a(a);
  }

  // \/ -> \_
  // f_{new} (x) = min f(y) (y <= x)
  void clear_right() {
    while (not R.empty()) {
      R.pop();
    }
  }

  // \/ -> _/
  // f_{new} (x) = min f(y) (y >= x)
  void clear_left() {
    while (not L.empty()) {
      L.pop();
    }
  }

  // \/ -> \_/
  // f_{new} (x) = min f(y) (x-b <= y <= x-a)
  void shift(const T &a, const T &b) {
    assert(a <= b);
    add_l += a;
    add_r += b;
  }

  // \/. -> .\/
  // f_{new} (x) = f(x - a)
  void shift(const T &a) { shift(a, a); }

  // L, R を破壊する
  T get(const T &x) {
    T ret = min_f;
    while (not L.empty()) {
      ret += std::max(T(0), pop_L() - x);
    }
    while (not R.empty()) {
      ret += std::max(T(0), x - pop_R());
    }
    return ret;
  }

  void merge(slope_trick &st) {
    if (st.size() > size()) {
      std::swap(st.L, L);
      std::swap(st.R, R);
      std::swap(st.add_l, add_l);
      std::swap(st.add_r, add_r);
      std::swap(st.min_f, min_f);
    }
    while (not st.R.empty()) {
      add_x_minus_a(st.pop_R());
    }
    while (not st.L.empty()) {
      add_a_minus_x(st.pop_L());
    }
    min_f += st.min_f;
  }
};

} // namespace jikka

#endif // JIKKA_SLOPE_TRICK
