#include <atcoder/modint>
#include <atcoder/segtree>
#include <iostream>
#include <vector>
#define REP(i, n) for (int i = 0; (i) < (int)(n); ++(i))
#define REP3(i, m, n) for (int i = (m); (i) < (int)(n); ++(i))
#define REP_R(i, n) for (int i = (int)(n)-1; (i) >= 0; --(i))
#define REP3R(i, m, n) for (int i = (int)(n)-1; (i) >= (int)(m); --(i))
#define ALL(x) ::std::begin(x), ::std::end(x)
using namespace std;
using namespace atcoder;

using mint = modint998244353;
mint plus_op(mint a, mint b) { return a + b; }
mint plus_e() { return 0; }

mint solve(int n, const std::vector<int64_t> &x,
           const std::vector<int64_t> &y) {
  mint ans = 0;

  // \sum \sum (x_i - x_j)^2 + (y_i - y_j)^2
  mint sum_x = 0;
  mint sum_y = 0;
  REP(i, n) {
    ans += mint(x[i]) * mint(x[i]) * (n - 1);
    ans += mint(y[i]) * mint(y[i]) * (n - 1);
    ans -= 2 * sum_x * mint(x[i]);
    ans -= 2 * sum_y * mint(y[i]);
    sum_x += x[i];
    sum_y += y[i];
  }

  // 2 \sum \sum |x_i - x_j| |y_i - y_j|
  vector<int> order_x(n);
  iota(ALL(order_x), 0);
  sort(ALL(order_x), [&](int i, int j) { return x[i] > x[j]; });
  vector<int64_t> compress_y = y;
  sort(ALL(compress_y));
  compress_y.erase(unique(ALL(compress_y)), compress_y.end());
  const int H = compress_y.size();
  segtree<mint, plus_op, plus_e> segtree_sum(H);
  segtree<mint, plus_op, plus_e> segtree_cnt(H);
  for (int j : order_x) {
    // - 2 \sum \sum x_j |y_i - y_j|
    int k = lower_bound(ALL(compress_y), y[j]) - compress_y.begin();
    ans -= 2 * x[j] *
           (segtree_sum.prod(k + 1, H) - segtree_cnt.prod(k + 1, H) * y[j]);
    ans -= 2 * x[j] * (segtree_cnt.prod(0, k) * y[j] - segtree_sum.prod(0, k));
    segtree_sum.set(k, segtree_sum.get(k) + y[j]);
    segtree_cnt.set(k, segtree_cnt.get(k) + 1);
  }
  for (int i : order_x) {
    // 2 \sum \sum x_i |y_i - y_j|
    int k = lower_bound(ALL(compress_y), y[i]) - compress_y.begin();
    segtree_sum.set(k, segtree_sum.get(k) - y[i]);
    segtree_cnt.set(k, segtree_cnt.get(k) - 1);
    ans += 2 * x[i] *
           (segtree_sum.prod(k + 1, H) - segtree_cnt.prod(k + 1, H) * y[i]);
    ans += 2 * x[i] * (segtree_cnt.prod(0, k) * y[i] - segtree_sum.prod(0, k));
  }

  return ans;
}

// generated by oj-template v4.8.0
// (https://github.com/online-judge-tools/template-generator)
int main() {
  std::ios::sync_with_stdio(false);
  std::cin.tie(nullptr);
  int N;
  std::cin >> N;
  std::vector<int64_t> x(N), y(N);
  REP(i, N) { std::cin >> x[i] >> y[i]; }
  auto ans = solve(N, x, y);
  std::cout << ans.val() << '\n';
  return 0;
}
