#include "atcoder/segtree"
#include <algorithm>
#include <climits>
#include <iostream>
#include <vector>
using namespace std;

int64_t op(int64_t a, int64_t b) { return max(a, b); }
int64_t e() { return INT64_MIN; }

int64_t solve(int64_t n, vector<int64_t> h, vector<int64_t> a) {
  vector<int64_t> dp(n, 0);
  atcoder::segtree<int64_t, op, e> segtree(dp);
  for (int32_t i = 0; i < n; ++i) {
    dp[h[i] - 1] = max<int64_t>(0, segtree.prod(0, h[i])) + a[i];
    segtree.set(h[i] - 1, dp[h[i] - 1]);
  }
  return *max_element(dp.begin(), dp.end());
}

int main() {
  int n;
  cin >> n;
  vector<int64_t> h(n);
  vector<int64_t> a(n);
  for (int i = 0; i < n; ++i) {
    cin >> h[i];
  }
  for (int i = 0; i < n; ++i) {
    cin >> a[i];
  }
  auto ans = solve(n, h, a);
  cout << ans << '\n';
  return 0;
}
