#include <climits>
#include <iostream>
#include <vector>
using namespace std;

int64_t solve(int n, int k, const vector<int64_t> &h) {
  vector<int64_t> dp(n, INT64_MAX);
  dp[0] = 0;
  for (int i = 1; i < n; ++i) {
    for (int j = max(0, i - k); j < i; ++j) {
      dp[i] = min(dp[i], dp[j] + abs(h[i] - h[j]));
    }
  }
  return dp[n - 1];
}

int main() {
  int n, k;
  cin >> n >> k;
  vector<int64_t> h(n);
  for (int i = 0; i < n; ++i) {
    cin >> h[i];
  }
  int64_t ans = solve(n, k, h);
  cout << ans << endl;
  return 0;
}
