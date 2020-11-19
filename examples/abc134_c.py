# https://atcoder.jp/contests/abc134/tasks/abc134_c

def solve(N: Interval["2", "200000"], A: Array[Interval["0", "200000"], "N"]) -> Array[int, "N"]:
    ans = [None for _ in range(N)]
    for i in range(N):
        ans[i] = max((0 if j = i else A[j]) for j in range(N))
    return ans
