int64_t solve(int64_t N, const vector<int64_t> & A) {
    int64_t K = 100000;
    int64_t a2 = 0;
    for (int64_t i2 = 0; i2 < K; ++ i2) {
        a2 += i2;
    }
    int64_t a1 = INT64_MIN;
    for (int64_t i1 = 0; i1 < N; ++ i1) {
        a1 = max(a1, 2 * A[i1]);
    }
    return a2 + K * a1;
}
