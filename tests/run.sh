#!/bin/bash
cd "$(dirname "$0")"/..

cpp=$(mktemp --suffix=.cpp)
exe=$(mktemp --suffix=.exe)
atexit() {
    [[ -n $cpp ]] && rm -f $cpp
    [[ -n $exe ]] && rm -f $exe
}
trap atexit EXIT


test_example() {
    : > $cpp
    cat <<'EOF' >> $cpp
#include <cstdint>
#include <iostream>
#include <vector>
using namespace std;

EOF
    dotnet run < tests/example.jikka >> $cpp || exit $?
    cat <<'EOF' >> $cpp

int main() {
    int64_t n; cin >> n;
    vector<int64_t> a(n);
    for (int64_t i = 0; i < n; ++ i) {
        cin >> a[i];
    }
    cout << solve(n, a) << endl;
    return 0;
}
EOF
    cat $cpp

    ${CXX:-g++} -Wall -std=c++14 -O2 $cpp -o $exe || exit $?

    diff <(echo 3 3 4 5 | $exe) <(echo 5000950000) || exit $?
    diff <({ echo 100000 ; seq 100000 ; } | $exe) <(echo 24999950000) || exit $?
}
test_example


test_abc134_c() {
    : > $cpp
    cat <<'EOF' >> $cpp
#include <cstdint>
#include <iostream>
#include <vector>
using namespace std;

EOF
    dotnet run < tests/abc134_c.jikka >> $cpp || exit $?
    cat <<'EOF' >> $cpp

int main() {
    int64_t n; cin >> n;
    vector<int64_t> a(n);
    for (int64_t i = 0; i < n; ++ i) {
        cin >> a[i];
    }
    auto b = solve(n, a);
    for (int64_t i = 0; i < n; ++ i) {
        cout << b[i] << endl;
    }
    return 0;
}
EOF
    cat $cpp

    ${CXX:-g++} -Wall -std=c++14 -O2 $cpp -o $exe || exit $?

    diff <(echo 3 1 4 3 | $exe) - <<'EOF' || exit $?
4
3
4
EOF
    diff <(echo 2 5 5 | $exe) - <<'EOF' || exit $?
5
5
EOF
    diff <({ echo 20 ; seq 20 ; } | $exe) <(yes 20 | head -n 20 | sed '20 c19') || exit $?
    diff <({ echo 20 ; seq 20 | tac ; } | $exe) <(yes 20 | head -n 20 | sed '1 c19') || exit $?
    diff <({ echo 20 ; seq 11 ; seq 9 ; } | $exe) <(yes 11 | head -n 20 | sed '11 c10') || exit $?
    diff <({ echo 200000 ; seq 200000 ; } | $exe) <(yes 200000 | head -n 200000 | sed '200000 c199999') || exit $?

}
test_abc134_c
