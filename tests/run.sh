#!/bin/bash
cd "$(dirname "$0")"/..

cpp=$(mktemp --suffix=.cpp)
exe=$(mktemp --suffix=.exe)
atexit() {
    [[ -n $cpp ]] && rm -f $cpp
    [[ -n $exe ]] && rm -f $exe
}
trap atexit EXIT

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

${CXX:-g++} -Wall -std=c++14 -O2 $cpp -o $exe || exit $?

[ $(echo 3 3 4 5 | $exe) -eq 5000950000 ]
[ $({ echo 100000 ; seq 100000 ; } | $exe) -eq 24999950000 ]
