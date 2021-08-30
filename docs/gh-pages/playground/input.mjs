import * as rts from "./rts.mjs";
import wasm from "./jikka-asterius.wasm.mjs";
import req from "./jikka-asterius.req.mjs";

async function convert(prog) {
  const m = await wasm;
  const i = await rts.newAsteriusInstance(Object.assign(req, { module: m }));
  return await i.exports.convert(prog);
}

// examples/dp_z-kubaru.py
const script = `\
# https://atcoder.jp/contests/dp/tasks/dp_z
from typing import *

INF = 10 ** 18

def solve(n: int, c: int, h: List[int]) -> int:
    assert 2 <= n <= 2 * 10 ** 5
    assert 1 <= c <= 10 ** 12
    assert len(h) == n
    assert all(1 <= h_i <= 10 ** 6 for h_i in h)

    dp = [INF for _ in range(n)]
    dp[0] = 0
    for i in range(n):
        for j in range(i + 1, n):
            dp[j] = min(dp[j], dp[i] + (h[i] - h[j]) ** 2 + c)
    return dp[n - 1]

def main() -> None:
    n, c = map(int, input().split())
    h = list(map(int, input().split()))
    assert len(h) == n
    ans = solve(n, c, h)
    print(ans)

if __name__ == '__main__':
    main()
`;

window.addEventListener("DOMContentLoaded", function () {
  require(["vs/editor/editor.main"], function () {
    var input = monaco.editor.create(document.getElementById("input"), {
      value: script,
      language: "python",
    });
    var output = monaco.editor.create(document.getElementById("output"), {
      value: "",
      language: "cpp",
    });

    let lastValue = "";
    console.log(lastValue);
    const sync = function () {
      try {
        if (input.getValue() == lastValue) {
          setTimeout(sync, 1000);
        } else {
          lastValue = input.getValue();
          output.setValue("transpiling...");
          convert(lastValue).then(function (value) {
            output.setValue(value);
            setTimeout(sync, 1000);
          });
        }
      } catch (e) {
        console.log(e);
        setTimeout(sync, 1000);
      }
    };
    sync();
  });
});
