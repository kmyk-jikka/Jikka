function loadData() {
  const req = new XMLHttpRequest();
  req.open("GET", "data.json", false);
  req.send();
  if (req.status != 200) {
    throw Error(req.statusText);
  }
  return JSON.parse(req.responseText);
}

function appendHeader(div, level, text) {
  const h = document.createElement("h" + level.toString());
  h.textContent = text;
  div.appendChild(h);
}

function appendPreCode(div, text, lang) {
  const pre = document.createElement("pre");
  const code = document.createElement("code");
  code.classList.add("language-" + lang);
  code.textContent = text;
  pre.appendChild(code);
  div.appendChild(pre);
  return pre;
}

function appendCommon(gallery, example) {
  appendHeader(gallery, 3, example["path"]);

  // Use two panes
  const div = document.createElement("div");
  div.classList.add("row");
  const left = document.createElement("div");
  const right = document.createElement("div");
  left.classList.add("col-lg-6");
  right.classList.add("col-lg-6");
  div.appendChild(left);
  div.appendChild(right);
  gallery.appendChild(div);

  appendHeader(left, 4, "Input");
  appendHeader(right, 4, "Output");

  appendHeader(left, 5, "Python");
  appendPreCode(left, example["python"], "python");
  return right;
}

function appendExample(gallery, example) {
  const right = appendCommon(gallery, example);
  // appendPreCode(right, example["rpython"], "python");
  appendHeader(right, 5, "core");
  appendPreCode(right, example["core"], "haskell");
  appendHeader(right, 5, "C++");
  appendPreCode(right, example["cxx"], "cxx");
}

function appendError(gallery, example) {
  const right = appendCommon(gallery, example);
  appendPreCode(right, example["error"], "plain");
}

window.addEventListener("DOMContentLoaded", function () {
  const gallery = document.getElementById("gallery");
  let data;
  try {
    data = loadData();
  } catch (error) {
    console.error(error);
    gallery.textContent = "error: " + error;
    return;
  }

  while (gallery.lastChild) {
    gallery.removeChild(gallery.lastChild);
  }
  for (const example of data["examples"]) {
    appendExample(gallery, example);
  }
  for (const example of data["errors"]) {
    appendError(gallery, example);
  }
  document.querySelectorAll("pre code").forEach((el) => {
    hljs.highlightElement(el);
  });
});
