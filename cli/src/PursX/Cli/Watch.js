import fs from "node:fs";
import path from "node:path";

export const watchImpl = (paths, onChange) => {
  const pending = new Set();
  let timer = null;
  const flush = () => {
    timer = null;
    const files = [...pending];
    pending.clear();
    for (const file of files) onChange(file);
  };
  for (const p of paths) {
    const dir = fs.statSync(p).isDirectory() ? p : path.dirname(p);
    fs.watch(dir, { recursive: true }, (_event, name) => {
      if (!name) return;
      pending.add(path.join(dir, name.toString()));
      if (timer === null) timer = setTimeout(flush, 50);
    });
  }
};
