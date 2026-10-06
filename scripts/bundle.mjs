// Bundles the CLI and the JavaScript API from spago's output into dist/.
import { build } from "esbuild";
import { fileURLToPath } from "node:url";

const root = fileURLToPath(new URL("..", import.meta.url));
const common = { bundle: true, platform: "node", format: "esm", absWorkingDir: root, logLevel: "warning" };

await build({
  ...common,
  stdin: { contents: 'import { main } from "./output/PursX.Cli/index.js";\nmain();\n', resolveDir: root },
  outfile: "dist/cli.js",
});

await build({ ...common, entryPoints: ["output/PursX.Api/index.js"], outfile: "dist/api.js" });
