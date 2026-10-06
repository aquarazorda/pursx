/** What JSX desugars to; the same settings as `pursx.json`. */
export interface Options {
  /** Qualifier for lowercase tags: with `"H"`, `<div>` calls `H.div`. Default `""`. */
  elements?: string;
  /** `"argument"`: `div props children` (default). `"prop"`: `div { ..., children: [...] }`. */
  children?: "argument" | "prop";
  /** With `children: "argument"`, `"bare"` passes a lone child as is. Default `"array"`. */
  singleChild?: "bare" | "array";
  /** Wraps text that is one of several children. Default `"text"`. */
  text?: string;
  /** Applied to components: `"Component.element"` turns `<Counter />` into `Component.element counter {}`. Default `""`. */
  component?: string;
  /** Lowercase tags that take props only. Default: HTML's void elements. */
  voidElements?: string[];
}

/**
 * Converts `.purx` source to PureScript with the same lines.
 * Throws a `SyntaxError` with `line` and `column` for bad JSX, and a `TypeError` for bad options.
 */
export function transform(source: string, options?: Options): string;
