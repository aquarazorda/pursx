export const throwSyntaxError = (message) => (line) => (column) => {
  const error = new SyntaxError(`${line}:${column}: ${message}`);
  error.line = line;
  error.column = column;
  error.reason = message;
  throw error;
};

export const throwTypeError = (message) => {
  throw new TypeError(message);
};

export const orEmpty = (options) => (options == null ? {} : options);
