type ReactFrame = {
  fileName: string | null,
  lineNumber: number | null,
  name: string | null,
};

type ErrorCallback = (error: Error) => void;

export type ErrorLocation = {|
  fileName: string,
  lineNumber: number,
  colNumber?: number,
|};
