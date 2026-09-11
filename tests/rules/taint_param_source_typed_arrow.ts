const sink = (value: string) => value;

import { Props as LocalProps } from "./types";

const TypedComponent = ({ html }: { html: string }) => {
  // ruleid:taint-param-source-typed-arrow
  return sink(html);
};

const UntypedComponent = ({ html }) => {
  // ruleid:taint-param-source-typed-arrow
  return sink(html);
};

// ruleid:typed-destructured-param-imported-type
const ImportedTypedComponent = ({ html }: LocalProps) => {
  return html;
};
