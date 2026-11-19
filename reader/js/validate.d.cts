// What reader/js/validate.ml exports, compiled to JavaScript: the reader's
// Validate, over a bundle the reader writes.
declare const validate: {
  /**
   * The bundle's validator, from its text: what holds a value, given as JSON
   * text, to the component schema the bundle names `schema`. Each refusal is
   * the JSON Pointer into the value and why; a schema the bundle does not
   * name throws.
   */
  validator(bundle: string): (schema: string, value: string) => string[];
};
export = validate;
