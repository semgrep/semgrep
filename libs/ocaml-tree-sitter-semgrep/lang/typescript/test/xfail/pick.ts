/*
  Copied from
    https://github.com/w0rp/typescript-algorithms/blob/master/src/pick.ts
  Public Domain.
*/

interface PickFunction {
  (obj: object): {}
  <T extends object, K extends keyof T>(obj: T, ...keys: K[]): Pick<T, K>
}

/**
 * Select a subset of keys from an object.
 *
 * The return type is known at compile time to contain only the keys specified.
 */
export const pick: PickFunction =
  <T extends object, K extends keyof T>
  (obj: T, ...keys: K[]): Pick<T, K> => {
    const result: Partial<T> = {}

    for (const key of keys) {
      result[key] = obj[key]
    }

    return result as Pick<T, K>
  }
