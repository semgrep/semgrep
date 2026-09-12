/*
  Copied from
    https://github.com/w0rp/typescript-algorithms/blob/master/src/uniq.ts
  Public Domain.
*/

/**
 * Given an Array, return a new array with all of the repeated elements
 * removed. The elements are compared using the provided comparison function.
 */
export const uniq = <T>(array: T[], cmp: (a: T, b: T) => number): T[] => {
  const newArray = array.length > 0 ? [array[0]] : []

  for (let i = 1; i < array.length; ++i) {
    if (cmp(newArray[newArray.length - 1], array[i])) {
      newArray.push(array[i])
    }
  }

  return newArray
}
