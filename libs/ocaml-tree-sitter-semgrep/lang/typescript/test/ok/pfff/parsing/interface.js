
interface Array<T> {
    toLocaleString(): string;
    concat(...items: Array<T>): Array<T>;
    join(separator?: string): string;
    pop(): T;
    push(...items: Array<T>): number;
    reverse(): Array<T>;
    shift(): T;
    slice(start?: number, end?: number): Array<T>;
    sort(compareFn?: (a: T, b: T) => number): Array<T>;
    splice(start: number, deleteCount?: number, ...items: Array<T>): Array<T>;
    unshift(...items: Array<T>): number;
    indexOf(searchElement: T, fromIndex?: number): number;
    lastIndexOf(searchElement: T, fromIndex?: number): number;
    every(callbackfn: (value: T, index: number, array: Array<T>) => boolean, thisArg?: any): boolean;
    some(callbackfn: (value: T, index: number, array: Array<T>) => boolean, thisArg?: any): boolean;
    forEach(callbackfn: (value: T, index: number, array: Array<T>) => void, thisArg?: any): void;
    map<U>(callbackfn: (value: T, index: number, array: Array<T>) => U, thisArg?: any): Array<U>;
    filter(callbackfn: (value: T, index: number, array: Array<T>) => boolean, thisArg?: any): Array<T>;
    reduce<U>(callbackfn: (previousValue: U, currentValue: T, currentIndex: number, array: Array<T>) => U, initialValue: U): U;
    reduceRight<U>(callbackfn: (previousValue: U, currentValue: T, currentIndex: number, array: Array<T>) => U, initialValue: U): U;
    length: number;
}


interface String {
    charAt(pos: number): string;
    charCodeAt(index: number): number;
    concat(...strings: Array<string>): string;
    indexOf(searchString: string, position?: number): number;
    lastIndexOf(searchString: string, position?: number): number;
    localeCompare(that: string): number;
    //match(regexp: string): Array<string>;
    //match(regexp: RegExp): Array<string>;
    match(regexp: any): Array<string>; // need special syntax for union/intersection
    //replace(searchValue: string, replaceValue: string): string;
    //replace(searchValue: string, replaceValue: (substring: string, ...args: Array<any>) => string): string;
    //replace(searchValue: RegExp, replaceValue: string): string;
    //replace(searchValue: RegExp, replaceValue: (substring: string, ...args: Array<any>) => string): string;
    replace(searchValue: any, replaceValue: any): string; // need special syntax for union/intersection
    //search(regexp: string): number;
    //search(regexp: RegExp): number;
    search(regexp: any): number; // need special syntax for union/intersection
    slice(start?: number, end?: number): string;
    //split(separator: string, limit?: number): Array<string>;
    //split(separator: RegExp, limit?: number): Array<string>;
    split(separator: any, limit?: number): Array<string>; // need special syntax for union/intersection
    substring(start: number, end?: number): string;
    toLowerCase(): string;
    toLocaleLowerCase(): string;
    toUpperCase(): string;
    toLocaleUpperCase(): string;
    trim(): string;
    length: number;
    substr(from: number, length?: number): string;
}

interface RegExp {
    exec(string: string): RegExpExecArray;
    test(string: string): boolean;
    source: string;
    global: boolean;
    ignoreCase: boolean;
    multiline: boolean;
    lastIndex: number;
    compile(): RegExp;
}
