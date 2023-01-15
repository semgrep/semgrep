function foo(): number {
    // ERROR:
    var config = {
        key: value,
        key2: value2,
        key3: value3,
    };

    var invoke = function(obj: {key: number, key2: string}) {
        return 2;
    };
}
