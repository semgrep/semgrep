interface ArrayConstructor {

    new(arrayLength?: number): any[];
    new <T>(arrayLength: number): T[];
    new <T>(...items: T[]): T[];
}
