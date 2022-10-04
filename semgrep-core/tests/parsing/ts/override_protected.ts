class BaseClass {
    protected _doSomething():void {
        console.log('i did something');
    }
}

export class TestClass extends BaseClass {
    protected override _doSomething():void {
        console.log('i did something different');
    }
}
