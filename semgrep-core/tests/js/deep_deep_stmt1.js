
function test1() {

    //ERROR: match
    var x = 1;

    function foo() {
        console.log(x);
    }
}

function test2() {

    //ERROR: match
    var x = 1;

    foo(() => {
        console.log(x);
    })
}

function test3() {

    //ERROR: match
    var x = 1;

    foo(function() {
        bar(() => {
            console.log(x);
        })
    })
}

function test4() {

    //ERROR: match
    var x = 1;

    class Hello {
        
	    test() {
		    console.log(x);
	    }
        
    }
}

function test5() {

    //ERROR: match
    var x = 1;

    function OldSchool () {
        
        this.test = function() {
            console.log(x);
        }
        
    }
}


function test6() {

    var x = 1;

    function foo(x) {
        // not the same x!
        console.log(x);
    }
}

