package pack1;

import pack1.A;
import pack1.B;

public class C {
    int c() {
	//MATCH:
	sink(user_input);

	//DEEP:
        sink(A.a());

        //DEEP: extra deep!
        sink(B.b());

	return 0;
    }
