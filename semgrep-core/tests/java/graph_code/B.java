package pack1;
import pack1.Danger;
import pack1.DangerFactory;    

class B {
    int test() {
	Danger d;

	int x1 = d.get();
	int x2 = DangerFactory.makeDanger().get();
    }
}
