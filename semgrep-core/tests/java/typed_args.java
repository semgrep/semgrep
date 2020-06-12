class Point {
    int x;
    int y;
}

class Line {
    public static void main() {
       Point p;
       int x;
       int y;

       //ERROR: match
       getPoint(p);
       //ERROR: match
       getPoint(p, x);
       getPoint(x, y);
    }
}
