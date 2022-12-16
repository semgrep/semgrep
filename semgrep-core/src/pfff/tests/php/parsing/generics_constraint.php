<?hh

interface IAble {
}

class TestConstraint<T as IAble> {
}

class TestConstraint2<T as ?IAble> {
}
