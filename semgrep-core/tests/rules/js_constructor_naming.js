class ClassName {
    foo () {
        // ruleid: js-constructor-naming
        return this.http.thing
    }

    constructor (http: Ty) { }
  
    bar () {
        // ruleid: js-constructor-naming
        return this.http
    }
  }
  