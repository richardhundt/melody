import {A,B} from './foobar.js'

module foo {
  export var a = 42

  export class Foo {
    greet(whom) {
      print("Hello "~whom~"!")
    }
  }
}

var f = new foo.Foo()
f.greet("Felix")

