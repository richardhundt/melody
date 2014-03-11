
class Foo {
  static create() {
    print(`create ${this}`)
  }
  greet() {
    print(`greet ${this}`)
  }
}

Foo.create()

