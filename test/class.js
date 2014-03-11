class Point {
  constructor(x = 0, y = 0) {
    this.move(x, y)
  }
  move(x, y) {
    this.x = x
    this.y = y
  }
}

class Point3D extends Point {
  constructor(x, y, z = 0) {
    super(x, y)
    this.move(x, y, z)
  }
  move(x, y, z) {
    super.move(x, y)
    this.z = z
  }
  get size() {
    return this._size || 3
  }
  set size(s) {
    this._size = s
  }
}

var p = new Point3D(1, 2, 3)
console.log(p.x, p.y, p.z)

console.log("size: ", p.size)

var o = {
  answer: 42,
  greet(whom) {
    console.log(`Hi ${whom} the answer is: ${this.answer}!`)
  }
}

o.greet("Felix")

for (var i = 0; i < 100_000_000; i += 1) {
  p.move(i, i + 1, i + 2)
}
print(p.x, p.y, p.z)

var rows = [['Unicorns', 'Sunbeams', 'Puppies'], ['<3', '<3', '<3']];
var out = `<table>${
  [for (row of rows)`<tr>${
    [for (cell of row) `<td>${cell}</td>`].join('')
  }</tr>`].join('')
}</table>`;

console.log(out)
