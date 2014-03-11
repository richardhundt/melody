var a = [ 'a', 'b', 'c' ]

a.map = function(f) {
  var a = [ ]
  for (var i = 0; i < this.length; i += 1) {
    a[a.length] = f(this[i])
  }
}

a.map(_ => console.log(_))
