function foo() {
  try {
    throw new Error("cheese")
    return 42
  }
  catch (e if e instanceof Error) {
    print("caught:", e)
    return 69
  }
  catch (e) {
    print("something else")
  }
  finally {
    print('cleanup')
  }
}

print("GOT:", foo())

