# NAME

Melody - ECMA Script Harmony inspired language on LuaJIT VM

# STATUS

This is an experimental project and there are a few things left to do. The biggest being destructuring assignment (it's parsing already, but not compiling).

Also, there are several semantics which are hangovers from ECMA Script 4 which Harmony retains which I may just ignore, such as the `arguments` object not being an array and so forth. The object meta-model is a bit different too.

In short, I'm just having a bit of fun with this while trying to prove that LuaJIT + LPeg + bytecode generation makes for a decent platform for implementing dynamic languages.
