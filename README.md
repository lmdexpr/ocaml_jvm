# ocaml-jvm

## What
- JVM implementation using OCaml
- Specification : https://docs.oracle.com/javase/specs/jvms/se17/html/index.html

## Now Features
- run HelloWorld.class
```
 ❯ dune exec ocaml_jvm java_src/HelloWorld.class
 Hello,World.
```
- run FizzBuzz.class
```
 ❯ dune exec ocaml_jvm java_src/FizzBuzz.class
 1
 2
 Fizz
 4
 Buzz
 Fizz
 ...
 98
 Fizz
 Buzz
```
