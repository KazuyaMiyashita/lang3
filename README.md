## lang3

Practice writing a programming language.

### Usage

This is a normal sbt project. You can compile code with `sbt compile`, run it with `sbt run`, and `sbt console` will start a Scala 3 REPL.

For more information on the sbt-dotty plugin, see the
[scala3-example-project](https://github.com/scala/scala3-example-project/blob/main/README.md).

### Build jar file

```
$ sbt assembly
```

### Run program

```
$ java -jar target/scala-3.1.3/lang3-assembly-0.1.0-SNAPSHOT.jar program.l3
```
