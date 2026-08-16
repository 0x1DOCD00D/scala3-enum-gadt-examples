# Scala 3 Enum and GADT Examples

This project collects the runnable examples from the Scala 3 enum and GADT tutorial derived from the attached Java enum-pattern article.

The examples cover:

1. Enum values carrying validation functions.
2. Permission enums with Set and Map, plus a capability GADT.
3. Nested enums carrying functions, plus typed configuration keys.
4. Enum template methods, plus a typed SQL GADT.
5. Standard enum values combined with validated runtime values, plus a protocol-state GADT.
6. A typed expression-language GADT.
7. Existential packaging of differently typed GADT values.
8. GADTs used as proof objects for type equality.

## Requirements

- JDK 17 or newer
- sbt 1.11 or newer
- Scala 3.7.x

## Compile

```bash
sbt compile
```

## Run examples

Each file contains an `@main` entry point. Examples can be run with `sbt run` and selecting the desired main class, or directly from an IDE.

The source files are under:

```text
src/main/scala/examples/
```
