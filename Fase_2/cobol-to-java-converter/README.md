# COBOL to Java Converter

A tool to convert COBOL source code to equivalent Java code using the Koopa COBOL parser.

## Project Structure

```
cobol-to-java-converter/
├── src/
│   └── main/
│       └── java/
│           └── com/
│               └── DigArch/
│                   └── cobol2java/
│                       ├── model/
│                       ├── parser/
│                       ├── transformer/
│                       └── generator/
├── build/
├── koopa/
├── libs/
└── build.gradle
```

## Prerequisites

- Java 21 or higher
- Gradle 8.5 or higher
- Ant (for building Koopa)

## Building the Project

1. Build Koopa parser:
```bash
cd koopa
ant clean
ant build
ant jar
cp koopa.jar ../libs/
cd ..
```

2. Build the converter:
```bash
./gradlew clean build
```

## Usage

Run the converter with:
```bash
./gradlew run --args="input.cob output.java"
```

Or using the JAR directly:
```bash
java -jar build/libs/cobol-to-java-converter-1.0-SNAPSHOT.jar input.cob output.java
```

## Project Components

### Parser
Uses Koopa library to parse COBOL source code into an Abstract Syntax Tree (AST).

### Transformer
Converts COBOL AST into Java model objects.

### Generator
Generates Java source code from the transformed model.

## Dependencies

- Koopa COBOL Parser
- Commons IO
- SLF4J
- Logback Classic

## Contributing

1. Fork the repository
2. Create your feature branch
3. Commit your changes
4. Push to the branch
5. Create a new Pull Request

## License

[Your chosen license]