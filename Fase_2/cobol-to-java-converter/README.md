# COBOL to Java Converter

Converts COBOL programs to Java classes. Currently supports IDENTIFICATION DIVISION parsing.

## Prerequisites

- Java 11+
- Maven 3.6+
- Docker (optional)

## Quick Start

### Local Build & Run

```bash
# Clone and navigate to project
cd cobol-to-java-converter

# Build the project
mvn clean package

# Run converter - SPECIFY THE PATH TO YOUR COBOL FILE
java -jar target/cobol-to-java-converter-1.0.0.jar src/test/resources/sample-cobol/hello.cob
# Output will be created as myprogram.java in the same directory
```

## Project Structure

```
cobol-to-java-converter/
├── src/main/java/com/cobolconverter/
│   ├── CobolToJavaConverter.java         (REPLACE with my "Updated CobolToJavaConverter.java")
│   ├── model/
│   │   ├── CobolProgram.java            (NEW - use my "CobolProgram.java - Model Class")
│   │   ├── IdentificationDivision.java  (NEW - use my "IdentificationDivision.java - Model Class")
│   │   └── EnvironmentDivision.java     (NEW - use my "EnvironmentDivision.java - Model Class")
│   ├── parser/
│   │   ├── IdentificationDivisionParser.java  (NEW - use my "IdentificationDivisionParser.java")
│   │   └── EnvironmentDivisionParser.java     (NEW - use my "EnvironmentDivisionParser.java")
│   └── generator/
│       └── JavaCodeGenerator.java       (NEW - use my "JavaCodeGenerator.java")```

## Usage

```bash
java -jar target/cobol-to-java-converter-1.0.0.jar <input-cobol-file>
```

The converter will:
- Parse the IDENTIFICATION DIVISION
- Generate a Java class with the same name (converted to CamelCase)
- Include COBOL metadata as JavaDoc comments
- Create the .java file in the same directory as the input

## Example

Input: `CUSTOMER-REPORT.cob`
Output: `CustomerReport.java`

## Development

To extend the converter:
1. Add parsers for other divisions in `src/main/java/com/cobolconverter/parser/`
2. Update the model classes in `src/main/java/com/cobolconverter/model/`
3. Enhance the Java code generator in `src/main/java/com/cobolconverter/generator/`

## Testing

```bash
mvn test
```