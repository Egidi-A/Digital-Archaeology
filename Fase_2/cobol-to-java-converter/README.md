# COBOL to Java Converter

Progetto per il parsing e la conversione di codice COBOL in Java utilizzando ProLeap COBOL Parser.

## Prerequisiti

- Java 11+
- Maven 3.6+
- Git

## Installazione

### 1. Clona e installa ProLeap COBOL Parser

```bash
cd ~/temp
git clone https://github.com/uwol/proleap-cobol-parser.git
cd proleap-cobol-parser
mvn clean install
```

### 2. Clona questo progetto //da implementare

```bash
git clone <your-repo-url>
cd cobol-to-java-converter
```

### 3. Compila il progetto

```bash
mvn clean compile
```

## Struttura del Progetto

```
cobol-to-java-converter/
├── pom.xml
├── README.md
├── src/
│   └── main/
│       ├── java/
│       │   └── com/
│       │       └── example/
│       │           ├── CobolToJavaConverter.java    # Main class
│       │           ├── ASTToXMLExporter.java       # Esporta AST in XML
│       │           ├── ASTVisitorExample.java      # Visitor pattern
│       │           └── CobolToJavaXMLTransformer.java # Trasforma XML COBOL → Java
│       └── resources/
│           └── cobol/
│               ├── example.cbl                      # Hello World COBOL
│               └── employee.cbl                     # Employee manager COBOL
└── output/
    ├── EmployeeASGCobol.xml
    ├── EmployeeASGJava.xml
    ├── ExampleASGCobol.xml
    └── ExampleASGJava.xml
```

## Utilizzo

### Eseguire il converter principale

```bash
mvn exec:java -Dexec.mainClass="com.example.CobolToJavaConverter"
```

### Eseguire il visitor example

```bash
mvn exec:java -Dexec.mainClass="com.example.ASTVisitorExample"
```

### Generare il file XML

Il file XML viene generato automaticamente quando esegui `CobolToJavaConverter`. Per personalizzare:

```java
// Cambia il file COBOL di input
File cobolFile = new File("src/main/resources/cobol/employee.cbl");

// Cambia il percorso di output XML
ASTToXMLExporter.exportToXML(program, "output/employee-ast.xml");
```

### Trasformare XML COBOL in XML Java

```bash
mvn exec:java -Dexec.mainClass="com.example.CobolToJavaXMLTransformer"
```

Questo comando:
1. Legge `output/ast.xml` (ASG COBOL)
2. Trasforma la struttura in equivalente Java
3. Genera `output/java-ast.xml` (ASG Java)

Mappatura XML:
- program-unit → class
- data-entry → field + getter/setter  
- paragraph → method
- statement → Java statement equivalente

## Funzionalità

- **Parsing COBOL**: Analizza file COBOL e genera AST/ASG (Abstract Syntax Tree / Abstract Syntax Graph)
- **Export XML**: Esporta la struttura AST in formato XML
- **Visitor Pattern**: Attraversa l'AST per analisi personalizzate
- **Trasformazione XML**: Converte ASG COBOL in ASG Java
- **Supporto formati**: FIXED, VARIABLE, TANDEM

## Esempi COBOL

### example.cbl
```cobol
       IDENTIFICATION DIVISION.
       PROGRAM-ID. HELLO-WORLD.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 WS-MESSAGE PIC X(20) VALUE "Hello COBOL World!".
       PROCEDURE DIVISION.
           DISPLAY WS-MESSAGE.
           STOP RUN.
```

### employee.cbl
Esempio più complesso con strutture dati gerarchiche e paragrafi multipli.

## Output XML COBOL
Il file XML generato contiene la rappresentazione dell'AST del codice COBOL, con dettagli su:
- Struttura delle divisioni COBOL
- Variabili e tipi di dato
- Paragrafi e statement
Il converter genera un file XML in `output/ast.xml`.


## Estensioni Future

- [ ] Generazione codice Java (sorgente) tramite chiamate API Gemini
- [ ] Supporto COPY statements
- [ ] Conversione tipi COBOL → Java?

## License
