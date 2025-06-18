# Note per la Conversione COBOL -> Java

### What do i need to remember
- Prerequisiti: Java, Maven e git installati.
- Serve il ProLeap COBOL Parser, che è un progetto Maven.
- Il file `.cbl` da convertire va messo in `src/main/resources/cobol/`.
- Tutti i file generati finiranno nella cartella `output/`.

### Funzionalità

- **Parsing COBOL**: Analizza file COBOL e genera AST/ASG (Abstract Syntax Tree / Abstract Syntax Graph)
- **Export `.xml`**: Esporta la struttura AST/ASG in formato `.xml`
- **Visitor Pattern**: Attraversa l'AST per analisi personalizzate
- **Trasformazione `.xml`**: Converte ASG COBOL in ASG Java
- **Supporto formati**: FIXED, VARIABLE, TANDEM


### Clonazione e install ProLeap COBOL Parser

```bash
cd ~/temp
git clone https://github.com/uwol/proleap-cobol-parser.git
cd proleap-cobol-parser
mvn clean install
```

## Passo 1: Generare l'`.xml` dall'AST del COBOL
Questo è il primo step: legge il file `.cbl` e lo trasforma in un file `.xml` che rappresenta il suo AST/ASG (Abstract Syntax Tree/Graph).


Cosa fa:
1. Esegue la classe `CobolToJavaConverter.java`;
2. Questa, a sua volta, usa `ASTToXMLExporter.java` per scrivere il file.

- Input: `src/main/resources/cobol/File_COBOL.cbl`.
- Output: `output/ASG_COBOL.xml`.

Il file `.xml` generato contiene la rappresentazione dell'ASG del codice COBOL, con dettagli su:
- Struttura delle divisioni COBOL
- Variabili e tipi di dato
- Paragrafi e statement

### Per eseguirlo
Da terminale dentro a `cobol-to-java-converter`, lanciare comando Maven:
```bash
# Compila il progetto ed esegue il main di CobolToJavaConverter:
mvn compile exec:java -Dexec.mainClass="com.example.CobolToJavaConverter"
# Oppure prova:
mvn exec:java -Dexec.mainClass="com.example.CobolToJavaConverter"
```
Per personalizzare input e output:
```java
// Cambia il file COBOL di input
File cobolFile = new File("src/main/resources/cobol/employee.cbl");

// Cambia il percorso di output `.xml`
ASTToXMLExporter.exportToXML(program, "output/employee-ast.xml");
```

## Passo 2: Trasformare l'`.xml` COBOL in `.xml` Java
Ora che ho l'AST/ASG del COBOL in formato `.xml`, lo trasformo in un altro `.xml` che rappresenta la struttura di una classe Java (un Abstract Syntax Graph, o ASG).

Cosa fa: 
1. Esegue la classe CobolToJavaXMLTransformer.

- Input: `output/ASG_COBOL.xml`.
- Output: `output/ASG_Java.xml`.

Mappatura `.xml`:
- program-unit → class
- data-entry → field + getter/setter  
- paragraph → method
- statement → Java statement equivalente

### Per eseguirlo
Da terminale dentro a `cobol-to-java-converter`, lanciare comando Maven:
```bash
# Compila il progetto ed esegue il main di CobolToJavaXMLTransformer:
mvn compile exec:java -Dexec.mainClass="com.example.CobolToJavaXMLTransformer"
# Oppure prova:
mvn exec:java -Dexec.mainClass="com.example.CobolToJavaXMLTransformer"
```
A questo punto, ho ASG_Java.xml, che contiene la struttura della classe, dei campi, dei getter/setter e dei metodi che deriverebbero dal COBOL.

## Passo 3: Generare il Codice `.java`
Lo scriptino python `gemini_java_generator.py` legge il file `output/ASG_Java.xml` e genera il codice sorgente in un file `.java` vero e proprio.

Per farlo girare devo essere dentro allo spazio virtuale `venv` in cui ho installato `pip install google-generativeai`.
Per entrare nello spazio virtuale, da terminale lanciato in `cobol-to-java-converter`, eseguire:
```bash
source venv/bin/activate
```

Cosa fa:
1. Esegue il file `gemini_java_generator.py` che usa le API Gemini per generare il codice Java.

- Input: `output/ASG_Java.xml`.
- Output: `output/GeneratedClass.java`.

### Per eseguirlo
Da terminale dentro a `cobol-to-java-converter`, lanciare il comando:
```bash
python gemini_java_generator.py
```

# Note Extra
## AST Visitor
`ASTVisitorExample.java`: Questa è solo una classe di esempio. Non fa parte del flusso di conversione, ma serve per vedere come posso "visitare" l'AST del COBOL se volessi estrarre delle informazioni specifiche senza passare per l'`.xml`.
### seguire il visitor example
```bash
# Compila il progetto ed esegue il main di ASTVisitorExample:
mvn compile exec:java -Dexec.mainClass="com.example.ASTVisitorExample"
# Oppure prova:
mvn exec:java -Dexec.mainClass="com.example.ASTVisitorExample"
```

## Estensioni Future

- [ ] Generazione codice Java (sorgente) tramite chiamate API Gemini
- [ ] Supporto COPY statements
- [ ] Conversione tipi COBOL → Java?
- [ ] Miglioramento visitor pattern per estrazione dati specifici
- [ ]