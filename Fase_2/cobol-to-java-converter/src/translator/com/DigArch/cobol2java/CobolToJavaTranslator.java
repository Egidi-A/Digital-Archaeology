package com.DigArch.cobol2java;

import koopa.cobol.parser.CobolParser;
import koopa.cobol.parser.ParseResults;
import koopa.core.trees.Tree;
import com.DigArch.cobol2java.ast.visitor.CobolTreeVisitor;
import com.DigArch.cobol2java.model.JavaClass;
import com.DigArch.cobol2java.output.JavaCodeGenerator;
import koopa.core.trees.Tree;

import java.io.File;
import java.io.IOException;

/**
 * Classe principale per la traduzione di programmi COBOL in Java.
 * Coordina il processo di traduzione attraverso tre fasi principali:
 * 1. Parsing del file COBOL
 * 2. Visita dell'AST e costruzione del modello Java
 * 3. Generazione del codice Java
 */
public class CobolToJavaTranslator {
    
    /** Parser per l'analisi del codice COBOL */
    private final CobolParser parser;
    
    /** Visitor per la costruzione del modello Java dall'AST COBOL */
    private final CobolTreeVisitor visitor;
    
    /** Generatore per la produzione del codice Java finale */
    private final JavaCodeGenerator generator;
    
    /**
     * Costruisce un nuovo traduttore inizializzando i componenti necessari
     * per il parsing, la visita e la generazione del codice.
     */
    public CobolToJavaTranslator() {
        this.parser = new CobolParser();
        this.visitor = new CobolTreeVisitor();
        this.generator = new JavaCodeGenerator();
    }
    
    /**
     * Traduce un file COBOL in codice Java.
     * Il processo si articola in tre fasi:
     * 1. Parse del file COBOL per ottenere l'AST
     * 2. Visita dell'AST per costruire il modello Java
     * 3. Generazione del codice Java dal modello
     *
     * @param cobolFile Il file COBOL da tradurre
     * @return Il codice Java generato come stringa
     * @throws IOException Se si verificano errori di I/O durante la lettura del file
     * @throws IllegalArgumentException Se il file COBOL non è valido
     */
    public String translate(File cobolFile) throws IOException {
        // Parse COBOL file
        ParseResults results = parser.parse(cobolFile);
        if (!results.isValidInput()) {
            throw new IllegalArgumentException("Invalid COBOL file: " + cobolFile.getName());
        }
        
        // Get AST
        Object treeObj = results.getTree();
        if (!(treeObj instanceof Tree)) {
            throw new IllegalStateException("Failed to parse COBOL file: AST not available");
        }
        Tree ast = (Tree) treeObj;
        
        // Visit AST and build Java model
        JavaClass javaClass = visitor.visit(ast);
        
        // Generate Java code
        return generator.generate(javaClass);
    }
    
    /**
     * Punto di ingresso principale del traduttore.
     * Accetta due argomenti da linea di comando:
     * 1. Il percorso del file COBOL di input
     * 2. Il percorso del file Java di output
     *
     * Esempio di utilizzo:
     * java CobolToJavaTranslator input.cob output.java
     *
     * @param args Array contenente i percorsi dei file di input e output
     */
    public static void main(String[] args) {
        if (args.length != 2) {
            System.err.println("Usage: CobolToJavaTranslator <input.cob> <output.java>");
            System.exit(1);
        }
        
        try {
            CobolToJavaTranslator translator = new CobolToJavaTranslator();
            String javaCode = translator.translate(new File(args[0]));
            
            // Write to output file
            java.nio.file.Files.write(
                java.nio.file.Paths.get(args[1]), 
                javaCode.getBytes()
            );
            
            System.out.println("Translation completed successfully!");
        } catch (Exception e) {
            System.err.println("Translation failed: " + e.getMessage());
            e.printStackTrace();
            System.exit(1);
        }
    }
}