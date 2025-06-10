package com.DigArch.cobol2java.ast.builders;

import com.github.javaparser.ast.CompilationUnit;
import com.github.javaparser.ast.body.ClassOrInterfaceDeclaration;
import com.github.javaparser.ast.body.MethodDeclaration;
import com.github.javaparser.ast.stmt.BlockStmt;
import com.github.javaparser.ast.stmt.Statement;
import com.DigArch.cobol2java.model.JavaClass;
import com.DigArch.cobol2java.model.JavaMethod;
import com.DigArch.cobol2java.ast.mapping.StatementMapper;

import java.util.List;

/**
 * Builder per la costruzione dell'Abstract Syntax Tree (AST) Java.
 * Questa classe è responsabile della generazione della struttura del codice sorgente Java
 * utilizzando la libreria JavaParser, partendo da modelli intermedi.
 */
public class JavaClassBuilder {
    
    private final JavaASTBuilder astBuilder;
    private final StatementMapper statementMapper;
    
    public JavaClassBuilder() {
        this.astBuilder = new JavaASTBuilder();
        this.statementMapper = new StatementMapper();
    }
    
    /**
     * Costruisce una CompilationUnit Java completa partendo da un modello JavaClass.
     * 
     * @param javaClass Il modello della classe Java contenente campi e metodi
     * @return CompilationUnit che rappresenta la struttura completa del file sorgente Java
     */
    public CompilationUnit build(JavaClass javaClass) {
        CompilationUnit cu = astBuilder.buildCompilationUnit(javaClass);
        
        ClassOrInterfaceDeclaration classDecl = cu.getClassByName(javaClass.getClassName())
            .orElseThrow(() -> new RuntimeException("Class not found"));
        
        // Add method bodies
        for (JavaMethod method : javaClass.getMethods()) {
            addMethodBody(classDecl, method);
        }
        
        // Add main method if needed
        if (javaClass.needsMainMethod()) {
            addMainMethod(classDecl, javaClass.getMainMethodName());
        }
        
        return cu;
    }
    
    /**
     * Aggiunge il corpo di un metodo alla dichiarazione del metodo nella classe Java.
     * Il corpo del metodo viene popolato con le istruzioni mappate dal modello intermedio.
     *
     * @param classDecl La dichiarazione della classe contenente il metodo
     * @param method Il modello del metodo Java di cui aggiungere il corpo
     */
    private void addMethodBody(ClassOrInterfaceDeclaration classDecl, JavaMethod method) {
        MethodDeclaration methodDecl = classDecl.getMethodsByName(method.getName()).get(0);
        BlockStmt body = new BlockStmt();
        
        for (String statement : method.getStatements()) {
            List<Statement> javaStatements = statementMapper.mapStatement(statement);
            javaStatements.forEach(body::addStatement);
        }
        
        methodDecl.setBody(body);
    }
    
    /**
     * Aggiunge un metodo main alla classe Java, necessario per l'esecuzione del programma.
     * Il metodo main è statico e pubblico, e chiama il metodo principale dell'istanza della classe.
     *
     * @param classDecl La dichiarazione della classe a cui aggiungere il metodo main
     * @param mainMethodName Il nome del metodo principale da chiamare nel corpo del metodo main
     */
    private void addMainMethod(ClassOrInterfaceDeclaration classDecl, String mainMethodName) {
        MethodDeclaration main = classDecl.addMethod("main", com.github.javaparser.ast.Modifier.Keyword.PUBLIC, com.github.javaparser.ast.Modifier.Keyword.STATIC);
        main.setType(new com.github.javaparser.ast.type.VoidType());
        main.addParameter("String[]", "args");
        
        BlockStmt body = new BlockStmt();
        body.addStatement(String.format("%s instance = new %s();", 
            classDecl.getNameAsString(), classDecl.getNameAsString()));
        body.addStatement(String.format("instance.%s();", mainMethodName));
        
        main.setBody(body);
    }
}