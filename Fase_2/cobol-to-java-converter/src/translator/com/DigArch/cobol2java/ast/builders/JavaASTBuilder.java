package com.DigArch.cobol2java.ast.builders;

import com.github.javaparser.ast.CompilationUnit;
import com.github.javaparser.ast.Modifier;
import com.github.javaparser.ast.body.ClassOrInterfaceDeclaration;
import com.github.javaparser.ast.body.FieldDeclaration;
import com.github.javaparser.ast.body.MethodDeclaration;
import com.github.javaparser.ast.body.VariableDeclarator;
import com.github.javaparser.ast.type.Type;
import com.github.javaparser.ast.type.PrimitiveType;
import com.github.javaparser.ast.type.ClassOrInterfaceType;
import com.DigArch.cobol2java.model.JavaClass;
import com.DigArch.cobol2java.model.JavaField;
import com.DigArch.cobol2java.model.JavaMethod;

/**
 * Builder per la costruzione dell'Abstract Syntax Tree (AST) Java.
 * Questa classe è responsabile della generazione della struttura del codice sorgente Java
 * utilizzando la libreria JavaParser, partendo da modelli intermedi.
 */
public class JavaASTBuilder {
    
    /**
     * Costruisce una CompilationUnit Java completa partendo da un modello JavaClass.
     * 
     * @param javaClass Il modello della classe Java contenente campi e metodi
     * @return CompilationUnit che rappresenta la struttura completa del file sorgente Java
     */
    public CompilationUnit buildCompilationUnit(JavaClass javaClass) {
        CompilationUnit cu = new CompilationUnit();
        cu.setPackageDeclaration(javaClass.getPackageName());
        
        ClassOrInterfaceDeclaration classDeclaration = cu.addClass(javaClass.getClassName())
            .setPublic(true);
        
        // Add fields
        for (JavaField field : javaClass.getFields()) {
            addField(classDeclaration, field);
        }
        
        // Add methods
        for (JavaMethod method : javaClass.getMethods()) {
            addMethod(classDeclaration, method);
        }
        
        return cu;
    }
    
    /**
     * Aggiunge una dichiarazione di campo alla classe Java.
     * Il campo viene dichiarato come privato e può includere un valore iniziale.
     *
     * @param classDecl La dichiarazione della classe a cui aggiungere il campo
     * @param field Il modello del campo Java da aggiungere
     */
    private void addField(ClassOrInterfaceDeclaration classDecl, JavaField field) {
        Type type = parseType(field.getType());
        FieldDeclaration fieldDecl = classDecl.addField(type, field.getName(), Modifier.Keyword.PRIVATE);
        
        if (field.getInitialValue() != null) {
            fieldDecl.getVariable(0).setInitializer(field.getInitialValue());
        }
    }
    
    /**
     * Aggiunge una dichiarazione di metodo alla classe Java.
     * Il metodo viene dichiarato come pubblico e può includere parametri.
     * Il corpo del metodo viene lasciato vuoto per essere popolato successivamente
     * dal JavaClassBuilder.
     *
     * @param classDecl La dichiarazione della classe a cui aggiungere il metodo
     * @param method Il modello del metodo Java da aggiungere
     */
    private void addMethod(ClassOrInterfaceDeclaration classDecl, JavaMethod method) {
        MethodDeclaration methodDecl = classDecl.addMethod(method.getName(), Modifier.Keyword.PUBLIC);
        methodDecl.setType(parseType(method.getReturnType()));
        
        // Add parameters
        for (JavaMethod.Parameter param : method.getParameters()) {
            methodDecl.addParameter(parseType(param.getType()), param.getName());
        }
        
        // Method body will be set later by JavaClassBuilder
        methodDecl.setBody(null);
    }
    
    /**
     * Converte una stringa rappresentante un tipo Java nel corrispondente oggetto Type.
     * Gestisce i tipi primitivi (int, double, boolean), void e i tipi di classe.
     *
     * @param typeString La stringa rappresentante il tipo Java
     * @return L'oggetto Type corrispondente al tipo Java specificato
     */
    private Type parseType(String typeString) {
        switch (typeString) {
            case "int":
                return PrimitiveType.intType();
            case "double":
                return PrimitiveType.doubleType();
            case "boolean":
                return PrimitiveType.booleanType();
            case "void":
                return new com.github.javaparser.ast.type.VoidType();
            default:
                return new ClassOrInterfaceType(null, typeString);
        }
    }
}