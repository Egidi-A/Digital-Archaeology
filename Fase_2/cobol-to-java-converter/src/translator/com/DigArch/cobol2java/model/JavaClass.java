package com.DigArch.cobol2java.model;

import java.util.ArrayList;
import java.util.List;

/**
 * Modello che rappresenta una classe Java generata da un programma COBOL.
 * Contiene tutte le informazioni necessarie per generare una classe Java completa,
 * inclusi campi, metodi e configurazioni per il metodo main.
 */
public class JavaClass {
    private String className;
    private String packageName = "generated";
    private List<JavaField> fields;
    private List<JavaMethod> methods;
    private boolean needsMainMethod = true;
    private String mainMethodName;
    
    /**
     * Costruisce una nuova classe Java con il nome specificato.
     * Inizializza le liste vuote per campi e metodi.
     *
     * @param className Il nome della classe Java da generare
     */
    public JavaClass(String className) {
        this.className = className;
        this.fields = new ArrayList<>();
        this.methods = new ArrayList<>();
    }
    
    /**
     * Restituisce il nome della classe.
     *
     * @return Il nome della classe Java
     */
    public String getClassName() {
        return className;
    }
    
    /**
     * Imposta il nome della classe.
     *
     * @param className Il nuovo nome della classe
     */
    public void setClassName(String className) {
        this.className = className;
    }
    
    /**
     * Restituisce il nome del package.
     * Il package di default è "generated".
     *
     * @return Il nome del package Java
     */
    public String getPackageName() {
        return packageName;
    }
    
    /**
     * Imposta il nome del package.
     *
     * @param packageName Il nuovo nome del package
     */
    public void setPackageName(String packageName) {
        this.packageName = packageName;
    }
    
    /**
     * Restituisce la lista dei campi della classe.
     *
     * @return Lista dei campi Java
     */
    public List<JavaField> getFields() {
        return fields;
    }
    
    /**
     * Imposta la lista dei campi della classe.
     *
     * @param fields La nuova lista di campi
     */
    public void setFields(List<JavaField> fields) {
        this.fields = fields;
    }
    
    /**
     * Restituisce la lista dei metodi della classe.
     *
     * @return Lista dei metodi Java
     */
    public List<JavaMethod> getMethods() {
        return methods;
    }
    
    /**
     * Imposta la lista dei metodi della classe.
     * Il primo metodo della lista viene automaticamente impostato
     * come punto di ingresso principale (main entry point).
     *
     * @param methods La nuova lista di metodi
     */
    public void setMethods(List<JavaMethod> methods) {
        this.methods = methods;
        // First method becomes main entry point
        if (!methods.isEmpty()) {
            this.mainMethodName = methods.get(0).getName();
        }
    }
    
    /**
     * Verifica se la classe necessita di un metodo main.
     *
     * @return true se la classe deve includere un metodo main
     */
    public boolean needsMainMethod() {
        return needsMainMethod;
    }
    
    /**
     * Imposta se la classe necessita di un metodo main.
     *
     * @param needsMainMethod true se la classe deve includere un metodo main
     */
    public void setNeedsMainMethod(boolean needsMainMethod) {
        this.needsMainMethod = needsMainMethod;
    }
    
    /**
     * Restituisce il nome del metodo da chiamare nel main.
     *
     * @return Il nome del metodo entry point
     */
    public String getMainMethodName() {
        return mainMethodName;
    }
    
    /**
     * Imposta il nome del metodo da chiamare nel main.
     *
     * @param mainMethodName Il nome del metodo entry point
     */
    public void setMainMethodName(String mainMethodName) {
        this.mainMethodName = mainMethodName;
    }
}