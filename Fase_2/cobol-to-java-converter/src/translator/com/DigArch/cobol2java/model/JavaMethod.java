package com.DigArch.cobol2java.model;

import java.util.ArrayList;
import java.util.List;

/**
 * Modello che rappresenta un metodo di una classe Java.
 * Questa classe mantiene tutte le informazioni necessarie per generare
 * la dichiarazione e l'implementazione di un metodo Java, inclusi
 * nome, tipo di ritorno, parametri, corpo del metodo e modificatori.
 */
public class JavaMethod {
    private String name;            // Nome del metodo
    private String returnType = "void";      // Tipo di ritorno (default: "void")
    private List<Parameter> parameters;    // Lista dei parametri
    private List<String> statements;       // Corpo del metodo
    private boolean isStatic = false;             // Se il metodo è statico
    
    /**
     * Costruttore di default.
     * Inizializza le liste vuote per parametri e istruzioni.
     */
    public JavaMethod() {
        this.parameters = new ArrayList<>();
        this.statements = new ArrayList<>();
    }
    
    /**
     * Costruisce un nuovo metodo con il nome specificato.
     *
     * @param name Il nome del metodo
     */
    public JavaMethod(String name) {
        this();
        this.name = name;
    }
    
    /**
     * Restituisce il nome del metodo.
     *
     * @return Il nome del metodo
     */
    public String getName() {
        return name;
    }
    
    /**
     * Imposta il nome del metodo.
     *
     * @param name Il nuovo nome del metodo
     */
    public void setName(String name) {
        this.name = name;
    }
    
    /**
     * Restituisce il tipo di ritorno del metodo.
     *
     * @return Il tipo di ritorno del metodo
     */
    public String getReturnType() {
        return returnType;
    }
    
    /**
     * Imposta il tipo di ritorno del metodo.
     *
     * @param returnType Il nuovo tipo di ritorno
     */
    public void setReturnType(String returnType) {
        this.returnType = returnType;
    }
    
    /**
     * Restituisce la lista dei parametri del metodo.
     *
     * @return Lista dei parametri del metodo
     */
    public List<Parameter> getParameters() {
        return parameters;
    }
    
    /**
     * Imposta la lista dei parametri del metodo.
     *
     * @param parameters La nuova lista di parametri
     */
    public void setParameters(List<Parameter> parameters) {
        this.parameters = parameters;
    }
    
    /**
     * Aggiunge un nuovo parametro al metodo.
     *
     * @param type Il tipo del parametro
     * @param name Il nome del parametro
     */
    public void addParameter(String type, String name) {
        parameters.add(new Parameter(type, name));
    }
    
    /**
     * Restituisce la lista delle istruzioni che compongono il corpo del metodo.
     *
     * @return Lista delle istruzioni del metodo
     */
    public List<String> getStatements() {
        return statements;
    }
    
    /**
     * Imposta la lista delle istruzioni del metodo.
     *
     * @param statements La nuova lista di istruzioni
     */
    public void setStatements(List<String> statements) {
        this.statements = statements;
    }
    
    /**
     * Aggiunge una nuova istruzione al corpo del metodo.
     *
     * @param statement L'istruzione da aggiungere
     */
    public void addStatement(String statement) {
        statements.add(statement);
    }
    
    /**
     * Verifica se il metodo è dichiarato come statico.
     *
     * @return true se il metodo è statico, false altrimenti
     */
    public boolean isStatic() {
        return isStatic;
    }
    
    /**
     * Imposta se il metodo deve essere dichiarato come statico.
     *
     * @param isStatic true per rendere il metodo statico
     */
    public void setStatic(boolean isStatic) {
        this.isStatic = isStatic;
    }
    
    /**
     * Classe interna che rappresenta un parametro di un metodo.
     * Contiene il tipo e il nome del parametro.
     */
    public static class Parameter {
        private String type;    // Tipo del parametro
        private String name;    // Nome del parametro
        
        /**
         * Costruisce un nuovo parametro.
         *
         * @param type Il tipo del parametro
         * @param name Il nome del parametro
         */
        public Parameter(String type, String name) {
            this.type = type;
            this.name = name;
        }
        
        /**
         * Restituisce il tipo del parametro.
         *
         * @return Il tipo del parametro
         */
        public String getType() {
            return type;
        }
        
        /**
         * Restituisce il nome del parametro.
         *
         * @return Il nome del parametro
         */
        public String getName() {
            return name;
        }
    }
}