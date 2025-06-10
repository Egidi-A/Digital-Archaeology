package com.DigArch.cobol2java.ast.visitor;

import com.DigArch.cobol2java.model.JavaField;
import com.DigArch.cobol2java.model.JavaMethod;
import java.util.ArrayList;
import java.util.List;

/**
 * Classe di contesto per il visitor dell'AST COBOL.
 * Mantiene lo stato durante la visita dell'albero sintattico, raccogliendo
 * informazioni sul programma COBOL e memorizzando gli elementi convertiti
 * in Java (campi e metodi).
 */
public class VisitorContext {
    private String programName;
    private List<JavaField> fields;
    private List<JavaMethod> methods;
    
    /**
     * Costruisce un nuovo contesto con liste vuote per campi e metodi.
     */
    public VisitorContext() {
        this.fields = new ArrayList<>();
        this.methods = new ArrayList<>();
    }
    
    /**
     * Imposta il nome del programma COBOL.
     *
     * @param programName Il nome del programma COBOL
     */
    public void setProgramName(String programName) {
        this.programName = programName;
    }
    
    /**
     * Restituisce il nome del programma COBOL.
     *
     * @return Il nome del programma COBOL
     */
    public String getProgramName() {
        return programName;
    }
    
    /**
     * Genera il nome della classe Java dal nome del programma COBOL.
     * Se il nome del programma non è disponibile, restituisce "GeneratedClass".
     * Converte il nome seguendo le convenzioni Java:
     * - Rimuove i trattini
     * - Applica il CamelCase
     * - Inizia con lettera maiuscola
     *
     * @return Il nome della classe Java generato
     */
    public String getClassName() {
        if (programName == null || programName.isEmpty()) {
            return "GeneratedClass";
        }
        // Convert COBOL program name to Java class name
        return toCamelCase(programName.replace("-", "_"), true);
    }
    
    /**
     * Aggiunge un campo Java alla lista dei campi.
     *
     * @param field Il campo Java da aggiungere
     */
    public void addField(JavaField field) {
        fields.add(field);
    }
    
    /**
     * Restituisce una copia della lista dei campi Java.
     * La lista restituita è una copia difensiva per evitare modifiche esterne.
     *
     * @return Lista dei campi Java
     */
    public List<JavaField> getFields() {
        return new ArrayList<>(fields);
    }
    
    /**
     * Aggiunge un metodo Java alla lista dei metodi.
     *
     * @param method Il metodo Java da aggiungere
     */
    public void addMethod(JavaMethod method) {
        methods.add(method);
    }
    
    /**
     * Restituisce una copia della lista dei metodi Java.
     * La lista restituita è una copia difensiva per evitare modifiche esterne.
     *
     * @return Lista dei metodi Java
     */
    public List<JavaMethod> getMethods() {
        return new ArrayList<>(methods);
    }
    
    /**
     * Converte una stringa in formato camelCase.
     * 
     * @param input La stringa da convertire
     * @param capitalizeFirst Se true, la prima lettera sarà maiuscola
     * @return La stringa convertita in camelCase
     */
    private String toCamelCase(String input, boolean capitalizeFirst) {
        StringBuilder result = new StringBuilder();
        boolean nextCapital = capitalizeFirst;
        
        for (char c : input.toCharArray()) {
            if (c == '_' || c == '-') {
                nextCapital = true;
            } else if (nextCapital) {
                result.append(Character.toUpperCase(c));
                nextCapital = false;
            } else {
                result.append(Character.toLowerCase(c));
            }
        }
        
        return result.toString();
    }
}