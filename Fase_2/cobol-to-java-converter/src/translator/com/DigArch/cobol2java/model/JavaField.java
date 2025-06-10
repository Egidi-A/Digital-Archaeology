package com.DigArch.cobol2java.model;

/**
 * Modello che rappresenta un campo di una classe Java.
 * Questa classe mantiene tutte le informazioni necessarie per generare
 * la dichiarazione di un campo in una classe Java, inclusi tipo, nome,
 * valore iniziale e modificatori.
 */
public class JavaField {
    private String name;            // Nome del campo
    private String type;            // Tipo del campo (es. "String", "int")
    private String initialValue;    // Valore iniziale opzionale
    private boolean isStatic = false;       // Se il campo è statico
    private boolean isFinal = false;        // Se il campo è finale (costante)
    
    /**
     * Costruttore di default.
     * Crea un nuovo campo senza nome né tipo.
     */
    public JavaField() {
    }
    
    /**
     * Costruisce un nuovo campo con nome e tipo specificati.
     *
     * @param name Il nome del campo
     * @param type Il tipo del campo
     */
    public JavaField(String name, String type) {
        this.name = name;
        this.type = type;
    }
    
    /**
     * Restituisce il nome del campo.
     *
     * @return Il nome del campo
     */
    public String getName() {
        return name;
    }
    
    /**
     * Imposta il nome del campo.
     *
     * @param name Il nuovo nome del campo
     */
    public void setName(String name) {
        this.name = name;
    }
    
    /**
     * Restituisce il tipo del campo.
     *
     * @return Il tipo del campo (es. "String", "int")
     */
    public String getType() {
        return type;
    }
    
    /**
     * Imposta il tipo del campo.
     *
     * @param type Il nuovo tipo del campo
     */
    public void setType(String type) {
        this.type = type;
    }
    
    /**
     * Restituisce il valore iniziale del campo.
     *
     * @return Il valore iniziale del campo, o null se non impostato
     */
    public String getInitialValue() {
        return initialValue;
    }
    
    /**
     * Imposta il valore iniziale del campo.
     *
     * @param initialValue Il nuovo valore iniziale del campo
     */
    public void setInitialValue(String initialValue) {
        this.initialValue = initialValue;
    }
    
    /**
     * Verifica se il campo è dichiarato come statico.
     *
     * @return true se il campo è statico, false altrimenti
     */
    public boolean isStatic() {
        return isStatic;
    }
    
    /**
     * Imposta se il campo deve essere dichiarato come statico.
     *
     * @param isStatic true per rendere il campo statico
     */
    public void setStatic(boolean isStatic) {
        this.isStatic = isStatic;
    }
    
    /**
     * Verifica se il campo è dichiarato come final.
     *
     * @return true se il campo è final, false altrimenti
     */
    public boolean isFinal() {
        return isFinal;
    }
    
    /**
     * Imposta se il campo deve essere dichiarato come final.
     *
     * @param isFinal true per rendere il campo final
     */
    public void setFinal(boolean isFinal) {
        this.isFinal = isFinal;
    }
}