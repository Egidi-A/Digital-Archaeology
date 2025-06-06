package com.cobolconverter.model;

/**
 * Rappresenta la ENVIRONMENT DIVISION di un programma COBOL.
 * <p>
 * Questa classe modella le due principali sezioni della Environment Division:
 * <ul>
 *   <li><b>Configuration Section</b>: contiene informazioni sull'ambiente di esecuzione,
 *       come il computer sorgente, il computer oggetto e i nomi speciali.</li>
 *   <li><b>Input-Output Section</b>: descrive la gestione dei file e il controllo I/O.</li>
 * </ul>
 * Ogni campo rappresenta una clausola o una sezione COBOL specifica.
 */
public class EnvironmentDivision {
    /**
     * Nome del computer sorgente (SOURCE-COMPUTER).
     */
    private String sourceComputer;
    /**
     * Nome del computer oggetto (OBJECT-COMPUTER).
     */
    private String objectComputer;
    /**
     * Clausola SPECIAL-NAMES, per la definizione di nomi speciali.
     */
    private String specialNames;
    
    /**
     * Clausola FILE-CONTROL della Input-Output Section.
     */
    private String fileControl;
    /**
     * Clausola I-O-CONTROL della Input-Output Section.
     */
    private String ioControl;
    
    // Getters and Setters

    /**
     * Restituisce il nome del computer sorgente (SOURCE-COMPUTER).
     * @return nome del computer sorgente
     */
    public String getSourceComputer() { return sourceComputer; }

    /**
     * Imposta il nome del computer sorgente (SOURCE-COMPUTER).
     * @param sourceComputer nome del computer sorgente
     */
    public void setSourceComputer(String sourceComputer) { this.sourceComputer = sourceComputer; }

    /**
     * Restituisce il nome del computer oggetto (OBJECT-COMPUTER).
     * @return nome del computer oggetto
     */
    public String getObjectComputer() { return objectComputer; }

    /**
     * Imposta il nome del computer oggetto (OBJECT-COMPUTER).
     * @param objectComputer nome del computer oggetto
     */
    public void setObjectComputer(String objectComputer) { this.objectComputer = objectComputer; }

    /**
     * Restituisce la clausola SPECIAL-NAMES.
     * @return special names
     */
    public String getSpecialNames() { return specialNames; }

    /**
     * Imposta la clausola SPECIAL-NAMES.
     * @param specialNames special names
     */
    public void setSpecialNames(String specialNames) { this.specialNames = specialNames; }

    /**
     * Restituisce la clausola FILE-CONTROL.
     * @return file control
     */
    public String getFileControl() { return fileControl; }

    /**
     * Imposta la clausola FILE-CONTROL.
     * @param fileControl file control
     */
    public void setFileControl(String fileControl) { this.fileControl = fileControl; }

    /**
     * Restituisce la clausola I-O-CONTROL.
     * @return io control
     */
    public String getIoControl() { return ioControl; }

    /**
     * Imposta la clausola I-O-CONTROL.
     * @param ioControl io control
     */
    public void setIoControl(String ioControl) { this.ioControl = ioControl; }

    /**
     * Restituisce un commento riassuntivo della Environment Division,
     * utile per la generazione di codice o documentazione.
     * @return commento Java con le sezioni presenti
     */
    public String toJavaComment() {
        StringBuilder sb = new StringBuilder();
        sb.append("    // Environment Division\n");
        if (sourceComputer != null) {
            sb.append("    // Source Computer: ").append(sourceComputer).append("\n");
        }
        if (objectComputer != null) {
            sb.append("    // Object Computer: ").append(objectComputer).append("\n");
        }
        if (fileControl != null) {
            sb.append("    // File Control: Present\n");
        }
        if (ioControl != null) {
            sb.append("    // Input/Output Control: Present\n");
        }
        return sb.toString();
    }
}