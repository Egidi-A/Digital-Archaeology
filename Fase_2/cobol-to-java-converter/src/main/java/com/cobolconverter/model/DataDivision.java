package com.cobolconverter.model;

import java.util.ArrayList;
import java.util.List;

/**
 * Rappresenta la DATA DIVISION di un programma COBOL.
 * <p>
 * Questa classe modella le principali sezioni della Data Division:
 * <ul>
 *   <li><b>File Section</b>: descrive i file utilizzati dal programma COBOL.</li>
 *   <li><b>Working-Storage Section</b>: contiene le variabili e strutture dati persistenti durante l'esecuzione.</li>
 *   <li><b>Local-Storage Section</b>: contiene variabili temporanee, inizializzate a ogni invocazione.</li>
 *   <li><b>Linkage Section</b>: definisce i dati passati da o verso altri programmi.</li>
 * </ul>
 * Ogni elemento dati della Working-Storage Section è rappresentato dalla classe interna {@link DataItem}.
 */
public class DataDivision {
    /**
     * Contenuto della FILE SECTION COBOL.
     */
    private String fileSection;

    /**
     * Lista degli elementi della WORKING-STORAGE SECTION.
     */
    private List<DataItem> workingStorageItems = new ArrayList<>();

    /**
     * Contenuto della LOCAL-STORAGE SECTION COBOL.
     */
    private String localStorageSection;

    /**
     * Contenuto della LINKAGE SECTION COBOL.
     */
    private String linkageSection;
    
    /**
     * Rappresenta un singolo elemento dati COBOL (campo o gruppo).
     * <p>
     * Ogni DataItem può rappresentare un campo elementare o un gruppo (struttura annidata).
     */
    public static class DataItem {
        /**
         * Livello COBOL (es. "01", "05", "77").
         */
        private String level;
        /**
         * Nome COBOL dell'elemento dati.
         */
        private String name;
        /**
         * Clausola PICTURE COBOL (es. "X(10)", "9(5)V99").
         */
        private String picture;
        /**
         * Valore iniziale (clausola VALUE COBOL).
         */
        private String value;
        /**
         * Clausola OCCURS (per array COBOL).
         */
        private String occurs;
        /**
         * Indica se l'elemento è un gruppo (struttura composta).
         */
        private boolean isGroup;
        
        /**
         * Costruttore per DataItem.
         * @param level livello COBOL
         * @param name nome COBOL
         */
        public DataItem(String level, String name) {
            this.level = level;
            this.name = name;
            this.isGroup = false;
        }
        
        /** Restituisce il livello COBOL. */
        public String getLevel() { return level; }
        /** Imposta il livello COBOL. */
        public void setLevel(String level) { this.level = level; }
        
        /** Restituisce il nome COBOL. */
        public String getName() { return name; }
        /** Imposta il nome COBOL. */
        public void setName(String name) { this.name = name; }
        
        /** Restituisce la clausola PICTURE. */
        public String getPicture() { return picture; }
        /** Imposta la clausola PICTURE. */
        public void setPicture(String picture) { this.picture = picture; }
        
        /** Restituisce il valore iniziale. */
        public String getValue() { return value; }
        /** Imposta il valore iniziale. */
        public void setValue(String value) { this.value = value; }
        
        /** Restituisce la clausola OCCURS. */
        public String getOccurs() { return occurs; }
        /** Imposta la clausola OCCURS. */
        public void setOccurs(String occurs) { this.occurs = occurs; }
        
        /** Indica se l'elemento è un gruppo. */
        public boolean isGroup() { return isGroup; }
        /** Imposta se l'elemento è un gruppo. */
        public void setGroup(boolean isGroup) { this.isGroup = isGroup; }
        
        /**
         * Restituisce il tipo Java corrispondente alla clausola PICTURE COBOL.
         * <ul>
         *   <li>Numerico senza decimali: int</li>
         *   <li>Numerico con decimali: double</li>
         *   <li>Alfanumerico: String</li>
         * </ul>
         * @return tipo Java equivalente
         */
        public String getJavaType() {
            if (picture == null) return "Object";
            
            if (picture.matches(".*[9]+.*")) {
                if (picture.contains("V") || picture.contains(".")) {
                    return "double";
                }
                return "int";
            } else if (picture.matches(".*[X]+.*") || picture.matches(".*[A]+.*")) {
                return "String";
            }
            return "String"; // Default
        }
        
        /**
         * Restituisce la dichiarazione del campo Java equivalente all'item COBOL.
         * Include OCCURS e VALUE se presenti.
         * @return dichiarazione campo Java come stringa
         */
        public String toJavaField() {
            StringBuilder sb = new StringBuilder();
            String javaName = toJavaName(name);
            String javaType = getJavaType();
            
            if (occurs != null) {
                sb.append("    private ").append(javaType).append("[] ").append(javaName);
                sb.append("; // OCCURS ").append(occurs);
            } else {
                sb.append("    private ").append(javaType).append(" ").append(javaName);
                if (value != null) {
                    if ("String".equals(javaType)) {
                        sb.append(" = \"").append(value.replaceAll("\"", "\\\\\"")).append("\"");
                    } else {
                        sb.append(" = ").append(value);
                    }
                }
            }
            sb.append("; // PIC ").append(picture != null ? picture : "GROUP");
            
            return sb.toString();
        }
        
        /**
         * Converte il nome COBOL in camelCase per Java.
         * Esempio: CUSTOMER-NAME → customerName
         * @param cobolName nome COBOL
         * @return nome Java in camelCase
         */
        private String toJavaName(String cobolName) {
            if (cobolName == null) return "unnamed";
            
            String[] parts = cobolName.split("[-_]");
            StringBuilder result = new StringBuilder();
            
            for (int i = 0; i < parts.length; i++) {
                String part = parts[i];
                if (!part.isEmpty()) {
                    if (i == 0) {
                        result.append(part.toLowerCase());
                    } else {
                        result.append(Character.toUpperCase(part.charAt(0)));
                        if (part.length() > 1) {
                            result.append(part.substring(1).toLowerCase());
                        }
                    }
                }
            }
            
            return result.toString();
        }
    }
    
    /** Restituisce il contenuto della FILE SECTION. */
    public String getFileSection() { return fileSection; }
    /** Imposta il contenuto della FILE SECTION. */
    public void setFileSection(String fileSection) { this.fileSection = fileSection; }
    
    /** Restituisce la lista degli item della WORKING-STORAGE SECTION. */
    public List<DataItem> getWorkingStorageItems() { return workingStorageItems; }
    /** Imposta la lista degli item della WORKING-STORAGE SECTION. */
    public void setWorkingStorageItems(List<DataItem> workingStorageItems) { 
        this.workingStorageItems = workingStorageItems; 
    }
    
    /** Restituisce il contenuto della LOCAL-STORAGE SECTION. */
    public String getLocalStorageSection() { return localStorageSection; }
    /** Imposta il contenuto della LOCAL-STORAGE SECTION. */
    public void setLocalStorageSection(String localStorageSection) { 
        this.localStorageSection = localStorageSection; 
    }
    
    /** Restituisce il contenuto della LINKAGE SECTION. */
    public String getLinkageSection() { return linkageSection; }
    /** Imposta il contenuto della LINKAGE SECTION. */
    public void setLinkageSection(String linkageSection) { this.linkageSection = linkageSection; }
    
    /**
     * Restituisce un commento riassuntivo della Data Division, utile per la generazione di codice Java.
     * @return commento Java con le sezioni presenti
     */
    public String toJavaComment() {
        StringBuilder sb = new StringBuilder();
        sb.append("    // Data Division\n");
        if (fileSection != null) {
            sb.append("    // File Section: Present\n");
        }
        if (!workingStorageItems.isEmpty()) {
            sb.append("    // Working-Storage: ").append(workingStorageItems.size()).append(" items\n");
        }
        if (localStorageSection != null) {
            sb.append("    // Local-Storage Section: Present\n");
        }
        if (linkageSection != null) {
            sb.append("    // Linkage Section: Present\n");
        }
        return sb.toString();
    }
}