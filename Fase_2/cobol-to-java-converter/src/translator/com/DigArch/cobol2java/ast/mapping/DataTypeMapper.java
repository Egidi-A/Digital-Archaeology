package com.DigArch.cobol2java.ast.mapping;

import java.util.regex.Matcher;
import java.util.regex.Pattern;

/**
 * Classe responsabile della mappatura dei tipi di dati da COBOL a Java.
 * Gestisce la conversione delle clausole PICTURE COBOL nei corrispondenti
 * tipi di dato Java e la conversione dei valori letterali.
 */
public class DataTypeMapper {
    
    /** Pattern per riconoscere campi numerici COBOL (sequenze di 9 con eventuale V per decimali) */
    private static final Pattern NUMERIC_PATTERN = Pattern.compile("9+(?:V9+)?");
    
    /** Pattern per riconoscere campi decimali COBOL (sequenze di 9 con V obbligatorio) */
    private static final Pattern DECIMAL_PATTERN = Pattern.compile("9+V9+");
    
    /** Pattern per riconoscere campi alfanumerici COBOL (sequenze di X) */
    private static final Pattern ALPHANUMERIC_PATTERN = Pattern.compile("X+");
    
    /**
     * Converte una clausola PICTURE COBOL nel corrispondente tipo Java.
     * Gestisce i seguenti casi:
     * - Numeri decimali (con V) -> BigDecimal
     * - Numeri interi (senza V) -> int o long in base alla dimensione
     * - Campi alfanumerici -> String
     * - Numeri con segno (S9...) -> tipo appropriato con segno
     *
     * @param picture La clausola PICTURE COBOL da convertire
     * @return Il nome del tipo Java corrispondente
     */
    public String mapPictureToJavaType(String picture) {
        String pic = picture.toUpperCase().replace("PIC", "").trim();
        
        if (DECIMAL_PATTERN.matcher(pic).matches()) {
            return "BigDecimal";
        } else if (NUMERIC_PATTERN.matcher(pic).matches()) {
            if (pic.length() <= 9) {
                return "int";
            } else {
                return "long";
            }
        } else if (ALPHANUMERIC_PATTERN.matcher(pic).matches()) {
            return "String";
        } else if (pic.startsWith("S9")) {
            return mapSignedNumeric(pic);
        }
        
        // Default to String for complex types
        return "String";
    }
    
    /**
     * Converte un valore letterale COBOL nel formato appropriato per Java.
     * Gestisce la formattazione corretta in base al tipo Java di destinazione,
     * inclusa la rimozione di keyword COBOL e la gestione delle quotature.
     *
     * @param cobolValue Il valore letterale COBOL da convertire
     * @param javaType Il tipo Java di destinazione
     * @return Il valore formattato per Java
     */
    public String mapValue(String cobolValue, String javaType) {
        String value = cobolValue.trim();
        
        // Remove VALUE keyword
        if (value.startsWith("VALUE")) {
            value = value.substring(5).trim();
        }
        
        // Remove quotes for strings
        if (value.startsWith("\"") || value.startsWith("'")) {
            value = value.substring(1, value.length() - 1);
        }
        
        switch (javaType) {
            case "String":
                return "\"" + value + "\"";
            case "int":
            case "long":
                return value.replace(" ", "");
            case "BigDecimal":
                return "new BigDecimal(\"" + value + "\")";
            case "boolean":
                return "true".equalsIgnoreCase(value) ? "true" : "false";
            default:
                return "null";
        }
    }
    
    /**
     * Converte una clausola PICTURE per numeri con segno nel tipo Java appropriato.
     * Analizza la parte numerica dopo il segno 'S' per determinare
     * se utilizzare int, long o BigDecimal.
     *
     * @param picture La clausola PICTURE del numero con segno (formato S9...)
     * @return Il tipo Java appropriato per il numero con segno
     */
    private String mapSignedNumeric(String picture) {
        String unsigned = picture.substring(1);
        if (DECIMAL_PATTERN.matcher(unsigned).matches()) {
            return "BigDecimal";
        } else if (unsigned.length() <= 9) {
            return "int";
        } else {
            return "long";
        }
    }
}