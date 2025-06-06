package com.cobolconverter.parser;

import com.cobolconverter.model.EnvironmentDivision;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

/**
 * Parser per la ENVIRONMENT DIVISION di un programma COBOL.
 * <p>
 * Questa classe estrae e interpreta le principali sezioni della Environment Division:
 * <ul>
 *   <li><b>Configuration Section</b>: estrae SOURCE-COMPUTER, OBJECT-COMPUTER e SPECIAL-NAMES.</li>
 *   <li><b>Input-Output Section</b>: estrae FILE-CONTROL e I-O-CONTROL.</li>
 * </ul>
 * Ogni campo viene estratto come testo grezzo e assegnato all'oggetto {@link EnvironmentDivision}.
 * Il parser gestisce sia campi su singola riga che campi multilinea.
 */
public class EnvironmentDivisionParser {
    
    /**
     * Analizza il sorgente COBOL normalizzato ed estrae la Environment Division.
     * <p>
     * Ogni sezione viene estratta e i campi principali vengono popolati nell'oggetto {@link EnvironmentDivision}.
     * @param normalizedSource sorgente COBOL normalizzato (tutto maiuscolo, spazi uniformi, ecc.)
     * @return oggetto {@link EnvironmentDivision} popolato con le informazioni estratte
     */
    public EnvironmentDivision parse(String normalizedSource) {
        EnvironmentDivision envDiv = new EnvironmentDivision();
        
        // Trova la sezione ENVIRONMENT DIVISION
        Pattern envDivPattern = Pattern.compile(
            "ENVIRONMENT\\s+DIVISION\\s*\\.([^\\n]*(?:\\n(?!\\s*(?:DATA|PROCEDURE)\\s+DIVISION)[^\\n]*)*)",
            Pattern.CASE_INSENSITIVE | Pattern.DOTALL
        );
        
        Matcher envDivMatcher = envDivPattern.matcher(normalizedSource);
        if (!envDivMatcher.find()) {
            return envDiv; // Ritorna vuoto se non trovata
        }
        
        String envDivContent = envDivMatcher.group(1);
        
        // Estrae la Configuration Section
        Pattern configPattern = Pattern.compile(
            "CONFIGURATION\\s+SECTION\\s*\\.([^\\n]*(?:\\n(?!\\s*(?:INPUT-OUTPUT|DATA|PROCEDURE)\\s+(?:SECTION|DIVISION))[^\\n]*)*)",
            Pattern.CASE_INSENSITIVE | Pattern.DOTALL
        );
        
        Matcher configMatcher = configPattern.matcher(envDivContent);
        if (configMatcher.find()) {
            String configContent = configMatcher.group(1);
            
            // Estrae SOURCE-COMPUTER
            envDiv.setSourceComputer(extractField(configContent, "SOURCE-COMPUTER"));
            
            // Estrae OBJECT-COMPUTER
            envDiv.setObjectComputer(extractField(configContent, "OBJECT-COMPUTER"));
            
            // Estrae SPECIAL-NAMES (può essere multilinea)
            envDiv.setSpecialNames(extractMultilineField(configContent, "SPECIAL-NAMES"));
        }
        
        // Estrae la Input-Output Section
        Pattern ioPattern = Pattern.compile(
            "INPUT-OUTPUT\\s+SECTION\\s*\\.([^\\n]*(?:\\n(?!\\s*(?:DATA|PROCEDURE)\\s+DIVISION)[^\\n]*)*)",
            Pattern.CASE_INSENSITIVE | Pattern.DOTALL
        );
        
        Matcher ioMatcher = ioPattern.matcher(envDivContent);
        if (ioMatcher.find()) {
            String ioContent = ioMatcher.group(1);
            
            // Estrae FILE-CONTROL (può essere multilinea)
            envDiv.setFileControl(extractMultilineField(ioContent, "FILE-CONTROL"));
            
            // Estrae I-O-CONTROL (può essere multilinea)
            envDiv.setIoControl(extractMultilineField(ioContent, "I-O-CONTROL"));
        }
        
        return envDiv;
    }
    
    /**
     * Estrae il valore di un campo su singola riga dalla sezione specificata.
     * @param content testo della sezione
     * @param fieldName nome del campo (es. "SOURCE-COMPUTER")
     * @return valore del campo, oppure null se non trovato
     */
    private String extractField(String content, String fieldName) {
        Pattern pattern = Pattern.compile(
            fieldName + "\\s*\\.\\s*([^.\\n]+)\\s*\\.",
            Pattern.CASE_INSENSITIVE
        );
        
        Matcher matcher = pattern.matcher(content);
        if (matcher.find()) {
            return matcher.group(1).trim();
        }
        
        return null;
    }
    
    /**
     * Estrae il valore di un campo multilinea dalla sezione specificata.
     * <p>
     * Il campo viene estratto fino al prossimo campo o alla fine della sezione.
     * @param content testo della sezione
     * @param fieldName nome del campo (es. "SPECIAL-NAMES", "FILE-CONTROL")
     * @return valore del campo multilinea, oppure null se non trovato
     */
    private String extractMultilineField(String content, String fieldName) {
        Pattern pattern = Pattern.compile(
            fieldName + "\\s*\\.\\s*([^.]+(?:\\.[^.]+)*?)\\s*(?=\\n\\s*[A-Z]+-?[A-Z]*\\s*\\.)",
            Pattern.CASE_INSENSITIVE | Pattern.DOTALL
        );
        
        Matcher matcher = pattern.matcher(content);
        if (matcher.find()) {
            return matcher.group(1).trim().replaceAll("\\s+", " ");
        }
        
        return null;
    }
}