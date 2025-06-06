package com.cobolconverter.parser;

import com.cobolconverter.model.DataDivision;
import com.cobolconverter.model.DataDivision.DataItem;
import java.util.ArrayList;
import java.util.List;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

/**
 * Parser per la DATA DIVISION di un programma COBOL.
 * <p>
 * Questa classe estrae e interpreta le sezioni principali della Data Division:
 * <ul>
 *   <li><b>FILE SECTION</b>: viene mantenuta come testo grezzo.</li>
 *   <li><b>WORKING-STORAGE SECTION</b>: viene convertita in una lista di {@link DataItem}.</li>
 *   <li><b>LOCAL-STORAGE SECTION</b>: viene mantenuta come testo grezzo.</li>
 *   <li><b>LINKAGE SECTION</b>: viene mantenuta come testo grezzo.</li>
 * </ul>
 * Il parser riconosce anche le condizioni di livello 88 come elementi speciali.
 */
public class DataDivisionParser {
    
    /**
     * Analizza il sorgente COBOL normalizzato ed estrae la Data Division.
     * <p>
     * Ogni sezione viene estratta e, per la Working-Storage, vengono creati oggetti {@link DataItem}.
     * @param normalizedSource sorgente COBOL normalizzato (tutto maiuscolo, spazi uniformi, ecc.)
     * @return oggetto {@link DataDivision} popolato con le informazioni estratte
     */
    public DataDivision parse(String normalizedSource) {
        DataDivision dataDiv = new DataDivision();
        
        // Trova la sezione DATA DIVISION
        Pattern dataDivPattern = Pattern.compile(
            "DATA\\s+DIVISION\\s*\\.([^\\n]*(?:\\n(?!\\s*(?:PROCEDURE)\\s+DIVISION)[^\\n]*)*)",
            Pattern.CASE_INSENSITIVE | Pattern.DOTALL
        );
        
        Matcher dataDivMatcher = dataDivPattern.matcher(normalizedSource);
        if (!dataDivMatcher.find()) {
            return dataDiv; // Ritorna vuoto se non trovata
        }
        
        String dataDivContent = dataDivMatcher.group(1);
        
        // Estrae la File Section come testo grezzo
        dataDiv.setFileSection(extractSection(dataDivContent, "FILE"));
        
        // Estrae e interpreta la Working-Storage Section
        String workingStorageContent = extractSection(dataDivContent, "WORKING-STORAGE");
        if (workingStorageContent != null) {
            List<DataItem> items = parseDataItems(workingStorageContent);
            dataDiv.setWorkingStorageItems(items);
        }
        
        // Estrae la Local-Storage Section come testo grezzo
        dataDiv.setLocalStorageSection(extractSection(dataDivContent, "LOCAL-STORAGE"));
        
        // Estrae la Linkage Section come testo grezzo
        dataDiv.setLinkageSection(extractSection(dataDivContent, "LINKAGE"));
        
        return dataDiv;
    }
    
    /**
     * Estrae il contenuto di una sezione specifica dalla Data Division.
     * @param content testo della Data Division
     * @param sectionName nome della sezione (es. "FILE", "WORKING-STORAGE")
     * @return contenuto della sezione come stringa, oppure null se non trovata
     */
    private String extractSection(String content, String sectionName) {
        Pattern pattern = Pattern.compile(
            sectionName + "\\s+SECTION\\s*\\.([^\\n]*(?:\\n(?!\\s*(?:FILE|WORKING-STORAGE|LOCAL-STORAGE|LINKAGE|PROCEDURE)\\s+(?:SECTION|DIVISION))[^\\n]*)*)",
            Pattern.CASE_INSENSITIVE | Pattern.DOTALL
        );
        
        Matcher matcher = pattern.matcher(content);
        if (matcher.find()) {
            return matcher.group(1).trim();
        }
        
        return null;
    }
    
    /**
     * Analizza la Working-Storage Section e crea una lista di {@link DataItem}.
     * <p>
     * Riconosce:
     * <ul>
     *   <li>Level number (es. 01, 05, 77, 88)</li>
     *   <li>Nome dell'item</li>
     *   <li>Clausola PIC/PICTURE</li>
     *   <li>Clausola VALUE</li>
     *   <li>Clausola OCCURS</li>
     * </ul>
     * Gli item di livello 88 (condition names) vengono aggiunti come elementi speciali.
     * Gli item FILLER vengono ignorati.
     * @param content testo della Working-Storage Section
     * @return lista di {@link DataItem}
     */
    private List<DataItem> parseDataItems(String content) {
        List<DataItem> items = new ArrayList<>();
        
        // Pattern per riconoscere gli item dati COBOL
        Pattern itemPattern = Pattern.compile(
            "(\\d{2})\\s+(\\S+)(?:\\s+PIC(?:TURE)?\\s+(?:IS\\s+)?([^.\\n]+?))?(?:\\s+VALUE\\s+(?:IS\\s+)?([^.\\n]+?))?(?:\\s+OCCURS\\s+([^.\\n]+?))?\\s*\\.",
            Pattern.CASE_INSENSITIVE | Pattern.MULTILINE
        );
        
        Matcher matcher = itemPattern.matcher(content);
        
        while (matcher.find()) {
            String level = matcher.group(1);
            String name = matcher.group(2);
            String picture = matcher.group(3);
            String value = matcher.group(4);
            String occurs = matcher.group(5);
            
            // Ignora gli item FILLER
            if ("FILLER".equalsIgnoreCase(name)) {
                continue;
            }
            
            DataItem item = new DataItem(level, name);
            
            if (picture != null) {
                item.setPicture(picture.trim());
            } else {
                // Se manca la PIC, è probabilmente un gruppo
                item.setGroup(true);
            }
            
            if (value != null) {
                // Pulisce il valore: rimuove virgolette e keyword COBOL speciali
                value = value.trim();
                if (value.startsWith("\"") && value.endsWith("\"")) {
                    value = value.substring(1, value.length() - 1);
                } else if (value.startsWith("'") && value.endsWith("'")) {
                    value = value.substring(1, value.length() - 1);
                } else if ("SPACES".equalsIgnoreCase(value) || "SPACE".equalsIgnoreCase(value)) {
                    value = "";
                } else if ("ZEROS".equalsIgnoreCase(value) || "ZERO".equalsIgnoreCase(value) || "ZEROES".equalsIgnoreCase(value)) {
                    value = "0";
                }
                item.setValue(value);
            }
            
            if (occurs != null) {
                // Estrae il numero dalla clausola OCCURS
                Pattern occursPattern = Pattern.compile("(\\d+)");
                Matcher occursMatcher = occursPattern.matcher(occurs);
                if (occursMatcher.find()) {
                    item.setOccurs(occursMatcher.group(1));
                }
            }
            
            items.add(item);
        }
        
        // Riconosce anche le condition names di livello 88
        Pattern conditionPattern = Pattern.compile(
            "88\\s+(\\S+)\\s+VALUE\\s+(?:IS\\s+)?([^.\\n]+)\\s*\\.",
            Pattern.CASE_INSENSITIVE | Pattern.MULTILINE
        );
        
        Matcher conditionMatcher = conditionPattern.matcher(content);
        while (conditionMatcher.find()) {
            String conditionName = conditionMatcher.group(1);
            String conditionValue = conditionMatcher.group(2).trim();
            
            // Crea un DataItem speciale per il livello 88
            DataItem condition = new DataItem("88", conditionName);
            condition.setValue(conditionValue);
            items.add(condition);
        }
        
        return items;
    }
}