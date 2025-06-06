package com.cobolconverter.parser;

import com.cobolconverter.model.IdentificationDivision;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

/**
 * Parser per la IDENTIFICATION DIVISION di un programma COBOL.
 * <p>
 * Questa classe estrae e interpreta i principali campi della Identification Division:
 * <ul>
 *   <li><b>PROGRAM-ID</b>: nome identificativo del programma COBOL.</li>
 *   <li><b>AUTHOR</b>: autore del programma.</li>
 *   <li><b>INSTALLATION</b>: informazioni sull'installazione o ambiente.</li>
 *   <li><b>DATE-WRITTEN</b>: data di scrittura del programma.</li>
 *   <li><b>DATE-COMPILED</b>: data di compilazione del programma.</li>
 *   <li><b>SECURITY</b>: eventuali informazioni di sicurezza.</li>
 *   <li><b>REMARKS</b>: commenti o note aggiuntive (può essere multilinea).</li>
 * </ul>
 * Ogni campo viene estratto come testo e assegnato all'oggetto {@link IdentificationDivision}.
 * Il parser gestisce sia campi su singola riga che campi multilinea.
 */
public class IdentificationDivisionParser {
    
    /**
     * Analizza il sorgente COBOL normalizzato ed estrae la Identification Division.
     * <p>
     * Ogni campo principale viene popolato nell'oggetto {@link IdentificationDivision}.
     * @param normalizedSource sorgente COBOL normalizzato (tutto maiuscolo, spazi uniformi, ecc.)
     * @return oggetto {@link IdentificationDivision} popolato con le informazioni estratte
     */
    public IdentificationDivision parse(String normalizedSource) {
        IdentificationDivision idDiv = new IdentificationDivision();
        
        // Trova la sezione IDENTIFICATION DIVISION tramite espressione regolare.
        Pattern idDivPattern = Pattern.compile(
            "IDENTIFICATION\\s+DIVISION\\s*\\.([^\\n]*(?:\\n(?!\\s*(?:ENVIRONMENT|DATA|PROCEDURE)\\s+DIVISION)[^\\n]*)*)",
            Pattern.CASE_INSENSITIVE | Pattern.DOTALL
        );
        
        Matcher idDivMatcher = idDivPattern.matcher(normalizedSource);
        if (!idDivMatcher.find()) {
            // Se la divisione non viene trovata, restituisce un oggetto vuoto e stampa un warning.
            System.err.println("Warning: IDENTIFICATION DIVISION not found");
            return idDiv;
        }
        
        String idDivContent = idDivMatcher.group(1);
        
        // Estrae i campi principali tramite metodi di supporto.
        idDiv.setProgramId(extractField(idDivContent, "PROGRAM-ID"));
        idDiv.setAuthor(extractField(idDivContent, "AUTHOR"));
        idDiv.setInstallation(extractField(idDivContent, "INSTALLATION"));
        idDiv.setDateWritten(extractField(idDivContent, "DATE-WRITTEN"));
        idDiv.setDateCompiled(extractField(idDivContent, "DATE-COMPILED"));
        idDiv.setSecurity(extractField(idDivContent, "SECURITY"));
        idDiv.setRemarks(extractMultilineField(idDivContent, "REMARKS"));
        
        return idDiv;
    }
    
    /**
     * Estrae il valore di un campo su singola riga dalla sezione specificata.
     * <p>
     * Cerca la presenza del campo richiesto e restituisce il valore associato,
     * oppure null se il campo non è presente.
     * @param content testo della sezione
     * @param fieldName nome del campo (es. "PROGRAM-ID")
     * @return valore del campo, oppure null se non trovato
     */
    private String extractField(String content, String fieldName) {
        Pattern pattern = Pattern.compile(
            fieldName + "\\s*\\.\\s*([^.\\n]+)\\s*\\.",
            Pattern.CASE_INSENSITIVE
        );
        
        Matcher matcher = pattern.matcher(content);
        if (matcher.find()) {
            // Restituisce il valore trovato, rimuovendo eventuali spazi superflui.
            return matcher.group(1).trim();
        }
        
        return null;
    }
    
    /**
     * Estrae il valore di un campo multilinea dalla sezione specificata.
     * <p>
     * Il campo viene estratto fino al prossimo campo o alla fine della sezione.
     * Utile per campi come REMARKS che possono occupare più righe.
     * @param content testo della sezione
     * @param fieldName nome del campo (es. "REMARKS")
     * @return valore del campo multilinea, oppure null se non trovato
     */
    private String extractMultilineField(String content, String fieldName) {
        Pattern pattern = Pattern.compile(
            fieldName + "\\s*\\.\\s*([^.]+(?:\\.[^.]+)*?)\\s*(?=\\n\\s*[A-Z]+-?[A-Z]*\\s*\\.)",
            Pattern.CASE_INSENSITIVE | Pattern.DOTALL
        );
        
        Matcher matcher = pattern.matcher(content);
        if (matcher.find()) {
            // Restituisce il valore trovato, rimuovendo spazi multipli.
            return matcher.group(1).trim().replaceAll("\\s+", " ");
        }
        
        return null;
    }
}