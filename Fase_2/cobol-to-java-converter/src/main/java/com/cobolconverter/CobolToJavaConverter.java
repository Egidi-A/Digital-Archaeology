package com.cobolconverter;

import java.io.*;
import com.cobolconverter.model.*;
import com.cobolconverter.parser.*;
import com.cobolconverter.generator.JavaCodeGenerator;

/**
 * Applicazione principale per la conversione di un programma COBOL in codice Java.
 * <p>
 * Il programma esegue le seguenti operazioni:
 * <ol>
 *   <li>Legge il file sorgente COBOL specificato da linea di comando.</li>
 *   <li>Normalizza il sorgente rimuovendo numeri di sequenza, commenti e spazi inutili.</li>
 *   <li>Analizza le divisioni principali (Identification, Environment, Data, Procedure) tramite i rispettivi parser.</li>
 *   <li>Genera il codice Java equivalente utilizzando {@link JavaCodeGenerator}.</li>
 *   <li>Scrive il risultato su file con estensione <code>.java</code>.</li>
 * </ol>
 * <b>Uso:</b> <code>java CobolToJavaConverter &lt;input-cobol-file&gt;</code>
 */
public class CobolToJavaConverter {
    
    /**
     * Metodo principale dell'applicazione.
     * <p>
     * Gestisce il flusso di conversione da COBOL a Java.
     * @param args argomenti da linea di comando (richiede il percorso del file COBOL)
     */
    public static void main(String[] args) {
        if (args.length < 1) {
            System.out.println("Usage: java CobolToJavaConverter <input-cobol-file>");
            return;
        }
        
        String inputFile = args[0];
        // Genera il nome del file di output sostituendo l'estensione con .java
        String outputFile = inputFile.replaceAll("\\.(cob|cbl|cobol)$", "") + ".java";        
        try {
            // Legge il sorgente COBOL dal file
            String cobolSource = readFile(inputFile);
            // Normalizza il sorgente (rimuove numeri di sequenza, commenti, ecc.)
            String normalizedSource = normalizeCobolSource(cobolSource);
            
            // Parsing delle divisioni COBOL
            CobolProgram program = new CobolProgram();
            
            // Analizza la Identification Division
            IdentificationDivisionParser idParser = new IdentificationDivisionParser();
            program.setIdentificationDivision(idParser.parse(normalizedSource));
            
            // Analizza la Environment Division
            EnvironmentDivisionParser envParser = new EnvironmentDivisionParser();
            program.setEnvironmentDivision(envParser.parse(normalizedSource));

            // Analizza la Data Division
            DataDivisionParser dataParser = new DataDivisionParser();
            program.setDataDivision(dataParser.parse(normalizedSource));

            // Analizza la Procedure Division
            ProcedureDivisionParser procParser = new ProcedureDivisionParser();
            program.setProcedureDivision(procParser.parse(normalizedSource));
            
            // Genera il codice Java equivalente
            JavaCodeGenerator generator = new JavaCodeGenerator();
            String javaCode = generator.generate(program);
            
            // Scrive il codice Java su file
            writeFile(outputFile, javaCode);
            
            System.out.println("Successfully converted COBOL to Java");
            System.out.println("Input: " + inputFile);
            System.out.println("Output: " + outputFile);
            
        } catch (IOException e) {
            System.err.println("Error: " + e.getMessage());
        }
    }
    
    /**
     * Legge il contenuto di un file di testo e lo restituisce come stringa.
     * @param filename percorso del file da leggere
     * @return contenuto del file come stringa
     * @throws IOException in caso di errore di lettura
     */
    private static String readFile(String filename) throws IOException {
        StringBuilder content = new StringBuilder();
        try (BufferedReader reader = new BufferedReader(new FileReader(filename))) {
            String line;
            // Legge ogni riga e la aggiunge al buffer
            while ((line = reader.readLine()) != null) {
                content.append(line).append("\n");
            }
        }
        return content.toString();
    }
    
    /**
     * Scrive una stringa su file.
     * @param filename percorso del file di destinazione
     * @param content contenuto da scrivere
     * @throws IOException in caso di errore di scrittura
     */
    private static void writeFile(String filename, String content) throws IOException {
        try (BufferedWriter writer = new BufferedWriter(new FileWriter(filename))) {
            writer.write(content);
        }
    }
    
    /**
     * Normalizza il sorgente COBOL rimuovendo numeri di sequenza, commenti e spazi inutili.
     * <ul>
     *   <li>Salta le righe di commento (con * in colonna 7).</li>
     *   <li>Rimuove i numeri di sequenza (colonne 1-6) e l'area indicatore (colonna 7).</li>
     *   <li>Rimuove gli spazi finali.</li>
     *   <li>Ignora le righe vuote.</li>
     * </ul>
     * @param source sorgente COBOL originale
     * @return sorgente normalizzato pronto per il parsing
     */
    private static String normalizeCobolSource(String source) {
        String[] lines = source.split("\n");
        StringBuilder normalized = new StringBuilder();
        
        for (String line : lines) {
            // Salta le righe di commento (asterisco in colonna 7)
            if (line.length() >= 7 && line.charAt(6) == '*') {
                continue;
            }
            
            // Rimuove numeri di sequenza (colonne 1-6) e area indicatore (colonna 7)
            if (line.length() > 7) {
                line = line.substring(7);
            }
            
            // Rimuove spazi finali
            line = line.replaceAll("\\s+$", "");
            
            // Aggiunge solo righe non vuote
            if (!line.isEmpty()) {
                normalized.append(line).append("\n");
            }
        }
        
        return normalized.toString();
    }
}