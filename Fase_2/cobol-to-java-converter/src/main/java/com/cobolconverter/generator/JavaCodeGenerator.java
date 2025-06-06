package com.cobolconverter.generator;

import com.cobolconverter.model.CobolProgram;
import com.cobolconverter.model.IdentificationDivision;
import com.cobolconverter.model.EnvironmentDivision;
import com.cobolconverter.model.DataDivision;
import com.cobolconverter.model.DataDivision.DataItem;
import com.cobolconverter.model.ProcedureDivision;
import com.cobolconverter.model.ProcedureDivision.*;

/**
 * Generatore di codice Java a partire da un oggetto {@link CobolProgram}.
 * <p>
 * Questa classe si occupa di trasformare la rappresentazione interna di un programma COBOL
 * (ottenuta tramite parsing delle divisioni principali) in una classe Java equivalente.
 * <ul>
 *   <li>Genera la JavaDoc e i commenti di classe a partire dalla Identification Division.</li>
 *   <li>Traduce le sezioni Environment e Data come commenti e campi Java.</li>
 *   <li>Traduce la Procedure Division in metodi Java, uno per ogni paragrafo COBOL.</li>
 *   <li>Gestisce la conversione di statement COBOL in istruzioni Java equivalenti.</li>
 * </ul>
 * Il risultato è una stringa contenente il codice sorgente Java generato.
 */
public class JavaCodeGenerator {
    
    /**
     * Genera il codice Java equivalente a partire da un {@link CobolProgram}.
     * <p>
     * Il metodo produce la dichiarazione di classe, i campi, i metodi e la main method,
     * traducendo le divisioni COBOL in strutture Java.
     * @param program oggetto CobolProgram popolato dal parser
     * @return codice sorgente Java generato come stringa
     */
    public String generate(CobolProgram program) {
        StringBuilder sb = new StringBuilder();
        
        IdentificationDivision idDiv = program.getIdentificationDivision();
        EnvironmentDivision envDiv = program.getEnvironmentDivision();
        DataDivision dataDiv = program.getDataDivision();
        ProcedureDivision procDiv = program.getProcedureDivision();
        
        /***********************************************************************************************
        * IDENTIFICATION DIVISION
        ************************************************************************************************/

        //----------------------------------------------------------------------------------------------

        // JavaDoc di classe generata a partire dai metadati COBOL
        sb.append("/**\n");
        if (idDiv.getAuthor() != null) {
            sb.append(" * @author ").append(idDiv.getAuthor()).append("\n");
        }
        if (idDiv.getDateWritten() != null) {
            sb.append(" * @date ").append(idDiv.getDateWritten()).append("\n");
        }
        if (idDiv.getInstallation() != null) {
            sb.append(" * Installation: ").append(idDiv.getInstallation()).append("\n");
        }
        if (idDiv.getSecurity() != null) {
            sb.append(" * Security: ").append(idDiv.getSecurity()).append("\n");
        }
        if (idDiv.getRemarks() != null) {
            sb.append(" * Remarks: ").append(idDiv.getRemarks()).append("\n");
        }
        sb.append(" * Generated from COBOL program\n");
        sb.append(" */\n");
        
        // Dichiarazione della classe Java
        String className = toCamelCase(idDiv.getProgramId());
        sb.append("public class ").append(className).append(" {\n");
        sb.append("    \n");
        sb.append("    // Program ID: ").append(idDiv.getProgramId()).append("\n");

        //----------------------------------------------------------------------------------------------
        
        /***********************************************************************************************
        * ENVIRONMENT DIVISION
        ************************************************************************************************/    

        //----------------------------------------------------------------------------------------------

        // Inserisce informazioni della Environment Division come commenti
        if (envDiv != null) {
            sb.append(envDiv.toJavaComment());
        }
        
        //----------------------------------------------------------------------------------------------

        /***********************************************************************************************
        * DATA DIVISION
        ************************************************************************************************/

        //----------------------------------------------------------------------------------------------

        // Inserisce informazioni della Data Division e genera i campi Java
        if (dataDiv != null) {
            sb.append(dataDiv.toJavaComment());
            sb.append("    \n");
            
            // Genera i campi Java corrispondenti agli item di Working-Storage
            if (!dataDiv.getWorkingStorageItems().isEmpty()) {
                sb.append("    // Working-Storage Fields\n");
                for (DataItem item : dataDiv.getWorkingStorageItems()) {
                    // Salta le condition names di livello 88
                    if (!"88".equals(item.getLevel())) {
                        sb.append(item.toJavaField()).append("\n");
                    }
                }
                sb.append("    \n");
            }
        }
        //----------------------------------------------------------------------------------------------

        /***********************************************************************************************
        * PROCEDURE DIVISION
        ************************************************************************************************/
        
        //----------------------------------------------------------------------------------------------
        // Inserisce informazioni della Procedure Division e genera i metodi Java
        if (procDiv != null) {
            sb.append(procDiv.toJavaComment());
            sb.append("    \n");
            
            // Genera il metodo main Java che richiama il primo paragrafo COBOL
            sb.append("    public static void main(String[] args) {\n");
            sb.append("        ").append(className).append(" program = new ").append(className).append("();\n");
            
            // Richiama il primo paragrafo (o il primo della prima sezione)
            if (procDiv.hasSections() && !procDiv.getSections().isEmpty()) {
                Section firstSection = procDiv.getSections().get(0);
                if (!firstSection.getParagraphs().isEmpty()) {
                    String firstPara = toJavaMethodName(firstSection.getParagraphs().get(0).getName());
                    sb.append("        program.").append(firstPara).append("();\n");
                }
            } else if (!procDiv.getParagraphs().isEmpty()) {
                String firstPara = toJavaMethodName(procDiv.getParagraphs().get(0).getName());
                sb.append("        program.").append(firstPara).append("();\n");
            }
            
            sb.append("    }\n");
            sb.append("    \n");
            
            // Genera i metodi Java per ogni paragrafo COBOL
            if (procDiv.hasSections()) {
                for (Section section : procDiv.getSections()) {
                    sb.append("    // ").append(section.getName()).append(" SECTION\n");
                    for (Paragraph para : section.getParagraphs()) {
                        generateParagraphMethod(para, sb);
                    }
                }
            } else {
                for (Paragraph para : procDiv.getParagraphs()) {
                    generateParagraphMethod(para, sb);
                }
            }
        } else {

    //----------------------------------------------------------------------------------------------

            // Metodo main di fallback se la Procedure Division non è presente
            sb.append("    public static void main(String[] args) {\n");
            sb.append("        // Main program logic goes here\n");
            sb.append("        System.out.println(\"").append(className).append(" - Generated from COBOL\");\n");
            sb.append("    }\n");
            sb.append("}\n");

        }
    //----------------------------------------------------------------------------------------------

        sb.append("}\n");

        // Restituisce il codice Java generato come stringa
        return sb.toString();
    }
    


    /************************************************************************************************
     * FUNZIONI DI SUPPORTO
     ************************************************************************************************/

    /**
     * Converte un nome COBOL (es. "MY-PROGRAM") in CamelCase per la dichiarazione di classe Java.
     * @param cobolName nome COBOL
     * @return nome Java in CamelCase
     */
    private String toCamelCase(String cobolName) {
        if (cobolName == null) return "UnnamedProgram";
        
        String[] parts = cobolName.split("[-_]");
        StringBuilder result = new StringBuilder();
        
        for (String part : parts) {
            if (!part.isEmpty()) {
                result.append(Character.toUpperCase(part.charAt(0)));
                if (part.length() > 1) {
                    result.append(part.substring(1).toLowerCase());
                }
            }
        }
        
        return result.toString();
    }
    
    /**
     * Converte un nome COBOL in camelCase per la dichiarazione di metodi Java.
     * @param cobolName nome COBOL
     * @return nome Java in camelCase
     */
    private String toJavaMethodName(String cobolName) {
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
    
    /**
     * Converte un nome COBOL in nome variabile Java, gestendo valori speciali COBOL.
     * @param cobolName nome COBOL
     * @return nome variabile Java o valore letterale
     */
    private String toJavaVarName(String cobolName) {
        if (cobolName == null) return "null";
        
        // Gestione di valori speciali COBOL
        if ("SPACES".equalsIgnoreCase(cobolName) || "SPACE".equalsIgnoreCase(cobolName)) {
            return "\"\"";
        }
        if ("ZEROS".equalsIgnoreCase(cobolName) || "ZERO".equalsIgnoreCase(cobolName)) {
            return "0";
        }
        
        // Conversione standard
        return toJavaMethodName(cobolName);
    }

    /**
     * Genera il metodo Java corrispondente a un paragrafo COBOL.
     * @param paragraph oggetto Paragraph da convertire
     * @param sb StringBuilder su cui scrivere il codice
     */
    private void generateParagraphMethod(Paragraph paragraph, StringBuilder sb) {
        String methodName = toJavaMethodName(paragraph.getName());
        
        sb.append("    private void ").append(methodName).append("() {\n");
        sb.append("        // ").append(paragraph.getName()).append("\n");
        
        for (Statement stmt : paragraph.getStatements()) {
            generateStatement(stmt, sb, "        ");
        }
        
        sb.append("    }\n");
        sb.append("    \n");
    }

    /**
     * Genera il codice Java per uno statement COBOL.
     * <p>
     * Gestisce la conversione di MOVE, DISPLAY, PERFORM e altri statement noti.
     * @param stmt oggetto Statement COBOL
     * @param sb StringBuilder su cui scrivere il codice
     * @param indent indentazione da applicare
     */
    private void generateStatement(Statement stmt, StringBuilder sb, String indent) {
        String javaCode = stmt.toJava();
        
        // Conversione specifica per statement noti
        if (stmt.getType() == Statement.StatementType.MOVE) {
            String source = toJavaVarName(stmt.getMainOperand());
            String target = toJavaVarName(stmt.getTargetOperand());
            javaCode = target + " = " + source + ";";
        } else if (stmt.getType() == Statement.StatementType.DISPLAY) {
            String operand = stmt.getMainOperand();
            if (operand != null && !operand.startsWith("\"") && !operand.startsWith("'")) {
                operand = toJavaVarName(operand);
            }
            javaCode = "System.out.println(" + operand + ");";
        } else if (stmt.getType() == Statement.StatementType.PERFORM) {
            String methodCall = toJavaMethodName(stmt.getMainOperand());
            javaCode = methodCall + "();";
        }
        
        sb.append(indent).append(javaCode).append("\n");
        
        // Gestione di eventuali statement annidati (es. IF/EVALUATE)
        for (Statement nested : stmt.getNestedStatements()) {
            generateStatement(nested, sb, indent + "    ");
        }
    }
}