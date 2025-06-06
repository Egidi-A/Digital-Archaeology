package com.cobolconverter.model;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

/**
 * Rappresenta la PROCEDURE DIVISION di un programma COBOL.
 * <p>
 * Questa classe modella la logica procedurale del programma COBOL, suddivisa in:
 * <ul>
 *   <li><b>Sections</b>: blocchi logici opzionali che raggruppano paragrafi.</li>
 *   <li><b>Paragraphs</b>: unità di codice eseguibile, contenenti una o più istruzioni.</li>
 *   <li><b>Statements</b>: singole istruzioni COBOL (MOVE, DISPLAY, IF, ecc.).</li>
 * </ul>
 * Ogni sezione può contenere più paragrafi, e ogni paragrafo più istruzioni.
 */
public class ProcedureDivision {
    /** Lista delle sezioni presenti nella Procedure Division. */
    private List<Section> sections = new ArrayList<>();
    /** Lista dei paragrafi presenti (se non sono usate le sezioni). */
    private List<Paragraph> paragraphs = new ArrayList<>();
    /** Indica se la Procedure Division contiene almeno una sezione. */
    private boolean hasSections;
    
    /**
     * Rappresenta una SECTION COBOL, che può contenere più paragrafi.
     */
    public static class Section {
        private String name;
        private List<Paragraph> paragraphs = new ArrayList<>();
        
        /**
         * Costruttore della Section.
         * @param name nome della sezione
         */
        public Section(String name) {
            this.name = name;
        }
        
        /** @return nome della sezione */
        public String getName() { return name; }
        /** @return lista dei paragrafi della sezione */
        public List<Paragraph> getParagraphs() { return paragraphs; }
        /** Aggiunge un paragrafo alla sezione */
        public void addParagraph(Paragraph paragraph) { paragraphs.add(paragraph); }
    }
    
    /**
     * Rappresenta un PARAGRAPH COBOL, contenente una lista di istruzioni.
     */
    public static class Paragraph {
        private String name;
        private List<Statement> statements = new ArrayList<>();
        
        /**
         * Costruttore del Paragraph.
         * @param name nome del paragrafo
         */
        public Paragraph(String name) {
            this.name = name;
        }
        
        /** @return nome del paragrafo */
        public String getName() { return name; }
        /** @return lista delle istruzioni del paragrafo */
        public List<Statement> getStatements() { return statements; }
        /** Aggiunge una istruzione al paragrafo */
        public void addStatement(Statement statement) { statements.add(statement); }
    }
    
    /**
     * Rappresenta una singola istruzione COBOL (statement).
     * <p>
     * Ogni Statement ha un tipo, il testo originale, eventuali operandi e attributi aggiuntivi.
     */
    public static class Statement {
        /** Tipo di istruzione COBOL (MOVE, DISPLAY, IF, ecc.). */
        private StatementType type;
        /** Testo COBOL originale dell'istruzione. */
        private String rawText;
        /** Operando principale (es. valore sorgente o condizione). */
        private String mainOperand = "Valore default mainOperand per evitare NullPointerException";
        /** Operando di destinazione (es. variabile target). */
        private String targetOperand;
        /** Eventuali istruzioni annidate (per IF, PERFORM INLINE, ecc.). */
        private List<Statement> nestedStatements = new ArrayList<>();
        /** Attributi aggiuntivi (es. UNTIL, TIMES, NO_ADVANCING, ecc.). */
        private Map<String, String> attributes = new HashMap<>();
        
        /**
         * Enum che rappresenta i principali tipi di istruzioni COBOL.
         */
        public enum StatementType {
            MOVE, COMPUTE, DISPLAY, ACCEPT, PERFORM, IF, EVALUATE, 
            ADD, SUBTRACT, MULTIPLY, DIVIDE, OPEN, CLOSE, READ, 
            WRITE, EXIT, STOP, GOBACK, STRING, UNSTRING,
            EXEC_SQL, ELSE, END_IF, WHEN, END_EVALUATE, UNKNOWN
        }
        
        /**
         * Costruttore di Statement.
         * @param type tipo di istruzione
         * @param rawText testo COBOL originale
         */
        public Statement(StatementType type, String rawText) {
            this.type = type;
            this.rawText = rawText;
        }
        
        /** @return tipo di istruzione */
        public StatementType getType() { return type; }
        /** @return testo COBOL originale */
        public String getRawText() { return rawText; }
        /** @return operando principale */
        public String getMainOperand() { return mainOperand; }
        /** Imposta l'operando principale */
        public void setMainOperand(String mainOperand) { this.mainOperand = mainOperand; }
        /** @return operando di destinazione */
        public String getTargetOperand() { return targetOperand; }
        /** Imposta l'operando di destinazione */
        public void setTargetOperand(String targetOperand) { this.targetOperand = targetOperand; }
        /** @return lista di istruzioni annidate */
        public List<Statement> getNestedStatements() { return nestedStatements; }
        /** Aggiunge una istruzione annidata */
        public void addNestedStatement(Statement statement) { nestedStatements.add(statement); }
        
        /** Aggiunge un attributo (es. UNTIL, TIMES, ecc.) */
        public void addAttribute(String key, String value) { attributes.put(key, value); }
        /** Restituisce il valore di un attributo */
        public String getAttribute(String key) { return attributes.get(key); }
        /** Verifica se un attributo è presente */
        public boolean hasAttribute(String key) { return attributes.containsKey(key); }
        
        /**
         * Genera una rappresentazione Java equivalente dell'istruzione COBOL.
         * @return codice Java equivalente come stringa
         */
        public String toJava() {
            switch (type) {
                case MOVE:
                    return targetOperand + " = " + mainOperand + ";";
                case DISPLAY:
                    String displayCode = "System.out.print";
                    if (!hasAttribute("NO_ADVANCING")) {
                        displayCode += "ln";
                    }
                    return displayCode + "(" + formatDisplayOperand(mainOperand) + ");";
                case ACCEPT:
                    return targetOperand + " = scanner.nextLine();";
                case COMPUTE:
                    return targetOperand + " = " + mainOperand + ";";
                case PERFORM:
                    if (hasAttribute("UNTIL")) {
                        return "while (!(" + convertCondition(getAttribute("UNTIL")) + ")) {";
                    } else if (hasAttribute("TIMES")) {
                        return "for (int i = 0; i < " + getAttribute("TIMES") + "; i++) {";
                    } else if (mainOperand != null && !"INLINE".equals(mainOperand)) {
                        return toJavaName(mainOperand) + "();";
                    } else {
                        return "// PERFORM " + rawText;
                    }
                case IF:
                    return "if (" + convertCondition(mainOperand) + ") {";
                case ELSE:
                    return "} else {";
                case END_IF:
                    return "}";
                case EVALUATE:
                    return "switch (" + mainOperand + ") {";
                case WHEN:
                    return "case " + mainOperand + ":";
                case END_EVALUATE:
                    return "}";
                case STRING:
                    return buildStringStatement();
                case EXEC_SQL:
                    return "// SQL: " + mainOperand.replaceAll("\\n", "\n// ");
                case STOP:
                    if (rawText.toUpperCase().contains("STOP RUN")) {
                        return "System.exit(0);";
                    }
                    return "// " + rawText;
                case GOBACK:
                case EXIT:
                    return "return;";
                default:
                    return "// TODO: " + type + " - " + rawText;
            }
        }
        
        /**
         * Formatta l'operando per la DISPLAY in stile Java.
         * Gestisce concatenazioni e valori speciali COBOL.
         * @param operand operando COBOL
         * @return stringa Java formattata
         */
        private String formatDisplayOperand(String operand) {
            if (operand == null) return "\"\"";
            
            // Gestisce più item separati da spazi (es. DISPLAY A B C)
            String[] parts = operand.split("\\s+(?=(?:[^\"]*\"[^\"]*\")*[^\"]*$)");
            
            if (parts.length > 1) {
                StringBuilder result = new StringBuilder();
                for (int i = 0; i < parts.length; i++) {
                    if (i > 0) result.append(" + \" \" + ");
                    result.append(formatSingleOperand(parts[i]));
                }
                return result.toString();
            }
            
            return formatSingleOperand(operand);
        }
        
        /**
         * Formatta un singolo operando per la DISPLAY.
         * @param operand operando COBOL
         * @return stringa Java
         */
        private String formatSingleOperand(String operand) {
            operand = operand.trim();
            
            // Già tra virgolette
            if ((operand.startsWith("\"") && operand.endsWith("\"")) ||
                (operand.startsWith("'") && operand.endsWith("'"))) {
                return operand;
            }
            
            // Valori speciali COBOL
            if ("SPACES".equalsIgnoreCase(operand) || "SPACE".equalsIgnoreCase(operand)) {
                return "\" \"";
            }
            if ("ZEROS".equalsIgnoreCase(operand) || "ZERO".equalsIgnoreCase(operand)) {
                return "\"0\"";
            }
            
            // Nome variabile
            return toJavaName(operand);
        }
        
        /**
         * Converte una condizione COBOL in sintassi Java.
         * @param condition condizione COBOL
         * @return condizione Java
         */
        private String convertCondition(String condition) {
            if (condition == null) return "true";
            
            // Conversioni semplici
            condition = condition.replaceAll("\\bNOT\\s*=\\b", "!=");
            condition = condition.replaceAll("\\b=\\b", "==");
            condition = condition.replaceAll("\\bAND\\b", "&&");
            condition = condition.replaceAll("\\bOR\\b", "||");
            condition = condition.replaceAll("\\bNOT\\b", "!");
            
            return condition;
        }
        
        /**
         * Genera codice Java per la STRING COBOL.
         * @return codice Java equivalente
         */
        private String buildStringStatement() {
            if (targetOperand == null) return "// STRING statement incomplete";
            
            return "StringBuilder sb = new StringBuilder();\n" +
                "// STRING components: " + mainOperand + "\n" +
                targetOperand + " = sb.toString();";
        }
        
        /**
         * Converte un nome COBOL in camelCase per Java.
         * @param cobolName nome COBOL
         * @return nome Java
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

    /** Restituisce la lista delle sezioni. */
    public List<Section> getSections() { return sections; }
    /** Aggiunge una sezione alla Procedure Division. */
    public void addSection(Section section) { 
        sections.add(section);
        hasSections = true;
    }
    
    /** Restituisce la lista dei paragrafi (se non sono usate le sezioni). */
    public List<Paragraph> getParagraphs() { return paragraphs; }
    /** Aggiunge un paragrafo alla Procedure Division. */
    public void addParagraph(Paragraph paragraph) { paragraphs.add(paragraph); }
    
    /** Indica se la Procedure Division contiene almeno una sezione. */
    public boolean hasSections() { return hasSections; }
    
    /**
     * Restituisce un commento riassuntivo della Procedure Division,
     * utile per la generazione di codice o documentazione.
     * @return commento Java con le sezioni e i paragrafi presenti
     */
    public String toJavaComment() {
        StringBuilder sb = new StringBuilder();
        sb.append("    // Procedure Division\n");
        if (hasSections) {
            sb.append("    // Sections: ").append(sections.size()).append("\n");
            int totalParas = sections.stream()
                .mapToInt(s -> s.getParagraphs().size())
                .sum();
            sb.append("    // Total Paragraphs: ").append(totalParas).append("\n");
        } else {
            sb.append("    // Paragraphs: ").append(paragraphs.size()).append("\n");
        }
        return sb.toString();
    }
}