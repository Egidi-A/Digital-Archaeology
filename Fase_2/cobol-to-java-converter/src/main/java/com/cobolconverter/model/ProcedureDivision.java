package com.cobolconverter.model;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

public class ProcedureDivision {
    private List<Section> sections = new ArrayList<>();
    private List<Paragraph> paragraphs = new ArrayList<>();
    private boolean hasSections;
    
    // Inner class for sections
    public static class Section {
        private String name;
        private List<Paragraph> paragraphs = new ArrayList<>();
        
        public Section(String name) {
            this.name = name;
        }
        
        public String getName() { return name; }
        public List<Paragraph> getParagraphs() { return paragraphs; }
        public void addParagraph(Paragraph paragraph) { paragraphs.add(paragraph); }
    }
    
    // Inner class for paragraphs
    public static class Paragraph {
        private String name;
        private List<Statement> statements = new ArrayList<>();
        
        public Paragraph(String name) {
            this.name = name;
        }
        
        public String getName() { return name; }
        public List<Statement> getStatements() { return statements; }
        public void addStatement(Statement statement) { statements.add(statement); }
    }
    
    // Inner class for statements
    public static class Statement {
        private StatementType type;
        private String rawText;
        private String mainOperand = "Valore default mainOperand per evitare NullPointerException"; // Default value for mainOperand da cambiare, solo per evitare NullPointerException
        private String targetOperand;
        private List<Statement> nestedStatements = new ArrayList<>();
        private Map<String, String> attributes = new HashMap<>();
        
        public enum StatementType {
            MOVE, COMPUTE, DISPLAY, ACCEPT, PERFORM, IF, EVALUATE, 
            ADD, SUBTRACT, MULTIPLY, DIVIDE, OPEN, CLOSE, READ, 
            WRITE, EXIT, STOP, GOBACK, STRING, UNSTRING,
            EXEC_SQL, ELSE, END_IF, WHEN, END_EVALUATE, UNKNOWN
        }
        
        public Statement(StatementType type, String rawText) {
            this.type = type;
            this.rawText = rawText;
        }
        
        // Getters and setters
        public StatementType getType() { return type; }
        public String getRawText() { return rawText; }
        public String getMainOperand() { return mainOperand; }
        public void setMainOperand(String mainOperand) { this.mainOperand = mainOperand; }
        public String getTargetOperand() { return targetOperand; }
        public void setTargetOperand(String targetOperand) { this.targetOperand = targetOperand; }
        public List<Statement> getNestedStatements() { return nestedStatements; }
        public void addNestedStatement(Statement statement) { nestedStatements.add(statement); }
        
        // New methods for attributes
        public void addAttribute(String key, String value) { attributes.put(key, value); }
        public String getAttribute(String key) { return attributes.get(key); }
        public boolean hasAttribute(String key) { return attributes.containsKey(key); }
        
        // Generate basic Java equivalent
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
        
        private String formatDisplayOperand(String operand) {
            if (operand == null) return "\"\"";
            
            // Handle multiple display items separated by spaces
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
        
        private String formatSingleOperand(String operand) {
            operand = operand.trim();
            
            // Already quoted
            if ((operand.startsWith("\"") && operand.endsWith("\"")) ||
                (operand.startsWith("'") && operand.endsWith("'"))) {
                return operand;
            }
            
            // COBOL special values
            if ("SPACES".equalsIgnoreCase(operand) || "SPACE".equalsIgnoreCase(operand)) {
                return "\" \"";
            }
            if ("ZEROS".equalsIgnoreCase(operand) || "ZERO".equalsIgnoreCase(operand)) {
                return "\"0\"";
            }
            
            // Variable name
            return toJavaName(operand);
        }
        
        private String convertCondition(String condition) {
            if (condition == null) return "true";
            
            // Simple conversions
            condition = condition.replaceAll("\\bNOT\\s*=\\b", "!=");
            condition = condition.replaceAll("\\b=\\b", "==");
            condition = condition.replaceAll("\\bAND\\b", "&&");
            condition = condition.replaceAll("\\bOR\\b", "||");
            condition = condition.replaceAll("\\bNOT\\b", "!");
            
            return condition;
        }
        
        private String buildStringStatement() {
            if (targetOperand == null) return "// STRING statement incomplete";
            
            return "StringBuilder sb = new StringBuilder();\n" +
                "// STRING components: " + mainOperand + "\n" +
                targetOperand + " = sb.toString();";
        }
        
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

    // Getters and setters
    public List<Section> getSections() { return sections; }
    public void addSection(Section section) { 
        sections.add(section);
        hasSections = true;
    }
    
    public List<Paragraph> getParagraphs() { return paragraphs; }
    public void addParagraph(Paragraph paragraph) { paragraphs.add(paragraph); }
    
    public boolean hasSections() { return hasSections; }
    
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