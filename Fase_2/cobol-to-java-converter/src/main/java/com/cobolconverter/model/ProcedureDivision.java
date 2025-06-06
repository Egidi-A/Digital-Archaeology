package com.cobolconverter.model;

import java.util.ArrayList;
import java.util.List;

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
        private String mainOperand;
        private String targetOperand;
        private List<Statement> nestedStatements = new ArrayList<>();
        
        public enum StatementType {
            MOVE, COMPUTE, DISPLAY, ACCEPT, PERFORM, IF, EVALUATE, 
            ADD, SUBTRACT, MULTIPLY, DIVIDE, OPEN, CLOSE, READ, 
            WRITE, EXIT, STOP, GOBACK, UNKNOWN
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
        
        // Generate basic Java equivalent
        public String toJava() {
            switch (type) {
                case MOVE:
                    return targetOperand + " = " + mainOperand + ";";
                case DISPLAY:
                    return "System.out.println(" + mainOperand + ");";
                case ACCEPT:
                    return targetOperand + " = scanner.nextLine();";
                case COMPUTE:
                    // For now, just return the raw compute expression
                    return "// COMPUTE " + rawText;
                case PERFORM:
                    return toJavaName(mainOperand) + "();";
                case STOP:
                    return "System.exit(0);";
                case GOBACK:
                case EXIT:
                    return "return;";
                default:
                    return "// TODO: " + type + " - " + rawText;
            }
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