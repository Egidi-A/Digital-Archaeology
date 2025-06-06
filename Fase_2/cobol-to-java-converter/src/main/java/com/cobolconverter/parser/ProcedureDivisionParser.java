package com.cobolconverter.parser;

import com.cobolconverter.model.ProcedureDivision;
import com.cobolconverter.model.ProcedureDivision.*;
import java.util.ArrayList;
import java.util.List;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

public class ProcedureDivisionParser {
    
    public ProcedureDivision parse(String normalizedSource) {
        ProcedureDivision procDiv = new ProcedureDivision();
        
        // Find PROCEDURE DIVISION section
        Pattern procDivPattern = Pattern.compile(
            "PROCEDURE\\s+DIVISION\\s*\\.([^\\n]*(?:\\n(?!\\s*(?:IDENTIFICATION|ENVIRONMENT|DATA)\\s+DIVISION)[^\\n]*)*)",
            Pattern.CASE_INSENSITIVE | Pattern.DOTALL
        );
        
        Matcher procDivMatcher = procDivPattern.matcher(normalizedSource);
        if (!procDivMatcher.find()) {
            return procDiv; // Return empty if not found
        }
        
        String procDivContent = procDivMatcher.group(1);
        
        // Check if it has sections or just paragraphs
        if (hasSection(procDivContent)) {
            parseSections(procDivContent, procDiv);
        } else {
            parseParagraphsOnly(procDivContent, procDiv);
        }
        
        return procDiv;
    }
    
    private boolean hasSection(String content) {
        Pattern sectionPattern = Pattern.compile(
            "\\b\\w+\\s+SECTION\\s*\\.",
            Pattern.CASE_INSENSITIVE
        );
        return sectionPattern.matcher(content).find();
    }
    
    private void parseSections(String content, ProcedureDivision procDiv) {
        // Pattern to match sections
        Pattern sectionPattern = Pattern.compile(
            "(\\w+)\\s+SECTION\\s*\\.([^\\n]*(?:\\n(?!\\s*\\w+\\s+SECTION)[^\\n]*)*)",
            Pattern.CASE_INSENSITIVE | Pattern.DOTALL
        );
        
        Matcher sectionMatcher = sectionPattern.matcher(content);
        
        while (sectionMatcher.find()) {
            String sectionName = sectionMatcher.group(1);
            String sectionContent = sectionMatcher.group(2);
            
            Section section = new Section(sectionName);
            parseParagraphsInSection(sectionContent, section);
            procDiv.addSection(section);
        }
    }
    
    private void parseParagraphsInSection(String content, Section section) {
        List<Paragraph> paragraphs = extractParagraphs(content);
        for (Paragraph para : paragraphs) {
            section.addParagraph(para);
        }
    }
    
    private void parseParagraphsOnly(String content, ProcedureDivision procDiv) {
        List<Paragraph> paragraphs = extractParagraphs(content);
        for (Paragraph para : paragraphs) {
            procDiv.addParagraph(para);
        }
    }
    
    private List<Paragraph> extractParagraphs(String content) {
        List<Paragraph> paragraphs = new ArrayList<>();
        
        // Pattern to match paragraph names (labels)
        Pattern paragraphPattern = Pattern.compile(
            "^\\s*(\\w+(?:-\\w+)*)\\s*\\.\\s*$",
            Pattern.MULTILINE | Pattern.CASE_INSENSITIVE
        );
        
        Matcher matcher = paragraphPattern.matcher(content);
        
        int lastEnd = 0;
        String lastName = null;
        
        while (matcher.find()) {
            // Process previous paragraph if exists
            if (lastName != null) {
                String paraContent = content.substring(lastEnd, matcher.start()).trim();
                if (!paraContent.isEmpty()) {
                    Paragraph para = new Paragraph(lastName);
                    parseStatements(paraContent, para);
                    paragraphs.add(para);
                }
            }
            
            lastName = matcher.group(1);
            lastEnd = matcher.end();
        }
        
        // Process last paragraph
        if (lastName != null) {
            String paraContent = content.substring(lastEnd).trim();
            if (!paraContent.isEmpty()) {
                Paragraph para = new Paragraph(lastName);
                parseStatements(paraContent, para);
                paragraphs.add(para);
            }
        }
        
        // If no paragraphs found, treat entire content as unnamed paragraph
        if (paragraphs.isEmpty() && !content.trim().isEmpty()) {
            Paragraph para = new Paragraph("MAIN");
            parseStatements(content, para);
            paragraphs.add(para);
        }
        
        return paragraphs;
    }
    
    private void parseStatements(String content, Paragraph paragraph) {
        // Split by periods but be careful with literals
        String[] lines = content.split("\\n");
        StringBuilder currentStatement = new StringBuilder();
        boolean inQuotes = false;
        
        for (String line : lines) {
            line = line.trim();
            if (line.isEmpty()) continue;
            
            // Check for quotes
            for (char c : line.toCharArray()) {
                if (c == '"' || c == '\'') {
                    inQuotes = !inQuotes;
                }
            }
            
            currentStatement.append(line).append(" ");
            
            // If line ends with period and not in quotes, it's end of statement
            if (line.endsWith(".") && !inQuotes) {
                String stmtText = currentStatement.toString().trim();
                if (!stmtText.isEmpty()) {
                    Statement stmt = parseStatement(stmtText);
                    if (stmt != null) {
                        paragraph.addStatement(stmt);
                    }
                }
                currentStatement = new StringBuilder();
            }
        }
        
        // Handle any remaining content
        String remaining = currentStatement.toString().trim();
        if (!remaining.isEmpty()) {
            Statement stmt = parseStatement(remaining);
            if (stmt != null) {
                paragraph.addStatement(stmt);
            }
        }
    }
    
    private Statement parseStatement(String statementText) {
        if (statementText.isEmpty()) return null;
        
        // Remove trailing period
        statementText = statementText.replaceAll("\\.$", "").trim();
        
        // Determine statement type
        Statement.StatementType type = identifyStatementType(statementText);
        Statement stmt = new Statement(type, statementText);
        
        // Parse operands based on type
        switch (type) {
            case MOVE:
                parseMoveStatement(statementText, stmt);
                break;
            case DISPLAY:
                parseDisplayStatement(statementText, stmt);
                break;
            case ACCEPT:
                parseAcceptStatement(statementText, stmt);
                break;
            case PERFORM:
                parsePerformStatement(statementText, stmt);
                break;
            case COMPUTE:
                parseComputeStatement(statementText, stmt);
                break;
            // Add more as needed
        }
        
        return stmt;
    }
    
    private Statement.StatementType identifyStatementType(String text) {
        String upperText = text.toUpperCase();
        
        if (upperText.startsWith("MOVE")) return Statement.StatementType.MOVE;
        if (upperText.startsWith("DISPLAY")) return Statement.StatementType.DISPLAY;
        if (upperText.startsWith("ACCEPT")) return Statement.StatementType.ACCEPT;
        if (upperText.startsWith("PERFORM")) return Statement.StatementType.PERFORM;
        if (upperText.startsWith("COMPUTE")) return Statement.StatementType.COMPUTE;
        if (upperText.startsWith("IF")) return Statement.StatementType.IF;
        if (upperText.startsWith("EVALUATE")) return Statement.StatementType.EVALUATE;
        if (upperText.startsWith("ADD")) return Statement.StatementType.ADD;
        if (upperText.startsWith("SUBTRACT")) return Statement.StatementType.SUBTRACT;
        if (upperText.startsWith("MULTIPLY")) return Statement.StatementType.MULTIPLY;
        if (upperText.startsWith("DIVIDE")) return Statement.StatementType.DIVIDE;
        if (upperText.startsWith("OPEN")) return Statement.StatementType.OPEN;
        if (upperText.startsWith("CLOSE")) return Statement.StatementType.CLOSE;
        if (upperText.startsWith("READ")) return Statement.StatementType.READ;
        if (upperText.startsWith("WRITE")) return Statement.StatementType.WRITE;
        if (upperText.startsWith("EXIT")) return Statement.StatementType.EXIT;
        if (upperText.startsWith("STOP")) return Statement.StatementType.STOP;
        if (upperText.startsWith("GOBACK")) return Statement.StatementType.GOBACK;
        
        return Statement.StatementType.UNKNOWN;
    }
    
    private void parseMoveStatement(String text, Statement stmt) {
        // Pattern: MOVE source TO target
        Pattern movePattern = Pattern.compile(
            "MOVE\\s+(.+?)\\s+TO\\s+(.+)",
            Pattern.CASE_INSENSITIVE
        );
        
        Matcher matcher = movePattern.matcher(text);
        if (matcher.find()) {
            stmt.setMainOperand(cleanOperand(matcher.group(1)));
            stmt.setTargetOperand(cleanOperand(matcher.group(2)));
        }
    }
    
    private void parseDisplayStatement(String text, Statement stmt) {
        // Pattern: DISPLAY value
        Pattern displayPattern = Pattern.compile(
            "DISPLAY\\s+(.+)",
            Pattern.CASE_INSENSITIVE
        );
        
        Matcher matcher = displayPattern.matcher(text);
        if (matcher.find()) {
            stmt.setMainOperand(cleanOperand(matcher.group(1)));
        }
    }
    
    private void parseAcceptStatement(String text, Statement stmt) {
        // Pattern: ACCEPT variable [FROM source]
        Pattern acceptPattern = Pattern.compile(
            "ACCEPT\\s+(\\S+)(?:\\s+FROM\\s+(.+))?",
            Pattern.CASE_INSENSITIVE
        );
        
        Matcher matcher = acceptPattern.matcher(text);
        if (matcher.find()) {
            stmt.setTargetOperand(cleanOperand(matcher.group(1)));
            if (matcher.group(2) != null) {
                stmt.setMainOperand(cleanOperand(matcher.group(2)));
            }
        }
    }
    
    private void parsePerformStatement(String text, Statement stmt) {
        // Pattern: PERFORM paragraph-name [THRU paragraph-name] [TIMES/UNTIL/VARYING]
        Pattern performPattern = Pattern.compile(
            "PERFORM\\s+(\\S+)(?:\\s+THRU\\s+(\\S+))?",
            Pattern.CASE_INSENSITIVE
        );
        
        Matcher matcher = performPattern.matcher(text);
        if (matcher.find()) {
            stmt.setMainOperand(cleanOperand(matcher.group(1)));
            if (matcher.group(2) != null) {
                stmt.setTargetOperand(cleanOperand(matcher.group(2)));
            }
        }
    }
    
    private void parseComputeStatement(String text, Statement stmt) {
        // Pattern: COMPUTE target = expression
        Pattern computePattern = Pattern.compile(
            "COMPUTE\\s+(\\S+)\\s*=\\s*(.+)",
            Pattern.CASE_INSENSITIVE
        );
        
        Matcher matcher = computePattern.matcher(text);
        if (matcher.find()) {
            stmt.setTargetOperand(cleanOperand(matcher.group(1)));
            stmt.setMainOperand(cleanOperand(matcher.group(2)));
        }
    }
    
    private String cleanOperand(String operand) {
        if (operand == null) return null;
        
        // Remove quotes if it's a literal
        operand = operand.trim();
        if ((operand.startsWith("\"") && operand.endsWith("\"")) ||
            (operand.startsWith("'") && operand.endsWith("'"))) {
            return operand;  // Keep quotes for literals
        }
        
        // Clean up variable names
        return operand.replaceAll("\\s+", " ").trim();
    }
}