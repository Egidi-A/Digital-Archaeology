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
            return procDiv;
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
        // First, handle EXEC SQL blocks as single units
        content = preprocessExecSqlBlocks(content);
        
        // Split into logical lines (handling continuations)
        List<String> logicalLines = splitIntoLogicalLines(content);
        
        // Process each logical line
        for (String logicalLine : logicalLines) {
            List<Statement> statements = parseLogicalLine(logicalLine);
            for (Statement stmt : statements) {
                if (stmt != null) {
                    paragraph.addStatement(stmt);
                }
            }
        }
    }
    
    private String preprocessExecSqlBlocks(String content) {
        // Replace EXEC SQL...END-EXEC blocks with placeholders to prevent splitting
        Pattern sqlPattern = Pattern.compile(
            "EXEC\\s+SQL\\s+(.+?)\\s+END-EXEC",
            Pattern.CASE_INSENSITIVE | Pattern.DOTALL
        );
        
        StringBuffer sb = new StringBuffer();
        Matcher matcher = sqlPattern.matcher(content);
        int sqlBlockCount = 0;
        
        while (matcher.find()) {
            String sqlContent = matcher.group(1);
            // Replace newlines with special marker to preserve them
            sqlContent = sqlContent.replaceAll("\\n", "~NEWLINE~");
            String placeholder = "~EXECSQL~" + sqlBlockCount + "~" + sqlContent + "~ENDEXECSQL~";
            matcher.appendReplacement(sb, placeholder);
            sqlBlockCount++;
        }
        matcher.appendTail(sb);
        
        return sb.toString();
    }
    
    private List<String> splitIntoLogicalLines(String content) {
        List<String> logicalLines = new ArrayList<>();
        String[] physicalLines = content.split("\\n");
        StringBuilder currentLogicalLine = new StringBuilder();
        
        for (String line : physicalLines) {
            line = line.trim();
            if (line.isEmpty()) continue;
            
            // Check if this line is a continuation
            if (currentLogicalLine.length() > 0) {
                currentLogicalLine.append(" ");
            }
            currentLogicalLine.append(line);
            
            // Check if we've completed a logical line (ends with period not in quotes)
            if (endsWithPeriod(line)) {
                logicalLines.add(currentLogicalLine.toString());
                currentLogicalLine = new StringBuilder();
            }
        }
        
        // Add any remaining content
        if (currentLogicalLine.length() > 0) {
            logicalLines.add(currentLogicalLine.toString());
        }
        
        return logicalLines;
    }
    
    private boolean endsWithPeriod(String line) {
        if (line.isEmpty()) return false;
        
        // Count quotes to see if we're in a string literal
        int quoteCount = 0;
        for (char c : line.toCharArray()) {
            if (c == '"' || c == '\'') quoteCount++;
        }
        
        // If odd number of quotes, we're inside a string
        if (quoteCount % 2 == 1) return false;
        
        // Check if line ends with period
        return line.matches(".*\\.\\s*$");
    }
    
    private List<Statement> parseLogicalLine(String logicalLine) {
        List<Statement> statements = new ArrayList<>();
        
        // Handle multiple statements on one line separated by specific keywords
        List<String> statementTexts = splitByStatementKeywords(logicalLine);
        
        for (String stmtText : statementTexts) {
            stmtText = stmtText.trim();
            if (!stmtText.isEmpty()) {
                Statement stmt = parseStatement(stmtText);
                if (stmt != null) {
                    statements.add(stmt);
                }
            }
        }
        
        return statements;
    }
    
    private List<String> splitByStatementKeywords(String line) {
        List<String> statements = new ArrayList<>();
        
        // Keywords that start new statements
        String[] keywords = {
            "DISPLAY", "MOVE", "ACCEPT", "PERFORM", "IF", "ELSE", 
            "END-IF", "EVALUATE", "WHEN", "END-EVALUATE", "COMPUTE",
            "ADD", "SUBTRACT", "MULTIPLY", "DIVIDE", "STRING", "UNSTRING",
            "OPEN", "CLOSE", "READ", "WRITE", "EXIT", "STOP", "GOBACK"
        };
        
        // Build pattern for statement keywords
        StringBuilder patternBuilder = new StringBuilder("\\b(");
        for (int i = 0; i < keywords.length; i++) {
            if (i > 0) patternBuilder.append("|");
            patternBuilder.append(keywords[i]);
        }
        patternBuilder.append(")\\b");
        
        Pattern keywordPattern = Pattern.compile(patternBuilder.toString(), Pattern.CASE_INSENSITIVE);
        Matcher matcher = keywordPattern.matcher(line);
        
        int lastEnd = 0;
        while (matcher.find()) {
            if (matcher.start() > lastEnd) {
                // Add any content before this keyword to previous statement
                if (statements.size() > 0 && lastEnd < matcher.start()) {
                    String lastStmt = statements.get(statements.size() - 1);
                    statements.set(statements.size() - 1, 
                        lastStmt + " " + line.substring(lastEnd, matcher.start()).trim());
                }
            }
            lastEnd = matcher.start();
        }
        
        // Add the last part
        if (lastEnd < line.length()) {
            statements.add(line.substring(lastEnd).trim());
        }
        
        // If no keywords found, treat entire line as one statement
        if (statements.isEmpty()) {
            statements.add(line);
        }
        
        return statements;
    }
    
    private Statement parseStatement(String statementText) {
        if (statementText.isEmpty()) return null;
        
        // Remove trailing period
        statementText = statementText.replaceAll("\\.$", "").trim();
        
        // Restore EXEC SQL blocks
        statementText = restoreExecSqlBlocks(statementText);
        
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
            case IF:
                parseIfStatement(statementText, stmt);
                break;
            case EVALUATE:
                parseEvaluateStatement(statementText, stmt);
                break;
            case STRING:
                parseStringStatement(statementText, stmt);
                break;
            case EXEC_SQL:
                parseExecSqlStatement(statementText, stmt);
                break;
            // Add more as needed
        }
        
        return stmt;
    }
    
    private String restoreExecSqlBlocks(String text) {
        // Restore EXEC SQL blocks from placeholders
        Pattern placeholderPattern = Pattern.compile("~EXECSQL~\\d+~(.+?)~ENDEXECSQL~");
        Matcher matcher = placeholderPattern.matcher(text);
        
        StringBuffer sb = new StringBuffer();
        while (matcher.find()) {
            String sqlContent = matcher.group(1);
            // Restore newlines
            sqlContent = sqlContent.replaceAll("~NEWLINE~", "\n");
            String replacement = "EXEC SQL " + sqlContent + " END-EXEC";
            matcher.appendReplacement(sb, replacement);
        }
        matcher.appendTail(sb);
        
        return sb.toString();
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
        if (upperText.startsWith("STRING")) return Statement.StatementType.STRING;
        if (upperText.startsWith("EXEC SQL") || upperText.contains("~EXECSQL~")) 
            return Statement.StatementType.EXEC_SQL;
        if (upperText.startsWith("OPEN")) return Statement.StatementType.OPEN;
        if (upperText.startsWith("CLOSE")) return Statement.StatementType.CLOSE;
        if (upperText.startsWith("READ")) return Statement.StatementType.READ;
        if (upperText.startsWith("WRITE")) return Statement.StatementType.WRITE;
        if (upperText.startsWith("EXIT")) return Statement.StatementType.EXIT;
        if (upperText.startsWith("STOP")) return Statement.StatementType.STOP;
        if (upperText.startsWith("GOBACK")) return Statement.StatementType.GOBACK;
        if (upperText.startsWith("ELSE")) return Statement.StatementType.ELSE;
        if (upperText.startsWith("END-IF")) return Statement.StatementType.END_IF;
        if (upperText.startsWith("WHEN")) return Statement.StatementType.WHEN;
        if (upperText.startsWith("END-EVALUATE")) return Statement.StatementType.END_EVALUATE;
        
        return Statement.StatementType.UNKNOWN;
    }
    
    private void parseMoveStatement(String text, Statement stmt) {
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
        // Handle DISPLAY with multiple values and WITH NO ADVANCING
        Pattern displayPattern = Pattern.compile(
            "DISPLAY\\s+(.+?)(?:\\s+WITH\\s+NO\\s+ADVANCING)?$",
            Pattern.CASE_INSENSITIVE
        );
        
        Matcher matcher = displayPattern.matcher(text);
        if (matcher.find()) {
            String displayContent = matcher.group(1);
            stmt.setMainOperand(displayContent.trim());
            
            // Check for WITH NO ADVANCING
            if (text.toUpperCase().contains("WITH NO ADVANCING")) {
                stmt.addAttribute("NO_ADVANCING", "true");
            }
        }
    }
    
    private void parseAcceptStatement(String text, Statement stmt) {
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
        // First check for PERFORM UNTIL (without paragraph name)
        Pattern untilPattern = Pattern.compile(
            "PERFORM\\s+UNTIL\\s+(.+)",
            Pattern.CASE_INSENSITIVE
        );
        
        Matcher untilMatcher = untilPattern.matcher(text);
        if (untilMatcher.find()) {
            stmt.addAttribute("UNTIL", untilMatcher.group(1).trim());
            stmt.setMainOperand("INLINE"); // Mark as inline PERFORM
            return;
        }
        
        // Check for PERFORM n TIMES
        Pattern timesPattern = Pattern.compile(
            "PERFORM\\s+(.+)\\s+TIMES",
            Pattern.CASE_INSENSITIVE
        );
        
        Matcher timesMatcher = timesPattern.matcher(text);
        if (timesMatcher.find()) {
            stmt.addAttribute("TIMES", timesMatcher.group(1).trim());
            stmt.setMainOperand("INLINE"); // Mark as inline PERFORM
            return;
        }
        
        // Standard PERFORM paragraph-name [THRU paragraph-name]
        Pattern standardPattern = Pattern.compile(
            "PERFORM\\s+(\\S+)(?:\\s+THRU\\s+(\\S+))?",
            Pattern.CASE_INSENSITIVE
        );
        
        Matcher standardMatcher = standardPattern.matcher(text);
        if (standardMatcher.find()) {
            stmt.setMainOperand(cleanOperand(standardMatcher.group(1)));
            if (standardMatcher.group(2) != null) {
                stmt.setTargetOperand(cleanOperand(standardMatcher.group(2)));
            }
        }
    }
    
    private void parseComputeStatement(String text, Statement stmt) {
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
    
    private void parseIfStatement(String text, Statement stmt) {
        // Extract condition from IF statement
        Pattern ifPattern = Pattern.compile(
            "IF\\s+(.+?)(?=\\s+(?:THEN|$))",
            Pattern.CASE_INSENSITIVE
        );
        
        Matcher matcher = ifPattern.matcher(text);
        if (matcher.find()) {
            stmt.setMainOperand(matcher.group(1).trim());
        }
    }
    
    private void parseEvaluateStatement(String text, Statement stmt) {
        // Extract what is being evaluated
        Pattern evalPattern = Pattern.compile(
            "EVALUATE\\s+(.+?)(?=\\s+(?:WHEN|$))",
            Pattern.CASE_INSENSITIVE
        );
        
        Matcher matcher = evalPattern.matcher(text);
        if (matcher.find()) {
            stmt.setMainOperand(matcher.group(1).trim());
        }
    }
    
    private void parseStringStatement(String text, Statement stmt) {
        // Parse STRING statement - simplified for now
        if (text.toUpperCase().contains("INTO")) {
            Pattern stringPattern = Pattern.compile(
                "STRING\\s+(.+?)\\s+INTO\\s+(\\S+)",
                Pattern.CASE_INSENSITIVE | Pattern.DOTALL
            );
            
            Matcher matcher = stringPattern.matcher(text);
            if (matcher.find()) {
                stmt.setMainOperand(matcher.group(1).trim());
                stmt.setTargetOperand(matcher.group(2).trim());
            }
        }
    }
    
    private void parseExecSqlStatement(String text, Statement stmt) {
        // Extract SQL content
        Pattern sqlPattern = Pattern.compile(
            "EXEC\\s+SQL\\s+(.+?)\\s+END-EXEC",
            Pattern.CASE_INSENSITIVE | Pattern.DOTALL
        );
        
        Matcher matcher = sqlPattern.matcher(text);
        if (matcher.find()) {
            stmt.setMainOperand(matcher.group(1).trim());
        }
    }
    
    private String cleanOperand(String operand) {
        if (operand == null) return null;
        
        operand = operand.trim();
        
        // Keep quotes for literals
        if ((operand.startsWith("\"") && operand.endsWith("\"")) ||
            (operand.startsWith("'") && operand.endsWith("'"))) {
            return operand;
        }
        
        // Clean up variable names
        return operand.replaceAll("\\s+", " ").trim();
    }
}