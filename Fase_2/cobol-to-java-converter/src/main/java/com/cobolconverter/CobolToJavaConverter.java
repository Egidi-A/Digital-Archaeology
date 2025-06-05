package com.cobolconverter;

import java.io.*;
import java.util.*;
import java.util.regex.*;

public class CobolToJavaConverter {
    
    // Class to hold the parsed Identification Division data
    static class IdentificationDivision {
        String programId;
        String author;
        String installation;
        String dateWritten;
        String dateCompiled;
        String security;
        String remarks;
        
        public String toJavaClass() {
            StringBuilder sb = new StringBuilder();
            
            // Generate Java class with metadata as comments
            sb.append("/**\n");
            if (author != null) sb.append(" * @author ").append(author).append("\n");
            if (dateWritten != null) sb.append(" * @date ").append(dateWritten).append("\n");
            if (installation != null) sb.append(" * Installation: ").append(installation).append("\n");
            if (security != null) sb.append(" * Security: ").append(security).append("\n");
            if (remarks != null) sb.append(" * Remarks: ").append(remarks).append("\n");
            sb.append(" * Generated from COBOL program\n");
            sb.append(" */\n");
            
            // Generate the class declaration
            sb.append("public class ").append(toCamelCase(programId)).append(" {\n");
            sb.append("    \n");
            sb.append("    // Program ID: ").append(programId).append("\n");
            if (dateCompiled != null) {
                sb.append("    // Date Compiled: ").append(dateCompiled).append("\n");
            }
            sb.append("    \n");
            sb.append("    public static void main(String[] args) {\n");
            sb.append("        // Main program logic goes here\n");
            sb.append("    }\n");
            sb.append("}\n");
            
            return sb.toString();
        }
        
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
    }
    
    public static void main(String[] args) {
        if (args.length < 1) {
            System.out.println("Usage: java CobolToJavaConverter <input-cobol-file>");
            return;
        }
        
        String inputFile = args[0];
        String outputFile = inputFile.replaceAll("\\.(cob|cbl|cobol)$", "") + ".java";
        
        try {
            String cobolSource = readFile(inputFile);
            IdentificationDivision idDiv = parseIdentificationDivision(cobolSource);
            
            if (idDiv.programId == null) {
                System.err.println("Error: No PROGRAM-ID found in IDENTIFICATION DIVISION");
                return;
            }
            
            String javaCode = idDiv.toJavaClass();
            writeFile(outputFile, javaCode);
            
            System.out.println("Successfully converted IDENTIFICATION DIVISION");
            System.out.println("Input: " + inputFile);
            System.out.println("Output: " + outputFile);
            System.out.println("Java class name: " + idDiv.toCamelCase(idDiv.programId));
            
        } catch (IOException e) {
            System.err.println("Error reading/writing file: " + e.getMessage());
        }
    }
    
    private static String readFile(String filename) throws IOException {
        StringBuilder content = new StringBuilder();
        try (BufferedReader reader = new BufferedReader(new FileReader(filename))) {
            String line;
            while ((line = reader.readLine()) != null) {
                content.append(line).append("\n");
            }
        }
        return content.toString();
    }
    
    private static void writeFile(String filename, String content) throws IOException {
        try (BufferedWriter writer = new BufferedWriter(new FileWriter(filename))) {
            writer.write(content);
        }
    }
    
    private static IdentificationDivision parseIdentificationDivision(String cobolSource) {
        IdentificationDivision idDiv = new IdentificationDivision();
        
        // Normalize the source (remove comments, handle continuations)
        String normalized = normalizeCobolSource(cobolSource);
        
        // Find IDENTIFICATION DIVISION section
        Pattern idDivPattern = Pattern.compile(
            "IDENTIFICATION\\s+DIVISION\\s*\\.([^\\n]*(?:\\n(?!\\s*(?:ENVIRONMENT|DATA|PROCEDURE)\\s+DIVISION)[^\\n]*)*)",
            Pattern.CASE_INSENSITIVE | Pattern.DOTALL
        );
        
        Matcher idDivMatcher = idDivPattern.matcher(normalized);
        if (!idDivMatcher.find()) {
            System.err.println("Warning: IDENTIFICATION DIVISION not found");
            return idDiv;
        }
        
        String idDivContent = idDivMatcher.group(1);
        
        // Parse individual fields
        idDiv.programId = extractField(idDivContent, "PROGRAM-ID");
        idDiv.author = extractField(idDivContent, "AUTHOR");
        idDiv.installation = extractField(idDivContent, "INSTALLATION");
        idDiv.dateWritten = extractField(idDivContent, "DATE-WRITTEN");
        idDiv.dateCompiled = extractField(idDivContent, "DATE-COMPILED");
        idDiv.security = extractField(idDivContent, "SECURITY");
        idDiv.remarks = extractMultilineField(idDivContent, "REMARKS");
        
        return idDiv;
    }
    
    private static String normalizeCobolSource(String source) {
        String[] lines = source.split("\n");
        StringBuilder normalized = new StringBuilder();
        
        for (String line : lines) {
            // Skip comment lines (asterisk in column 7)
            if (line.length() >= 7 && line.charAt(6) == '*') {
                continue;
            }
            
            // Remove sequence numbers (columns 1-6) and indicator area if present
            if (line.length() > 7) {
                line = line.substring(7);
            }
            
            // Trim trailing spaces
            line = line.replaceAll("\\s+$", "");
            
            if (!line.isEmpty()) {
                normalized.append(line).append("\n");
            }
        }
        
        return normalized.toString();
    }
    
    private static String extractField(String content, String fieldName) {
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
    
    private static String extractMultilineField(String content, String fieldName) {
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