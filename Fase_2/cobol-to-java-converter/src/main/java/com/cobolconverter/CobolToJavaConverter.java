package com.cobolconverter;

import java.io.*;
import com.cobolconverter.model.*;
import com.cobolconverter.parser.*;
import com.cobolconverter.generator.JavaCodeGenerator;

public class CobolToJavaConverter {
    
    public static void main(String[] args) {
        if (args.length < 1) {
            System.out.println("Usage: java CobolToJavaConverter <input-cobol-file>");
            return;
        }
        
        String inputFile = args[0];
        String outputFile = inputFile.replaceAll("\\.(cob|cbl|cobol)$", "") + ".java";        
        try {
            // Read COBOL source
            String cobolSource = readFile(inputFile);
            String normalizedSource = normalizeCobolSource(cobolSource);
            
            // Parse divisions
            CobolProgram program = new CobolProgram();
            
            IdentificationDivisionParser idParser = new IdentificationDivisionParser();
            program.setIdentificationDivision(idParser.parse(normalizedSource));
            
            EnvironmentDivisionParser envParser = new EnvironmentDivisionParser();
            program.setEnvironmentDivision(envParser.parse(normalizedSource));

            DataDivisionParser dataParser = new DataDivisionParser();
            program.setDataDivision(dataParser.parse(normalizedSource));
            
            // Generate Java code
            JavaCodeGenerator generator = new JavaCodeGenerator();
            String javaCode = generator.generate(program);
            
            // Write output
            writeFile(outputFile, javaCode);
            
            System.out.println("Successfully converted COBOL to Java");
            System.out.println("Input: " + inputFile);
            System.out.println("Output: " + outputFile);
            
        } catch (IOException e) {
            System.err.println("Error: " + e.getMessage());
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
    
    private static String normalizeCobolSource(String source) {
        String[] lines = source.split("\n");
        StringBuilder normalized = new StringBuilder();
        
        for (String line : lines) {
            // Skip comment lines
            if (line.length() >= 7 && line.charAt(6) == '*') {
                continue;
            }
            
            // Remove sequence numbers (columns 1-6) and indicator area
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
}