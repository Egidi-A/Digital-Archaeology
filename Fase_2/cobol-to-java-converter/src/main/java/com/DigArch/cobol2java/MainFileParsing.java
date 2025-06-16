package com.DigArch.cobol2java;

import com.DigArch.cobol2java.converter.DefaultCobolConverter;
import com.DigArch.cobol2java.api.CobolToJavaConverter;

public class MainFileParsing {
    public static void main(String[] args) {
        if (args.length != 2) {
            System.err.println("Usage: MainFileParsing <input.cob> <output.java>");
            System.exit(1);
        }

        try {
            String inputFile = args[0];
            String outputFile = args[1];
            
            CobolToJavaConverter converter = new DefaultCobolConverter();
            String javaCode = converter.convertCobolToJava(inputFile, outputFile);
            
            System.out.println("Conversion completed successfully.");
            System.out.println("Output written to: " + outputFile);
            
        } catch (Exception e) {
            System.err.println("Error: " + e.getMessage());
            e.printStackTrace();
        }
    }
}