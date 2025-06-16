package com.DigArch.cobol2java.converter;

import com.DigArch.cobol2java.api.CobolToJavaConverter;
import com.DigArch.cobol2java.parser.CustomCobolParser;
import koopa.core.trees.Tree;

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Paths;

public class DefaultCobolConverter implements CobolToJavaConverter {
    private final CustomCobolParser parser;

    public DefaultCobolConverter() {
        this.parser = new CustomCobolParser();
    }

    @Override
    public String convertCobolToJava(String inputFile, String outputFile) {
        try {
            Tree ast = parseCobol(inputFile);
            String javaCode = generateJava(ast);
            Files.writeString(Paths.get(outputFile), javaCode);
            return javaCode;
        } catch (IOException e) {
            throw new RuntimeException("Conversion failed: " + e.getMessage(), e);
        }
    }

    @Override
    public Tree parseCobol(String cobolFile) {
        try {
            return parser.parseFile(cobolFile);
        } catch (IOException e) {
            throw new RuntimeException("Parsing failed: " + e.getMessage(), e);
        }
    }

    @Override
    public String generateJava(Tree ast) {
        // Basic implementation - will need to be expanded
        StringBuilder java = new StringBuilder();
        java.append("public class GeneratedProgram {\n");
        java.append("    public static void main(String[] args) {\n");
        java.append("        // TODO: Implement COBOL to Java conversion\n");
        java.append("    }\n");
        java.append("}\n");
        return java.toString();
    }
}