package com.DigArch.cobol2java.parser;

import koopa.cobol.parser.CobolParser;
import koopa.cobol.parser.ParseResults;
import koopa.core.trees.Tree;
import koopa.core.data.Token;

import java.io.File;
import java.io.IOException;
import java.util.List;

public class CustomCobolParser {
    private final CobolParser parser;

    public CustomCobolParser() {
        this.parser = new CobolParser();
    }

    public Tree parseFile(String filePath) throws IOException {
        File cobolFile = new File(filePath);
        if (!cobolFile.exists()) {
            throw new IOException("COBOL file not found: " + filePath);
        }

        ParseResults results = parser.parse(cobolFile);
        
        if (!results.isValidInput()) {
            StringBuilder errorMsg = new StringBuilder("Parsing errors:\n");
            List<String> errors = results.hasErrors();
            for (String error : errors) {
                errorMsg.append("- ").append(error).append("\n");
            }
            throw new RuntimeException(errorMsg.toString());
        }

        return results.getAST();
    }

    public Tree parse(String cobolCode) {
        ParseResults results = parser.parse(cobolCode);
        return results.getAST();
    }

    public void printAST(Tree ast) {
        printNode(ast, 0);
    }

    private void printNode(Tree node, int depth) {
        String indent = "  ".repeat(depth);
        System.out.println(indent + "+" + node.getName());

        for (Token token : node.getTokens()) {
            System.out.println(indent + "  " + token.getText());
        }

        for (Tree child : node.getChildren()) {
            printNode(child, depth + 1);
        }
    }
}