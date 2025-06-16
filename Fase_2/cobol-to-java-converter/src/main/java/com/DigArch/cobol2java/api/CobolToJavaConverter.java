package com.DigArch.cobol2java.api;

import koopa.core.trees.Tree;

public interface CobolToJavaConverter {
    String convertCobolToJava(String inputFile, String outputFile);
    Tree parseCobol(String cobolFile);
    String generateJava(Tree ast);
}