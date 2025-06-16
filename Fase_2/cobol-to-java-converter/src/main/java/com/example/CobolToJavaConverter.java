package com.example;

import io.proleap.cobol.asg.metamodel.Program;
import io.proleap.cobol.asg.runner.impl.CobolParserRunnerImpl;
import io.proleap.cobol.preprocessor.CobolPreprocessor;

import java.io.File;

public class CobolToJavaConverter {
    public static void main(String[] args) {
        try {
            File cobolFile = new File("src/main/resources/cobol/example.cbl");
            
            Program program = new CobolParserRunnerImpl().analyzeFile(cobolFile, CobolPreprocessor.CobolSourceFormatEnum.FIXED);
            
            System.out.println("Parsing completato!");
            program.getCompilationUnits().forEach(cu -> {
                if (cu.getProgramUnit() != null) {
                    System.out.println("Program ID: " + cu.getProgramUnit().getIdentificationDivision()
                        .getProgramIdParagraph().getName());
                }
            });
            
            // Esporta in XML
            ASTToXMLExporter.exportToXML(program, "output/ast.xml");
        } catch (Exception e) {
            e.printStackTrace();
        }
    }
}