package com.example;

import io.proleap.cobol.CobolBaseVisitor;
import io.proleap.cobol.CobolParser;

import io.proleap.cobol.asg.metamodel.CompilationUnit;
import io.proleap.cobol.asg.metamodel.Program;
import io.proleap.cobol.asg.metamodel.data.datadescription.DataDescriptionEntry;
import io.proleap.cobol.asg.runner.impl.CobolParserRunnerImpl;
import io.proleap.cobol.preprocessor.CobolPreprocessor;

import java.io.File;

public class ASTVisitorExample {
    
    public static void main(String[] args) {
        try {
            // Parse COBOL file
            File inputFile = new File("src/main/resources/cobol/File_COBOL.cbl");
            Program program = new CobolParserRunnerImpl().analyzeFile(inputFile, CobolPreprocessor.CobolSourceFormatEnum.FIXED);
            
            // Create visitor for AST traversal
            CobolBaseVisitor<Boolean> visitor = new CobolBaseVisitor<Boolean>() {
                @Override
                public Boolean visitDataDescriptionEntryFormat1(CobolParser.DataDescriptionEntryFormat1Context ctx) {
                    DataDescriptionEntry entry = (DataDescriptionEntry) program.getASGElementRegistry().getASGElement(ctx);
                    if (entry != null) {
                        System.out.println("Data Entry: " + entry.getName() + " (Level: " + entry.getLevelNumber() + ")");
                    }
                    return visitChildren(ctx);
                }
                
                @Override
                public Boolean visitParagraph(CobolParser.ParagraphContext ctx) {
                    System.out.println("Paragraph: " + ctx.getText());
                    return visitChildren(ctx);
                }
                
                @Override
                public Boolean visitDisplayStatement(CobolParser.DisplayStatementContext ctx) {
                    System.out.println("Display Statement found");
                    return visitChildren(ctx);
                }
            };
            
            // Visit all compilation units
            for (CompilationUnit compilationUnit : program.getCompilationUnits()) {
                visitor.visit(compilationUnit.getCtx());
            }
            
        } catch (Exception e) {
            e.printStackTrace();
        }
    }
}