package com.cobolconverter.generator;

import com.cobolconverter.model.CobolProgram;
import com.cobolconverter.model.IdentificationDivision;
import com.cobolconverter.model.EnvironmentDivision;
import com.cobolconverter.model.DataDivision;
import com.cobolconverter.model.DataDivision.DataItem;
import com.cobolconverter.model.ProcedureDivision;
import com.cobolconverter.model.ProcedureDivision.*;



public class JavaCodeGenerator {
    
    public String generate(CobolProgram program) {
        StringBuilder sb = new StringBuilder();
        
        IdentificationDivision idDiv = program.getIdentificationDivision();
        EnvironmentDivision envDiv = program.getEnvironmentDivision();
        DataDivision dataDiv = program.getDataDivision();
        ProcedureDivision procDiv = program.getProcedureDivision();
        
        /***********************************************************************************************
        * IDENTIFICATION DIVISION
        ************************************************************************************************/

        //----------------------------------------------------------------------------------------------

        // JavaDoc
        sb.append("/**\n");
        if (idDiv.getAuthor() != null) {
            sb.append(" * @author ").append(idDiv.getAuthor()).append("\n");
        }
        if (idDiv.getDateWritten() != null) {
            sb.append(" * @date ").append(idDiv.getDateWritten()).append("\n");
        }
        if (idDiv.getInstallation() != null) {
            sb.append(" * Installation: ").append(idDiv.getInstallation()).append("\n");
        }
        if (idDiv.getSecurity() != null) {
            sb.append(" * Security: ").append(idDiv.getSecurity()).append("\n");
        }
        if (idDiv.getRemarks() != null) {
            sb.append(" * Remarks: ").append(idDiv.getRemarks()).append("\n");
        }
        sb.append(" * Generated from COBOL program\n");
        sb.append(" */\n");
        
        // Class Declaration
        String className = toCamelCase(idDiv.getProgramId());
        sb.append("public class ").append(className).append(" {\n");
        sb.append("    \n");
        sb.append("    // Program ID: ").append(idDiv.getProgramId()).append("\n");

        //----------------------------------------------------------------------------------------------
        
        /***********************************************************************************************
        * ENVIRONMENT DIVISION
        ************************************************************************************************/    

        //----------------------------------------------------------------------------------------------

        // Add Environment Division info as comments
        if (envDiv != null) {
            sb.append(envDiv.toJavaComment());
        }
        
        //----------------------------------------------------------------------------------------------

        /***********************************************************************************************
        * DATA DIVISION
        ************************************************************************************************/

        //----------------------------------------------------------------------------------------------

        // Add Data Division info
        if (dataDiv != null) {
            sb.append(dataDiv.toJavaComment());
            sb.append("    \n");
            
            // Generate fields from Working-Storage items
            if (!dataDiv.getWorkingStorageItems().isEmpty()) {
                sb.append("    // Working-Storage Fields\n");
                for (DataItem item : dataDiv.getWorkingStorageItems()) {
                    // Skip 88-level condition names for now
                    if (!"88".equals(item.getLevel())) {
                        sb.append(item.toJavaField()).append("\n");
                    }
                }
                sb.append("    \n");
            }
        }
        //----------------------------------------------------------------------------------------------

        /***********************************************************************************************
        * PROCEDURE DIVISION
        ************************************************************************************************/
        
        //----------------------------------------------------------------------------------------------
        // Add Procedure Division info
        if (procDiv != null) {
            sb.append(procDiv.toJavaComment());
            sb.append("    \n");
            
            // Generate main method
            sb.append("    public static void main(String[] args) {\n");
            sb.append("        ").append(className).append(" program = new ").append(className).append("();\n");
            
            // Call the first paragraph if exists
            if (procDiv.hasSections() && !procDiv.getSections().isEmpty()) {
                Section firstSection = procDiv.getSections().get(0);
                if (!firstSection.getParagraphs().isEmpty()) {
                    String firstPara = toJavaMethodName(firstSection.getParagraphs().get(0).getName());
                    sb.append("        program.").append(firstPara).append("();\n");
                }
            } else if (!procDiv.getParagraphs().isEmpty()) {
                String firstPara = toJavaMethodName(procDiv.getParagraphs().get(0).getName());
                sb.append("        program.").append(firstPara).append("();\n");
            }
            
            sb.append("    }\n");
            sb.append("    \n");
            
            // Generate methods for paragraphs
            if (procDiv.hasSections()) {
                for (Section section : procDiv.getSections()) {
                    sb.append("    // ").append(section.getName()).append(" SECTION\n");
                    for (Paragraph para : section.getParagraphs()) {
                        generateParagraphMethod(para, sb);
                    }
                }
            } else {
                for (Paragraph para : procDiv.getParagraphs()) {
                    generateParagraphMethod(para, sb);
                }
            }
        } else {

    //----------------------------------------------------------------------------------------------

            sb.append("    public static void main(String[] args) {\n");
            sb.append("        // Main program logic goes here\n");
            sb.append("        System.out.println(\"").append(className).append(" - Generated from COBOL\");\n");
            sb.append("    }\n");
            sb.append("}\n");

        }
    //----------------------------------------------------------------------------------------------

        sb.append("}\n");

        // Return the generated Java code as a string
        return sb.toString();
    }
    


    /************************************************************************************************
     * FUNZIONI DI SUPPORTO
     ************************************************************************************************/
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
    
    private String toJavaMethodName(String cobolName) {
        if (cobolName == null) return "unnamed";
        
        String[] parts = cobolName.split("[-_]");
        StringBuilder result = new StringBuilder();
        
        for (int i = 0; i < parts.length; i++) {
            String part = parts[i];
            if (!part.isEmpty()) {
                if (i == 0) {
                    result.append(part.toLowerCase());
                } else {
                    result.append(Character.toUpperCase(part.charAt(0)));
                    if (part.length() > 1) {
                        result.append(part.substring(1).toLowerCase());
                    }
                }
            }
        }
        
        return result.toString();
    }
    
    private String toJavaVarName(String cobolName) {
        if (cobolName == null) return "null";
        
        // Handle special COBOL values
        if ("SPACES".equalsIgnoreCase(cobolName) || "SPACE".equalsIgnoreCase(cobolName)) {
            return "\"\"";
        }
        if ("ZEROS".equalsIgnoreCase(cobolName) || "ZERO".equalsIgnoreCase(cobolName)) {
            return "0";
        }
        
        // Convert variable name
        return toJavaMethodName(cobolName);
    }

    private void generateParagraphMethod(Paragraph paragraph, StringBuilder sb) {
        String methodName = toJavaMethodName(paragraph.getName());
        
        sb.append("    private void ").append(methodName).append("() {\n");
        sb.append("        // ").append(paragraph.getName()).append("\n");
        
        for (Statement stmt : paragraph.getStatements()) {
            generateStatement(stmt, sb, "        ");
        }
        
        sb.append("    }\n");
        sb.append("    \n");
    }

    private void generateStatement(Statement stmt, StringBuilder sb, String indent) {
        String javaCode = stmt.toJava();
        
        // Handle variable name conversion for known operands
        if (stmt.getType() == Statement.StatementType.MOVE) {
            String source = toJavaVarName(stmt.getMainOperand());
            String target = toJavaVarName(stmt.getTargetOperand());
            javaCode = target + " = " + source + ";";
        } else if (stmt.getType() == Statement.StatementType.DISPLAY) {
            String operand = stmt.getMainOperand();
            if (operand != null && !operand.startsWith("\"") && !operand.startsWith("'")) {
                operand = toJavaVarName(operand);
            }
            javaCode = "System.out.println(" + operand + ");";
        } else if (stmt.getType() == Statement.StatementType.PERFORM) {
            String methodCall = toJavaMethodName(stmt.getMainOperand());
            javaCode = methodCall + "();";
        }
        
        sb.append(indent).append(javaCode).append("\n");
        
        // Handle nested statements (for future IF/EVALUATE blocks)
        for (Statement nested : stmt.getNestedStatements()) {
            generateStatement(nested, sb, indent + "    ");
        }
    }
}