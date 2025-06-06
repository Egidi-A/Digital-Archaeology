package com.cobolconverter.generator;

import com.cobolconverter.model.CobolProgram;
import com.cobolconverter.model.IdentificationDivision;
import com.cobolconverter.model.EnvironmentDivision;
import com.cobolconverter.model.DataDivision;
import com.cobolconverter.model.DataDivision.DataItem;


public class JavaCodeGenerator {
    
    public String generate(CobolProgram program) {
        StringBuilder sb = new StringBuilder();
        
        IdentificationDivision idDiv = program.getIdentificationDivision();
        EnvironmentDivision envDiv = program.getEnvironmentDivision();
        DataDivision dataDiv = program.getDataDivision();
        
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
        
        sb.append("    public static void main(String[] args) {\n");
        sb.append("        // Main program logic goes here\n");
        sb.append("        System.out.println(\"").append(className).append(" - Generated from COBOL\");\n");
        sb.append("    }\n");
        sb.append("}\n");

        //----------------------------------------------------------------------------------------------
        
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
}