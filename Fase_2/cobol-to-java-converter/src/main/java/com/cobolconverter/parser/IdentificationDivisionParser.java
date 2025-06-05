package com.cobolconverter.parser;

import com.cobolconverter.model.IdentificationDivision;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

public class IdentificationDivisionParser {
    
    public IdentificationDivision parse(String normalizedSource) {
        IdentificationDivision idDiv = new IdentificationDivision();
        
        // Find IDENTIFICATION DIVISION section
        Pattern idDivPattern = Pattern.compile(
            "IDENTIFICATION\\s+DIVISION\\s*\\.([^\\n]*(?:\\n(?!\\s*(?:ENVIRONMENT|DATA|PROCEDURE)\\s+DIVISION)[^\\n]*)*)",
            Pattern.CASE_INSENSITIVE | Pattern.DOTALL
        );
        
        Matcher idDivMatcher = idDivPattern.matcher(normalizedSource);
        if (!idDivMatcher.find()) {
            System.err.println("Warning: IDENTIFICATION DIVISION not found");
            return idDiv;
        }
        
        String idDivContent = idDivMatcher.group(1);
        
        // Parse individual fields
        idDiv.setProgramId(extractField(idDivContent, "PROGRAM-ID"));
        idDiv.setAuthor(extractField(idDivContent, "AUTHOR"));
        idDiv.setInstallation(extractField(idDivContent, "INSTALLATION"));
        idDiv.setDateWritten(extractField(idDivContent, "DATE-WRITTEN"));
        idDiv.setDateCompiled(extractField(idDivContent, "DATE-COMPILED"));
        idDiv.setSecurity(extractField(idDivContent, "SECURITY"));
        idDiv.setRemarks(extractMultilineField(idDivContent, "REMARKS"));
        
        return idDiv;
    }
    
    private String extractField(String content, String fieldName) {
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
    
    private String extractMultilineField(String content, String fieldName) {
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