package com.cobolconverter.parser;

import com.cobolconverter.model.EnvironmentDivision;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

public class EnvironmentDivisionParser {
    
    public EnvironmentDivision parse(String normalizedSource) {
        EnvironmentDivision envDiv = new EnvironmentDivision();
        
        // Find ENVIRONMENT DIVISION section
        Pattern envDivPattern = Pattern.compile(
            "ENVIRONMENT\\s+DIVISION\\s*\\.([^\\n]*(?:\\n(?!\\s*(?:DATA|PROCEDURE)\\s+DIVISION)[^\\n]*)*)",
            Pattern.CASE_INSENSITIVE | Pattern.DOTALL
        );
        
        Matcher envDivMatcher = envDivPattern.matcher(normalizedSource);
        if (!envDivMatcher.find()) {
            return envDiv; // Return empty if not found
        }
        
        String envDivContent = envDivMatcher.group(1);
        
        // Parse Configuration Section
        Pattern configPattern = Pattern.compile(
            "CONFIGURATION\\s+SECTION\\s*\\.([^\\n]*(?:\\n(?!\\s*(?:INPUT-OUTPUT|DATA|PROCEDURE)\\s+(?:SECTION|DIVISION))[^\\n]*)*)",
            Pattern.CASE_INSENSITIVE | Pattern.DOTALL
        );
        
        Matcher configMatcher = configPattern.matcher(envDivContent);
        if (configMatcher.find()) {
            String configContent = configMatcher.group(1);
            
            // Extract SOURCE-COMPUTER
            envDiv.setSourceComputer(extractField(configContent, "SOURCE-COMPUTER"));
            
            // Extract OBJECT-COMPUTER
            envDiv.setObjectComputer(extractField(configContent, "OBJECT-COMPUTER"));
            
            // Extract SPECIAL-NAMES
            envDiv.setSpecialNames(extractMultilineField(configContent, "SPECIAL-NAMES"));
        }
        
        // Parse Input-Output Section
        Pattern ioPattern = Pattern.compile(
            "INPUT-OUTPUT\\s+SECTION\\s*\\.([^\\n]*(?:\\n(?!\\s*(?:DATA|PROCEDURE)\\s+DIVISION)[^\\n]*)*)",
            Pattern.CASE_INSENSITIVE | Pattern.DOTALL
        );
        
        Matcher ioMatcher = ioPattern.matcher(envDivContent);
        if (ioMatcher.find()) {
            String ioContent = ioMatcher.group(1);
            
            // Extract FILE-CONTROL
            envDiv.setFileControl(extractMultilineField(ioContent, "FILE-CONTROL"));
            
            // Extract I-O-CONTROL
            envDiv.setIoControl(extractMultilineField(ioContent, "I-O-CONTROL"));
        }
        
        return envDiv;
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