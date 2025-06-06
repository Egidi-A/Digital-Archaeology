package com.cobolconverter.parser;

import com.cobolconverter.model.DataDivision;
import com.cobolconverter.model.DataDivision.DataItem;
import java.util.ArrayList;
import java.util.List;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

public class DataDivisionParser {
    
    public DataDivision parse(String normalizedSource) {
        DataDivision dataDiv = new DataDivision();
        
        // Find DATA DIVISION section
        Pattern dataDivPattern = Pattern.compile(
            "DATA\\s+DIVISION\\s*\\.([^\\n]*(?:\\n(?!\\s*(?:PROCEDURE)\\s+DIVISION)[^\\n]*)*)",
            Pattern.CASE_INSENSITIVE | Pattern.DOTALL
        );
        
        Matcher dataDivMatcher = dataDivPattern.matcher(normalizedSource);
        if (!dataDivMatcher.find()) {
            return dataDiv; // Return empty if not found
        }
        
        String dataDivContent = dataDivMatcher.group(1);
        
        // Parse File Section (keep as raw text)
        dataDiv.setFileSection(extractSection(dataDivContent, "FILE"));
        
        // Parse Working-Storage Section
        String workingStorageContent = extractSection(dataDivContent, "WORKING-STORAGE");
        if (workingStorageContent != null) {
            List<DataItem> items = parseDataItems(workingStorageContent);
            dataDiv.setWorkingStorageItems(items);
        }
        
        // Parse Local-Storage Section (keep as raw text)
        dataDiv.setLocalStorageSection(extractSection(dataDivContent, "LOCAL-STORAGE"));
        
        // Parse Linkage Section (keep as raw text)
        dataDiv.setLinkageSection(extractSection(dataDivContent, "LINKAGE"));
        
        return dataDiv;
    }
    
    private String extractSection(String content, String sectionName) {
        Pattern pattern = Pattern.compile(
            sectionName + "\\s+SECTION\\s*\\.([^\\n]*(?:\\n(?!\\s*(?:FILE|WORKING-STORAGE|LOCAL-STORAGE|LINKAGE|PROCEDURE)\\s+(?:SECTION|DIVISION))[^\\n]*)*)",
            Pattern.CASE_INSENSITIVE | Pattern.DOTALL
        );
        
        Matcher matcher = pattern.matcher(content);
        if (matcher.find()) {
            return matcher.group(1).trim();
        }
        
        return null;
    }
    
    private List<DataItem> parseDataItems(String content) {
        List<DataItem> items = new ArrayList<>();
        
        // Pattern to match data items
        // Matches: level-number data-name [PIC clause] [VALUE clause] [OCCURS clause]
        Pattern itemPattern = Pattern.compile(
            "(\\d{2})\\s+(\\S+)(?:\\s+PIC(?:TURE)?\\s+(?:IS\\s+)?([^.\\n]+?))?(?:\\s+VALUE\\s+(?:IS\\s+)?([^.\\n]+?))?(?:\\s+OCCURS\\s+([^.\\n]+?))?\\s*\\.",
            Pattern.CASE_INSENSITIVE | Pattern.MULTILINE
        );
        
        Matcher matcher = itemPattern.matcher(content);
        
        while (matcher.find()) {
            String level = matcher.group(1);
            String name = matcher.group(2);
            String picture = matcher.group(3);
            String value = matcher.group(4);
            String occurs = matcher.group(5);
            
            // Skip FILLER items
            if ("FILLER".equalsIgnoreCase(name)) {
                continue;
            }
            
            DataItem item = new DataItem(level, name);
            
            if (picture != null) {
                item.setPicture(picture.trim());
            } else {
                // If no PIC clause, it's likely a group item
                item.setGroup(true);
            }
            
            if (value != null) {
                // Clean up value - remove quotes and SPACES/ZEROS keywords
                value = value.trim();
                if (value.startsWith("\"") && value.endsWith("\"")) {
                    value = value.substring(1, value.length() - 1);
                } else if (value.startsWith("'") && value.endsWith("'")) {
                    value = value.substring(1, value.length() - 1);
                } else if ("SPACES".equalsIgnoreCase(value) || "SPACE".equalsIgnoreCase(value)) {
                    value = "";
                } else if ("ZEROS".equalsIgnoreCase(value) || "ZERO".equalsIgnoreCase(value) || "ZEROES".equalsIgnoreCase(value)) {
                    value = "0";
                }
                item.setValue(value);
            }
            
            if (occurs != null) {
                // Extract the number from OCCURS clause
                Pattern occursPattern = Pattern.compile("(\\d+)");
                Matcher occursMatcher = occursPattern.matcher(occurs);
                if (occursMatcher.find()) {
                    item.setOccurs(occursMatcher.group(1));
                }
            }
            
            items.add(item);
        }
        
        // Also look for 88-level condition names (for completeness, stored as comments)
        Pattern conditionPattern = Pattern.compile(
            "88\\s+(\\S+)\\s+VALUE\\s+(?:IS\\s+)?([^.\\n]+)\\s*\\.",
            Pattern.CASE_INSENSITIVE | Pattern.MULTILINE
        );
        
        Matcher conditionMatcher = conditionPattern.matcher(content);
        while (conditionMatcher.find()) {
            String conditionName = conditionMatcher.group(1);
            String conditionValue = conditionMatcher.group(2).trim();
            
            // Create as a special item with level 88
            DataItem condition = new DataItem("88", conditionName);
            condition.setValue(conditionValue);
            items.add(condition);
        }
        
        return items;
    }
}