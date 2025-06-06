package com.cobolconverter.model;

import java.util.ArrayList;
import java.util.List;

public class DataDivision {
    // Sections of the Data Division
    private String fileSection;
    private List<DataItem> workingStorageItems = new ArrayList<>();
    private String localStorageSection;
    private String linkageSection;
    
    // Inner class for data items
    public static class DataItem {
        private String level;
        private String name;
        private String picture;
        private String value;
        private String occurs;
        private boolean isGroup;
        
        public DataItem(String level, String name) {
            this.level = level;
            this.name = name;
            this.isGroup = false;
        }
        
        // Getters and setters
        public String getLevel() { return level; }
        public void setLevel(String level) { this.level = level; }
        
        public String getName() { return name; }
        public void setName(String name) { this.name = name; }
        
        public String getPicture() { return picture; }
        public void setPicture(String picture) { this.picture = picture; }
        
        public String getValue() { return value; }
        public void setValue(String value) { this.value = value; }
        
        public String getOccurs() { return occurs; }
        public void setOccurs(String occurs) { this.occurs = occurs; }
        
        public boolean isGroup() { return isGroup; }
        public void setGroup(boolean isGroup) { this.isGroup = isGroup; }
        
        // Convert COBOL PIC to Java type
        public String getJavaType() {
            if (picture == null) return "Object";
            
            if (picture.matches(".*[9]+.*")) {
                if (picture.contains("V") || picture.contains(".")) {
                    return "double";
                }
                return "int";
            } else if (picture.matches(".*[X]+.*") || picture.matches(".*[A]+.*")) {
                return "String";
            }
            return "String"; // Default
        }
        
        // Get Java field declaration
        public String toJavaField() {
            StringBuilder sb = new StringBuilder();
            String javaName = toJavaName(name);
            String javaType = getJavaType();
            
            if (occurs != null) {
                sb.append("    private ").append(javaType).append("[] ").append(javaName);
                sb.append("; // OCCURS ").append(occurs);
            } else {
                sb.append("    private ").append(javaType).append(" ").append(javaName);
                if (value != null) {
                    if ("String".equals(javaType)) {
                        sb.append(" = \"").append(value.replaceAll("\"", "\\\\\"")).append("\"");
                    } else {
                        sb.append(" = ").append(value);
                    }
                }
            }
            sb.append("; // PIC ").append(picture != null ? picture : "GROUP");
            
            return sb.toString();
        }
        
        private String toJavaName(String cobolName) {
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
    }
    
    // Getters and setters
    public String getFileSection() { return fileSection; }
    public void setFileSection(String fileSection) { this.fileSection = fileSection; }
    
    public List<DataItem> getWorkingStorageItems() { return workingStorageItems; }
    public void setWorkingStorageItems(List<DataItem> workingStorageItems) { 
        this.workingStorageItems = workingStorageItems; 
    }
    
    public String getLocalStorageSection() { return localStorageSection; }
    public void setLocalStorageSection(String localStorageSection) { 
        this.localStorageSection = localStorageSection; 
    }
    
    public String getLinkageSection() { return linkageSection; }
    public void setLinkageSection(String linkageSection) { this.linkageSection = linkageSection; }
    
    public String toJavaComment() {
        StringBuilder sb = new StringBuilder();
        sb.append("    // Data Division\n");
        if (fileSection != null) {
            sb.append("    // File Section: Present\n");
        }
        if (!workingStorageItems.isEmpty()) {
            sb.append("    // Working-Storage: ").append(workingStorageItems.size()).append(" items\n");
        }
        if (localStorageSection != null) {
            sb.append("    // Local-Storage Section: Present\n");
        }
        if (linkageSection != null) {
            sb.append("    // Linkage Section: Present\n");
        }
        return sb.toString();
    }
}