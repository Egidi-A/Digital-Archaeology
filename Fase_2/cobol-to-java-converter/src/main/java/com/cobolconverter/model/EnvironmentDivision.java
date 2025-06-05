package com.cobolconverter.model;

public class EnvironmentDivision {
    // Configuration Section
    private String sourceComputer;
    private String objectComputer;
    private String specialNames;
    
    // Input-Output Section
    private String fileControl;
    private String ioControl;
    
    // Getters and Setters
    public String getSourceComputer() { return sourceComputer; }
    public void setSourceComputer(String sourceComputer) { this.sourceComputer = sourceComputer; }
    
    public String getObjectComputer() { return objectComputer; }
    public void setObjectComputer(String objectComputer) { this.objectComputer = objectComputer; }
    
    public String getSpecialNames() { return specialNames; }
    public void setSpecialNames(String specialNames) { this.specialNames = specialNames; }
    
    public String getFileControl() { return fileControl; }
    public void setFileControl(String fileControl) { this.fileControl = fileControl; }
    
    public String getIoControl() { return ioControl; }
    public void setIoControl(String ioControl) { this.ioControl = ioControl; }
    
    public String toJavaComment() {
        StringBuilder sb = new StringBuilder();
        sb.append("    // Environment Configuration\n");
        if (sourceComputer != null) {
            sb.append("    // Source Computer: ").append(sourceComputer).append("\n");
        }
        if (objectComputer != null) {
            sb.append("    // Object Computer: ").append(objectComputer).append("\n");
        }
        if (fileControl != null) {
            sb.append("    // File Control: Present\n");
        }
        return sb.toString();
    }
}