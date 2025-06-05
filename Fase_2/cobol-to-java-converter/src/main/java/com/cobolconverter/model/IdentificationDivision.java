package com.cobolconverter.model;

public class IdentificationDivision {
    private String programId;
    private String author;
    private String installation;
    private String dateWritten;
    private String dateCompiled;
    private String security;
    private String remarks;
    
    // Getters and setters
    public String getProgramId() { return programId; }
    public void setProgramId(String programId) { this.programId = programId; }
    
    public String getAuthor() { return author; }
    public void setAuthor(String author) { this.author = author; }
    
    public String getInstallation() { return installation; }
    public void setInstallation(String installation) { this.installation = installation; }
    
    public String getDateWritten() { return dateWritten; }
    public void setDateWritten(String dateWritten) { this.dateWritten = dateWritten; }
    
    public String getDateCompiled() { return dateCompiled; }
    public void setDateCompiled(String dateCompiled) { this.dateCompiled = dateCompiled; }
    
    public String getSecurity() { return security; }
    public void setSecurity(String security) { this.security = security; }
    
    public String getRemarks() { return remarks; }
    public void setRemarks(String remarks) { this.remarks = remarks; }
}