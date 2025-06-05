package com.cobolconverter.model;

public class CobolProgram {
    private IdentificationDivision identificationDivision;
    private EnvironmentDivision environmentDivision;
    
    public IdentificationDivision getIdentificationDivision() {
        return identificationDivision;
    }
    
    public void setIdentificationDivision(IdentificationDivision identificationDivision) {
        this.identificationDivision = identificationDivision;
    }
    
    public EnvironmentDivision getEnvironmentDivision() {
        return environmentDivision;
    }
    
    public void setEnvironmentDivision(EnvironmentDivision environmentDivision) {
        this.environmentDivision = environmentDivision;
    }
}