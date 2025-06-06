package com.cobolconverter.model;

public class CobolProgram {
    private IdentificationDivision identificationDivision;
    private EnvironmentDivision environmentDivision;
    private DataDivision dataDivision;

    //IDENTIFICATION DIVISION
    public IdentificationDivision getIdentificationDivision() {
        return identificationDivision;
    }
    
    public void setIdentificationDivision(IdentificationDivision identificationDivision) {
        this.identificationDivision = identificationDivision;
    }
    
    //ENVIRONMENT DIVISION
    public EnvironmentDivision getEnvironmentDivision() {
        return environmentDivision;
    }
        public void setEnvironmentDivision(EnvironmentDivision environmentDivision) {
        this.environmentDivision = environmentDivision;
    }

    //DATA DIVISION
    public DataDivision getDataDivision() {
        return dataDivision;
    }
    
    public void setDataDivision(DataDivision dataDivision) {
        this.dataDivision = dataDivision;
    }
}