package com.cobolconverter.model;

/**
 * Rappresenta un programma COBOL suddiviso nelle sue principali divisioni:
 * Identification, Environment, Data e Procedure.
 * Ogni divisione è modellata come un oggetto separato.
 */
public class CobolProgram {

    /**
     * Divisione IDENTIFICATION del programma COBOL.
     */
    private IdentificationDivision identificationDivision;
    /**
     * Divisione ENVIRONMENT del programma COBOL.
     */
    private EnvironmentDivision environmentDivision;
    /**
     * Divisione DATA del programma COBOL.
     */
    private DataDivision dataDivision;
    /**
     * Divisione PROCEDURE del programma COBOL.
     */
    private ProcedureDivision procedureDivision;

    //IDENTIFICATION DIVISION
    /**
     * Restituisce la divisione IDENTIFICATION.
     * @return l'oggetto IdentificationDivision
     */
    public IdentificationDivision getIdentificationDivision() {
        return identificationDivision;
    }
    /**
     * Imposta la divisione IDENTIFICATION.
     * @param identificationDivision l'oggetto IdentificationDivision da impostare
     */
    public void setIdentificationDivision(IdentificationDivision identificationDivision) {
        this.identificationDivision = identificationDivision;
    }
    
    //ENVIRONMENT DIVISION
    /**
     * Restituisce la divisione ENVIRONMENT.
     * @return l'oggetto EnvironmentDivision
     */
    public EnvironmentDivision getEnvironmentDivision() {
        return environmentDivision;
    }
    /**
     * Imposta la divisione ENVIRONMENT.
     * @param environmentDivision l'oggetto EnvironmentDivision da impostare
     */
        public void setEnvironmentDivision(EnvironmentDivision environmentDivision) {
        this.environmentDivision = environmentDivision;
    }

    //DATA DIVISION
    /**
     * Restituisce la divisione DATA.
     * @return l'oggetto DataDivision
     */
    public DataDivision getDataDivision() {
        return dataDivision;
    }
    /**
     * Imposta la divisione DATA.
     * @param dataDivision l'oggetto DataDivision da impostare
     */
    public void setDataDivision(DataDivision dataDivision) {
        this.dataDivision = dataDivision;
    }

    //PROCEDURE DIVISION
    /**
     * Restituisce la divisione PROCEDURE.
     * @return l'oggetto ProcedureDivision
     */
    public ProcedureDivision getProcedureDivision() {
        return procedureDivision;
    }
    /**
     * Imposta la divisione PROCEDURE.
     * @param procedureDivision l'oggetto ProcedureDivision da impostare
     */
    public void setProcedureDivision(ProcedureDivision procedureDivision) {
        this.procedureDivision = procedureDivision;
    }
}