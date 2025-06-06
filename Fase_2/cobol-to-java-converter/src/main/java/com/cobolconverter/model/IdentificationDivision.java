package com.cobolconverter.model;

/**
 * Rappresenta la IDENTIFICATION DIVISION di un programma COBOL.
 * <p>
 * Questa classe modella i principali campi della sezione Identification:
 * <ul>
 *   <li><b>PROGRAM-ID</b>: nome identificativo del programma COBOL.</li>
 *   <li><b>AUTHOR</b>: autore del programma.</li>
 *   <li><b>INSTALLATION</b>: informazioni sull'installazione o ambiente.</li>
 *   <li><b>DATE-WRITTEN</b>: data di scrittura del programma.</li>
 *   <li><b>DATE-COMPILED</b>: data di compilazione del programma.</li>
 *   <li><b>SECURITY</b>: eventuali informazioni di sicurezza.</li>
 *   <li><b>REMARKS</b>: commenti o note aggiuntive.</li>
 * </ul>
 * Ogni campo corrisponde a una clausola COBOL e può essere opzionale.
 */
public class IdentificationDivision {
    /** Nome identificativo del programma (PROGRAM-ID). */
    private String programId;
    /** Autore del programma (AUTHOR). */
    private String author;
    /** Informazioni sull'installazione (INSTALLATION). */
    private String installation;
    /** Data di scrittura del programma (DATE-WRITTEN). */
    private String dateWritten;
    /** Data di compilazione del programma (DATE-COMPILED). */
    private String dateCompiled;
    /** Informazioni di sicurezza (SECURITY). */
    private String security;
    /** Commenti o note aggiuntive (REMARKS). */
    private String remarks;
    
    // Getters and setters

    /**
     * Restituisce il nome identificativo del programma (PROGRAM-ID).
     * @return programId
     */
    public String getProgramId() { return programId; }

    /**
     * Imposta il nome identificativo del programma (PROGRAM-ID).
     * @param programId nome del programma
     */
    public void setProgramId(String programId) { this.programId = programId; }

    /**
     * Restituisce l'autore del programma (AUTHOR).
     * @return autore
     */
    public String getAuthor() { return author; }

    /**
     * Imposta l'autore del programma (AUTHOR).
     * @param author autore
     */
    public void setAuthor(String author) { this.author = author; }

    /**
     * Restituisce le informazioni sull'installazione (INSTALLATION).
     * @return installazione
     */
    public String getInstallation() { return installation; }

    /**
     * Imposta le informazioni sull'installazione (INSTALLATION).
     * @param installation installazione
     */
    public void setInstallation(String installation) { this.installation = installation; }

    /**
     * Restituisce la data di scrittura del programma (DATE-WRITTEN).
     * @return data scrittura
     */
    public String getDateWritten() { return dateWritten; }

    /**
     * Imposta la data di scrittura del programma (DATE-WRITTEN).
     * @param dateWritten data scrittura
     */
    public void setDateWritten(String dateWritten) { this.dateWritten = dateWritten; }

    /**
     * Restituisce la data di compilazione del programma (DATE-COMPILED).
     * @return data compilazione
     */
    public String getDateCompiled() { return dateCompiled; }

    /**
     * Imposta la data di compilazione del programma (DATE-COMPILED).
     * @param dateCompiled data compilazione
     */
    public void setDateCompiled(String dateCompiled) { this.dateCompiled = dateCompiled; }

    /**
     * Restituisce le informazioni di sicurezza (SECURITY).
     * @return security
     */
    public String getSecurity() { return security; }

    /**
     * Imposta le informazioni di sicurezza (SECURITY).
     * @param security informazioni di sicurezza
     */
    public void setSecurity(String security) { this.security = security; }

    /**
     * Restituisce i commenti o note aggiuntive (REMARKS).
     * @return remarks
     */
    public String getRemarks() { return remarks; }

    /**
     * Imposta i commenti o note aggiuntive (REMARKS).
     * @param remarks commenti o note
     */
    public void setRemarks(String remarks) { this.remarks = remarks; }
}