package com.DigArch.cobol2java.ast.visitor;

import koopa.core.trees.Tree;
import koopa.core.trees.TreeWalker;
import com.DigArch.cobol2java.model.JavaClass;
import com.DigArch.cobol2java.model.JavaMethod;
import com.DigArch.cobol2java.model.JavaField;
import com.DigArch.cobol2java.ast.mapping.CobolToJavaMapper;

import java.util.ArrayList;
import java.util.List;

/**
 * Visitor per l'attraversamento dell'AST COBOL.
 * Questa classe si occupa di visitare l'albero sintattico COBOL e convertire
 * le sue componenti in una classe Java equivalente, utilizzando il CobolToJavaMapper
 * per le conversioni specifiche di elementi.
 */
public class CobolTreeVisitor {
    
    private final CobolToJavaMapper mapper;
    private VisitorContext context;
    
    /**
     * Costruisce un nuovo visitor con un mapper dedicato per le conversioni.
     */
    public CobolTreeVisitor() {
        this.mapper = new CobolToJavaMapper();
    }
    
    /**
     * Visita l'albero sintattico COBOL e costruisce una classe Java equivalente.
     * Attraversa tutti i nodi dell'albero processando identification, data e procedure division.
     *
     * @param ast L'albero sintattico COBOL da visitare
     * @return JavaClass contenente la conversione completa del programma COBOL
     */
    public JavaClass visit(Tree ast) {
        context = new VisitorContext();
        TreeWalker walker = new TreeWalker(ast);
        
        Tree node;
        while ((node = walker.next()) != null) {
            processNode(node);
        }
        
        return buildJavaClass();
    }
    
    /**
     * Processa un singolo nodo dell'AST in base al suo tipo.
     * Gestisce i nodi principali di un programma COBOL:
     * - programName
     * - identificationDivision
     * - dataDivision
     * - procedureDivision
     *
     * @param node Il nodo da processare
     */
    private void processNode(Tree node) {
        if (!node.isNode()) return;
        
        String nodeName = node.getName();
        if (nodeName == null) return;
        
        switch (nodeName) {
            case "programName":
                context.setProgramName(node.getProgramText());
                break;
            case "identificationDivision":
                processIdentificationDivision(node);
                break;
            case "dataDivision":
                processDataDivision(node);
                break;
            case "procedureDivision":
                processProcedureDivision(node);
                break;
        }
    }
    
    /**
     * Elabora la identification division estraendo i metadati del programma.
     *
     * @param node Il nodo della identification division
     */
    private void processIdentificationDivision(Tree node) {
        // Extract program metadata
        Tree programId = node.getChild("programID");
        if (programId != null) {
            context.setProgramName(programId.getProgramText().trim());
        }
    }
    
    /**
     * Elabora la data division cercando la working storage section.
     *
     * @param node Il nodo della data division
     */
    private void processDataDivision(Tree node) {
        Tree workingStorage = node.getChild("workingStorageSection");
        if (workingStorage != null) {
            processWorkingStorage(workingStorage);
        }
    }
    
    /**
     * Elabora la working storage section convertendo le definizioni dei dati
     * in campi Java equivalenti.
     *
     * @param node Il nodo della working storage section
     */
    private void processWorkingStorage(Tree node) {
        for (Tree child : node.childTrees()) {
            if ("dataDescriptionEntry".equals(child.getName())) {
                JavaField field = mapper.mapDataItem(child);
                if (field != null) {
                    context.addField(field);
                }
            }
        }
    }
    
    /**
     * Elabora la procedure division convertendo i paragrafi COBOL
     * in metodi Java equivalenti.
     *
     * @param node Il nodo della procedure division
     */
    private void processProcedureDivision(Tree node) {
        for (Tree child : node.childTrees()) {
            if ("paragraph".equals(child.getName())) {
                JavaMethod method = mapper.mapParagraph(child);
                if (method != null) {
                    context.addMethod(method);
                }
            }
        }
    }
    
    /**
     * Costruisce la classe Java finale utilizzando tutte le informazioni
     * raccolte durante la visita dell'AST.
     *
     * @return JavaClass completamente popolata con campi e metodi
     */
    private JavaClass buildJavaClass() {
        JavaClass javaClass = new JavaClass(context.getClassName());
        javaClass.setFields(context.getFields());
        javaClass.setMethods(context.getMethods());
        return javaClass;
    }
}