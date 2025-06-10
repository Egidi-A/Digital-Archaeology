package com.DigArch.cobol2java.ast.mapping;

import koopa.core.trees.Tree;
import com.DigArch.cobol2java.model.JavaField;
import com.DigArch.cobol2java.model.JavaMethod;
import java.util.ArrayList;
import java.util.List;

/**
 * Classe che converte elementi AST COBOL nei loro equivalenti Java.
 * Gestisce la conversione di elementi dati COBOL e paragrafi in
 * campi e metodi Java corrispondenti.
 */
public class CobolToJavaMapper {
    
    private final DataTypeMapper dataTypeMapper;
    
    /**
     * Costruisce un nuovo CobolToJavaMapper con una nuova istanza di DataTypeMapper.
     */
    public CobolToJavaMapper() {
        this.dataTypeMapper = new DataTypeMapper();
    }
    
    /**
     * Converte un elemento dati COBOL in un campo Java.
     *
     * @param dataItem Il nodo AST dell'elemento dati COBOL
     * @return Oggetto JavaField che rappresenta il campo mappato, o null se mancano informazioni necessarie
     */
    public JavaField mapDataItem(Tree dataItem) {
        String name = extractDataName(dataItem);
        String picture = extractPictureClause(dataItem);
        
        if (name == null || picture == null) {
            return null;
        }
        
        JavaField field = new JavaField();
        field.setName(toJavaFieldName(name));
        field.setType(dataTypeMapper.mapPictureToJavaType(picture));
        
        String value = extractInitialValue(dataItem);
        if (value != null) {
            field.setInitialValue(dataTypeMapper.mapValue(value, field.getType()));
        }
        
        return field;
    }
    
    /**
     * Converte un paragrafo COBOL in un metodo Java.
     *
     * @param paragraph Il nodo AST del paragrafo COBOL
     * @return Oggetto JavaMethod che rappresenta il metodo mappato, o null se manca il nome del paragrafo
     */
    public JavaMethod mapParagraph(Tree paragraph) {
        String name = extractParagraphName(paragraph);
        if (name == null) return null;
        
        JavaMethod method = new JavaMethod();
        method.setName(toJavaMethodName(name));
        method.setReturnType("void");
        
        List<String> statements = new ArrayList<>();
        for (Tree child : paragraph.childTrees()) {
            if ("sentence".equals(child.getName())) {
                statements.add(child.getProgramText());
            }
        }
        method.setStatements(statements);
        
        return method;
    }
    
    /**
     * Estrae il nome dati da un nodo elemento dati COBOL.
     *
     * @param dataItem Il nodo dell'elemento dati
     * @return Il nome dati estratto, o null se non trovato
     */
    private String extractDataName(Tree dataItem) {
        Tree nameNode = dataItem.getChild("dataName");
        return nameNode != null ? nameNode.getProgramText().trim() : null;
    }
    
    /**
     * Estrae la clausola PICTURE da un nodo elemento dati COBOL.
     *
     * @param dataItem Il nodo dell'elemento dati
     * @return La clausola PICTURE estratta, o null se non trovata
     */
    private String extractPictureClause(Tree dataItem) {
        Tree picNode = dataItem.getChild("pictureClause");
        return picNode != null ? picNode.getProgramText().trim() : null;
    }
    
    /**
     * Estrae il valore iniziale da un nodo elemento dati COBOL.
     *
     * @param dataItem Il nodo dell'elemento dati
     * @return Il valore iniziale estratto, o null se non trovato
     */
    private String extractInitialValue(Tree dataItem) {
        Tree valueNode = dataItem.getChild("valueClause");
        return valueNode != null ? valueNode.getProgramText().trim() : null;
    }
    
    /**
     * Estrae il nome del paragrafo da un nodo paragrafo COBOL.
     *
     * @param paragraph Il nodo del paragrafo
     * @return Il nome del paragrafo estratto, o null se non trovato
     */
    private String extractParagraphName(Tree paragraph) {
        Tree nameNode = paragraph.getChild("paragraphName");
        return nameNode != null ? nameNode.getProgramText().trim() : null;
    }
    
    /**
     * Converte un nome campo COBOL in un nome campo Java valido.
     * Sostituisce i trattini con underscore e applica il camelCase.
     *
     * @param cobolName Il nome campo COBOL originale
     * @return Il nome campo Java convertito
     */
    private String toJavaFieldName(String cobolName) {
        return toCamelCase(cobolName.replace("-", "_"), false);
    }
    
    /**
     * Converte un nome paragrafo COBOL in un nome metodo Java valido.
     * Sostituisce i trattini con underscore e applica il camelCase.
     *
     * @param cobolName Il nome paragrafo COBOL originale
     * @return Il nome metodo Java convertito
     */
    private String toJavaMethodName(String cobolName) {
        return toCamelCase(cobolName.replace("-", "_"), false);
    }
    
    /**
     * Converte una stringa in formato camelCase.
     *
     * @param input La stringa da convertire
     * @param capitalizeFirst Se capitalizzare il primo carattere
     * @return La stringa convertita in camelCase
     */
    private String toCamelCase(String input, boolean capitalizeFirst) {
        StringBuilder result = new StringBuilder();
        boolean nextCapital = capitalizeFirst;
        
        for (char c : input.toCharArray()) {
            if (c == '_' || c == '-') {
                nextCapital = true;
            } else if (nextCapital) {
                result.append(Character.toUpperCase(c));
                nextCapital = false;
            } else {
                result.append(Character.toLowerCase(c));
            }
        }
        
        return result.toString();
    }
}