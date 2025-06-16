package com.example;

import javax.xml.parsers.*;
import javax.xml.transform.*;
import javax.xml.transform.dom.DOMSource;
import javax.xml.transform.stream.StreamResult;
import org.w3c.dom.*;
import java.io.File;

public class CobolToJavaXMLTransformer {
    
    public static void main(String[] args) {
        try {
            // Legge XML COBOL ASG
            File inputFile = new File("output/ast.xml");
            DocumentBuilderFactory dbFactory = DocumentBuilderFactory.newInstance();
            DocumentBuilder dBuilder = dbFactory.newDocumentBuilder();
            Document cobolDoc = dBuilder.parse(inputFile);
            cobolDoc.getDocumentElement().normalize();
            
            // Crea nuovo documento per Java ASG
            Document javaDoc = dBuilder.newDocument();
            
            // Root element
            Element javaRoot = javaDoc.createElement("java-compilation-unit");
            javaRoot.setAttribute("package", "com.generated");
            javaDoc.appendChild(javaRoot);
            
            // Processa compilation units COBOL
            NodeList compUnits = cobolDoc.getElementsByTagName("compilation-unit");
            for (int i = 0; i < compUnits.getLength(); i++) {
                Element cobolUnit = (Element) compUnits.item(i);
                processCompilationUnit(cobolUnit, javaDoc, javaRoot);
            }
            
            // Salva XML Java ASG
            TransformerFactory transformerFactory = TransformerFactory.newInstance();
            Transformer transformer = transformerFactory.newTransformer();
            transformer.setOutputProperty(OutputKeys.INDENT, "yes");
            transformer.setOutputProperty("{http://xml.apache.org/xslt}indent-amount", "2");
            
            DOMSource source = new DOMSource(javaDoc);
            StreamResult result = new StreamResult(new File("output/java-ast.xml"));
            transformer.transform(source, result);
            
            System.out.println("Java ASG XML salvato in: output/java-ast.xml");
            
        } catch (Exception e) {
            e.printStackTrace();
        }
    }
    
    private static void processCompilationUnit(Element cobolUnit, Document javaDoc, Element javaRoot) {
        NodeList programUnits = cobolUnit.getElementsByTagName("program-unit");
        
        for (int i = 0; i < programUnits.getLength(); i++) {
            Element programUnit = (Element) programUnits.item(i);
            String className = sanitizeName(programUnit.getAttribute("name"));
            
            // Crea classe Java
            Element javaClass = javaDoc.createElement("class");
            javaClass.setAttribute("name", className);
            javaClass.setAttribute("visibility", "public");
            javaRoot.appendChild(javaClass);
            
            // Processa DATA DIVISION → fields
            NodeList dataDivisions = programUnit.getElementsByTagName("data-division");
            if (dataDivisions.getLength() > 0) {
                processDataDivision((Element) dataDivisions.item(0), javaDoc, javaClass);
            }
            
            // Processa PROCEDURE DIVISION → methods
            NodeList procDivisions = programUnit.getElementsByTagName("procedure-division");
            if (procDivisions.getLength() > 0) {
                processProcedureDivision((Element) procDivisions.item(0), javaDoc, javaClass);
            }
        }
    }
    
    private static void processDataDivision(Element dataDiv, Document javaDoc, Element javaClass) {
        NodeList dataEntries = dataDiv.getElementsByTagName("data-entry");
        
        for (int i = 0; i < dataEntries.getLength(); i++) {
            Element dataEntry = (Element) dataEntries.item(i);
            
            // Crea field Java
            Element field = javaDoc.createElement("field");
            field.setAttribute("name", sanitizeName(dataEntry.getAttribute("name")));
            field.setAttribute("type", mapCobolTypeToJava(dataEntry));
            field.setAttribute("visibility", "private");
            javaClass.appendChild(field);
            
            // Crea getter/setter
            createGetterSetter(dataEntry, javaDoc, javaClass);
        }
    }
    
    private static void processProcedureDivision(Element procDiv, Document javaDoc, Element javaClass) {
        // Crea main method
        Element mainMethod = javaDoc.createElement("method");
        mainMethod.setAttribute("name", "main");
        mainMethod.setAttribute("visibility", "public");
        mainMethod.setAttribute("static", "true");
        mainMethod.setAttribute("returnType", "void");
        
        Element parameter = javaDoc.createElement("parameter");
        parameter.setAttribute("name", "args");
        parameter.setAttribute("type", "String[]");
        mainMethod.appendChild(parameter);
        
        Element body = javaDoc.createElement("body");
        mainMethod.appendChild(body);
        
        // Processa paragraphs
        NodeList paragraphs = procDiv.getElementsByTagName("paragraph");
        for (int i = 0; i < paragraphs.getLength(); i++) {
            Element paragraph = (Element) paragraphs.item(i);
            processParagraph(paragraph, javaDoc, javaClass, body);
        }
        
        javaClass.appendChild(mainMethod);
    }
    
    private static void processParagraph(Element paragraph, Document javaDoc, Element javaClass, Element mainBody) {
        String methodName = sanitizeName(paragraph.getAttribute("name"));
        
        // Crea metodo per paragraph
        Element method = javaDoc.createElement("method");
        method.setAttribute("name", methodName);
        method.setAttribute("visibility", "private");
        method.setAttribute("returnType", "void");
        
        Element methodBody = javaDoc.createElement("body");
        method.appendChild(methodBody);
        
        // Processa statements
        NodeList statements = paragraph.getElementsByTagName("statement");
        for (int i = 0; i < statements.getLength(); i++) {
            Element stmt = (Element) statements.item(i);
            processStatement(stmt, javaDoc, methodBody);
        }
        
        javaClass.appendChild(method);
        
        // Aggiungi chiamata in main
        Element methodCall = javaDoc.createElement("method-call");
        methodCall.setAttribute("name", methodName);
        mainBody.appendChild(methodCall);
    }
    
    private static void processStatement(Element stmt, Document javaDoc, Element methodBody) {
        String stmtType = stmt.getAttribute("type");
        
        Element javaStmt = javaDoc.createElement("statement");
        javaStmt.setAttribute("type", mapStatementType(stmtType));
        
        if (stmtType.contains("DISPLAY")) {
            javaStmt.setAttribute("expression", "System.out.println()");
        } else if (stmtType.contains("STOP")) {
            javaStmt.setAttribute("expression", "System.exit(0)");
        }
        
        methodBody.appendChild(javaStmt);
    }
    
    private static void createGetterSetter(Element dataEntry, Document javaDoc, Element javaClass) {
        String fieldName = sanitizeName(dataEntry.getAttribute("name"));
        String capitalizedName = fieldName.substring(0, 1).toUpperCase() + fieldName.substring(1);
        String type = mapCobolTypeToJava(dataEntry);
        
        // Getter
        Element getter = javaDoc.createElement("method");
        getter.setAttribute("name", "get" + capitalizedName);
        getter.setAttribute("visibility", "public");
        getter.setAttribute("returnType", type);
        Element getterBody = javaDoc.createElement("body");
        getterBody.setAttribute("return", fieldName);
        getter.appendChild(getterBody);
        javaClass.appendChild(getter);
        
        // Setter
        Element setter = javaDoc.createElement("method");
        setter.setAttribute("name", "set" + capitalizedName);
        setter.setAttribute("visibility", "public");
        setter.setAttribute("returnType", "void");
        Element param = javaDoc.createElement("parameter");
        param.setAttribute("name", "value");
        param.setAttribute("type", type);
        setter.appendChild(param);
        Element setterBody = javaDoc.createElement("body");
        setterBody.setAttribute("assignment", fieldName + " = value");
        setter.appendChild(setterBody);
        javaClass.appendChild(setter);
    }
    
    private static String mapCobolTypeToJava(Element dataEntry) {
        // Semplificato - estendere con analisi PIC
        return "String";
    }
    
    private static String mapStatementType(String cobolType) {
        if (cobolType.contains("DISPLAY")) return "println";
        if (cobolType.contains("STOP")) return "exit";
        if (cobolType.contains("MOVE")) return "assignment";
        return "unknown";
    }
    
    private static String sanitizeName(String name) {
        return name.toLowerCase().replace("-", "_").replace(" ", "_");
    }
}