package com.example;

import org.w3c.dom.Document;
import org.w3c.dom.Element;
import org.w3c.dom.Node;
import org.w3c.dom.NodeList;

import javax.xml.parsers.DocumentBuilder;
import javax.xml.parsers.DocumentBuilderFactory;
import javax.xml.transform.OutputKeys;
import javax.xml.transform.Transformer;
import javax.xml.transform.TransformerFactory;
import javax.xml.transform.dom.DOMSource;
import javax.xml.transform.stream.StreamResult;
import java.io.File;
import java.util.ArrayList;
import java.util.List;

public class CobolToJavaXMLTransformer {

    public static void main(String[] args) {
        try {
            File inputFile = new File("output/ASG_COBOL.xml");
            DocumentBuilderFactory dbFactory = DocumentBuilderFactory.newInstance();
            DocumentBuilder dBuilder = dbFactory.newDocumentBuilder();
            Document cobolDoc = dBuilder.parse(inputFile);
            cobolDoc.getDocumentElement().normalize();

            Document javaDoc = dBuilder.newDocument();
            Element javaRoot = javaDoc.createElement("java-compilation-unit");
            javaRoot.setAttribute("package", "com.generated");
            javaDoc.appendChild(javaRoot);

            transformCobolProgram(cobolDoc.getDocumentElement(), javaDoc, javaRoot);

            TransformerFactory transformerFactory = TransformerFactory.newInstance();
            Transformer transformer = transformerFactory.newTransformer();
            transformer.setOutputProperty(OutputKeys.INDENT, "yes");
            transformer.setOutputProperty("{http://xml.apache.org/xslt}indent-amount", "2");

            DOMSource source = new DOMSource(javaDoc);
            StreamResult result = new StreamResult(new File("output/ASG_Java.xml"));
            transformer.transform(source, result);

            System.out.println("Java ASG XML (da Parse Tree) salvato in: output/ASG_Java.xml");

        } catch (Exception e) {
            e.printStackTrace();
        }
    }

    private static void transformCobolProgram(Element cobolRoot, Document javaDoc, Element javaRoot) {
        NodeList programIdNodes = cobolRoot.getElementsByTagName("programIdParagraph");
        String className = "DefaultClassName";
        if (programIdNodes.getLength() > 0) {
            Node programNameNode = findFirstChildNodeByName(programIdNodes.item(0), "programName");
            if (programNameNode != null) {
                className = sanitizeNameForClass(programNameNode.getTextContent());
            }
        }

        Element javaClass = javaDoc.createElement("class");
        javaClass.setAttribute("name", className);
        javaClass.setAttribute("visibility", "public");
        javaRoot.appendChild(javaClass);

        NodeList dataDivisions = cobolRoot.getElementsByTagName("dataDivision");
        if (dataDivisions.getLength() > 0) {
            processDataDivision((Element) dataDivisions.item(0), javaDoc, javaClass);
        }

        NodeList procDivisions = cobolRoot.getElementsByTagName("procedureDivision");
        if (procDivisions.getLength() > 0) {
            processProcedureDivision((Element) procDivisions.item(0), javaDoc, javaClass);
        }
    }

    private static void processDataDivision(Element dataDiv, Document javaDoc, Element javaClass) {
        NodeList dataEntries = dataDiv.getElementsByTagName("dataDescriptionEntry");
        for (int i = 0; i < dataEntries.getLength(); i++) {
            Element dataEntry = (Element) dataEntries.item(i);
            Node dataNameNode = findFirstChildNodeByName(dataEntry, "dataName");
            if (dataNameNode == null) continue;

            String fieldName = sanitizeNameForIdentifier(dataNameNode.getTextContent());
            String fieldType = "Object"; // Default

            Node picClauseNode = findFirstChildNodeByName(dataEntry, "pictureClause");
            if (picClauseNode != null) {
                fieldType = mapCobolTypeToJava(picClauseNode.getTextContent());
            }

            Element field = javaDoc.createElement("field");
            field.setAttribute("name", fieldName);
            field.setAttribute("type", fieldType);
            field.setAttribute("visibility", "private");
            javaClass.appendChild(field);

            createGetterSetter(fieldName, fieldType, javaDoc, javaClass);
        }
    }

    private static void processProcedureDivision(Element procDiv, Document javaDoc, Element javaClass) {
        NodeList paragraphs = procDiv.getElementsByTagName("paragraph");
        List<String> paragraphNames = new ArrayList<>();

        for (int i = 0; i < paragraphs.getLength(); i++) {
            Element paragraph = (Element) paragraphs.item(i);
            Node paragraphNameNode = findFirstChildNodeByName(paragraph, "paragraphName");
            if (paragraphNameNode == null) continue;

            String methodName = sanitizeNameForIdentifier(paragraphNameNode.getTextContent());
            paragraphNames.add(methodName);

            Element method = javaDoc.createElement("method");
            method.setAttribute("name", methodName);
            method.setAttribute("visibility", "private");
            method.setAttribute("returnType", "void");
            javaClass.appendChild(method);

            Element body = javaDoc.createElement("body");
            method.appendChild(body);

            NodeList statements = paragraph.getElementsByTagName("statement");
            for (int j = 0; j < statements.getLength(); j++) {
                Element statementTypeNode = (Element) findFirstChildNode(statements.item(j));
                if (statementTypeNode != null) {
                    processStatement(statementTypeNode, javaDoc, body);
                }
            }
        }
        createMainMethod(paragraphNames, javaDoc, javaClass);
    }

    private static void processStatement(Element stmtNode, Document javaDoc, Element methodBody) {
        String stmtType = stmtNode.getTagName();
        Element javaStmt = javaDoc.createElement("statement");
        javaStmt.setAttribute("cobolType", stmtType);

        if ("displayStatement".equals(stmtType)) {
            javaStmt.setAttribute("type", "println");
            Node operandNode = findFirstChildNodeByName(stmtNode, "displayOperand");
            if (operandNode != null) {
                String operandText = operandNode.getTextContent().trim();
                javaStmt.setAttribute("expression", "System.out.println(" + operandText + ")");
            } else {
                javaStmt.setAttribute("expression", "System.out.println()"); // fallback
            }
        } else if ("moveStatement".equals(stmtType)) {
            javaStmt.setAttribute("type", "assignment");
            Node targetNode = findFirstChildNodeByName(stmtNode, "identifier");
            Node literalNode = findFirstChildNodeByName(stmtNode, "literal");
            Node sourceIdentifierNode = findFirstChildNodeByName(stmtNode, "identifier", 1);

            if (targetNode != null) {
                String target = sanitizeNameForIdentifier(targetNode.getTextContent());
                String source = "\"\""; // Default
                if (literalNode != null) {
                    source = literalNode.getTextContent();
                } else if (sourceIdentifierNode != null) {
                    source = sanitizeNameForIdentifier(sourceIdentifierNode.getTextContent());
                }
                javaStmt.setAttribute("expression", target + " = " + source + ";");
            } else {
                javaStmt.setAttribute("type", "unknown_assignment");
            }
        } else if ("performStatement".equals(stmtType)) {
            javaStmt.setAttribute("type", "method-call");
            // CORREZIONE: Cerca 'procedureName', non 'paragraphName' e aggiunge un controllo
            Node procedureNameNode = findFirstChildNodeByName(stmtNode, "procedureName");
            if (procedureNameNode != null) {
                String paragraphToCall = sanitizeNameForIdentifier(procedureNameNode.getTextContent());
                javaStmt.setAttribute("name", paragraphToCall);
            } else {
                javaStmt.setAttribute("name", "unknown_procedure_call");
            }
        } else if ("stopStatement".equals(stmtType)) {
            javaStmt.setAttribute("type", "exit");
            javaStmt.setAttribute("expression", "System.exit(0)");
        } else {
            javaStmt.setAttribute("type", "unknown");
        }
        methodBody.appendChild(javaStmt);
    }

    private static void createMainMethod(List<String> paragraphsToCall, Document javaDoc, Element javaClass) {
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
        Element instanceCreation = javaDoc.createElement("statement");
        instanceCreation.setAttribute("type", "instantiation");
        String className = javaClass.getAttribute("name");
        instanceCreation.setAttribute("expression", className + " app = new " + className + "();");
        body.appendChild(instanceCreation);
        for (String paraName : paragraphsToCall) {
            Element methodCall = javaDoc.createElement("method-call");
            methodCall.setAttribute("name", "app." + paraName);
            body.appendChild(methodCall);
        }
        javaClass.appendChild(mainMethod);
    }

    private static void createGetterSetter(String fieldName, String fieldType, Document javaDoc, Element javaClass) {
        String capitalizedName = fieldName.substring(0, 1).toUpperCase() + fieldName.substring(1);
        Element getter = javaDoc.createElement("method");
        getter.setAttribute("name", "get" + capitalizedName);
        getter.setAttribute("visibility", "public");
        getter.setAttribute("returnType", fieldType);
        Element getterBody = javaDoc.createElement("body");
        getterBody.setAttribute("return", fieldName);
        getter.appendChild(getterBody);
        javaClass.appendChild(getter);
        Element setter = javaDoc.createElement("method");
        setter.setAttribute("name", "set" + capitalizedName);
        setter.setAttribute("visibility", "public");
        setter.setAttribute("returnType", "void");
        Element param = javaDoc.createElement("parameter");
        param.setAttribute("name", "value");
        param.setAttribute("type", fieldType);
        setter.appendChild(param);
        Element setterBody = javaDoc.createElement("body");
        setterBody.setAttribute("assignment", fieldName + " = value");
        setter.appendChild(setterBody);
        javaClass.appendChild(setter);
    }

    private static String mapCobolTypeToJava(String picClause) {
        picClause = picClause.toUpperCase();
        if (picClause.contains("X")) return "String";
        if (picClause.contains("9")) {
            if (picClause.contains("V9") || picClause.contains(".9")) {
                return "java.math.BigDecimal";
            }
            if (picClause.length() > 9) {
                return "long";
            }
            return "int";
        }
        return "Object";
    }

    private static String sanitizeNameForClass(String name) {
        String[] parts = name.trim().split("-");
        StringBuilder sb = new StringBuilder();
        for (String part : parts) {
            if (part.isEmpty()) continue;
            sb.append(part.substring(0, 1).toUpperCase());
            sb.append(part.substring(1).toLowerCase());
        }
        return sb.toString();
    }

    private static String sanitizeNameForIdentifier(String name) {
        String sanitized = sanitizeNameForClass(name);
        return sanitized.substring(0, 1).toLowerCase() + sanitized.substring(1);
    }

    private static Node findFirstChildNodeByName(Node parent, String name) {
        return findFirstChildNodeByName(parent, name, 0);
    }

    private static Node findFirstChildNodeByName(Node parent, String name, int occurrence) {
        int count = 0;
        NodeList children = parent.getChildNodes();
        for (int i = 0; i < children.getLength(); i++) {
            Node child = children.item(i);
            if (child.getNodeType() == Node.ELEMENT_NODE && name.equals(child.getNodeName())) {
                if (count == occurrence) {
                    return child;
                }
                count++;
            }
        }
        return null;
    }

    private static Node findFirstChildNode(Node parent) {
        NodeList children = parent.getChildNodes();
        for (int i = 0; i < children.getLength(); i++) {
            if (children.item(i).getNodeType() == Node.ELEMENT_NODE) {
                return children.item(i);
            }
        }
        return null;
    }
}