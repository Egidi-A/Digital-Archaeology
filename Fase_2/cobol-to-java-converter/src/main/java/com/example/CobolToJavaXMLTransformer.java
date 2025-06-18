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
import java.util.*;

public class CobolToJavaXMLTransformer {

    // Map per tenere traccia dei campi dichiarati
    private static Map<String, String> declaredFields = new HashMap<>();
    
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
        // Estrai il nome del programma
        NodeList programIdNodes = cobolRoot.getElementsByTagName("programIdParagraph");
        String className = "DefaultClassName";
        if (programIdNodes.getLength() > 0) {
            Node programNameNode = findDeepChildByName(programIdNodes.item(0), "programName");
            if (programNameNode != null) {
                String programName = extractTextFromNode(programNameNode);
                className = sanitizeNameForClass(programName);
            }
        }

        Element javaClass = javaDoc.createElement("class");
        javaClass.setAttribute("name", className);
        javaClass.setAttribute("visibility", "public");
        javaRoot.appendChild(javaClass);

        // Processa DATA DIVISION
        NodeList dataDivisions = cobolRoot.getElementsByTagName("dataDivision");
        if (dataDivisions.getLength() > 0) {
            processDataDivision((Element) dataDivisions.item(0), javaDoc, javaClass);
        }

        // Processa PROCEDURE DIVISION
        NodeList procDivisions = cobolRoot.getElementsByTagName("procedureDivision");
        if (procDivisions.getLength() > 0) {
            processProcedureDivision((Element) procDivisions.item(0), javaDoc, javaClass);
        }
    }

    private static void processDataDivision(Element dataDiv, Document javaDoc, Element javaClass) {
        NodeList dataEntries = dataDiv.getElementsByTagName("dataDescriptionEntryFormat1");
        
        for (int i = 0; i < dataEntries.getLength(); i++) {
            Element dataEntry = (Element) dataEntries.item(i);
            
            // Estrai il livello
            Node levelNode = findChildNodeByName(dataEntry, "Token");
            if (levelNode == null) continue;
            
            String level = levelNode.getTextContent().trim();
            
            // Estrai il nome del campo
            Node dataNameNode = findDeepChildByName(dataEntry, "dataName");
            if (dataNameNode == null) continue;
            
            String fieldName = extractTextFromNode(dataNameNode);
            fieldName = sanitizeNameForIdentifier(fieldName);
            
            // Processa solo i campi elementari (non i gruppi di livello 01)
            Node picClauseNode = findDeepChildByName(dataEntry, "dataPictureClause");
            if (picClauseNode != null && !level.equals("01")) {
                String pictureString = extractPictureString(picClauseNode);
                String fieldType = mapCobolTypeToJava(pictureString);
                
                // Aggiungi il campo alla mappa
                declaredFields.put(fieldName.toUpperCase(), fieldType);
                
                Element field = javaDoc.createElement("field");
                field.setAttribute("name", fieldName);
                field.setAttribute("type", fieldType);
                field.setAttribute("visibility", "private");
                
                // Estrai il valore iniziale se presente
                Node valueClauseNode = findDeepChildByName(dataEntry, "dataValueClause");
                if (valueClauseNode != null) {
                    String initialValue = extractValueClause(valueClauseNode);
                    field.setAttribute("initialValue", initialValue);
                }
                
                javaClass.appendChild(field);
                
                // Crea getter e setter
                createGetterSetter(fieldName, fieldType, javaDoc, javaClass);
            }
        }
    }

    private static void processProcedureDivision(Element procDiv, Document javaDoc, Element javaClass) {
        NodeList paragraphs = procDiv.getElementsByTagName("paragraph");
        List<String> paragraphNames = new ArrayList<>();

        for (int i = 0; i < paragraphs.getLength(); i++) {
            Element paragraph = (Element) paragraphs.item(i);
            
            // Estrai il nome del paragrafo
            Node paragraphNameNode = findDeepChildByName(paragraph, "paragraphName");
            if (paragraphNameNode == null) continue;

            String methodName = extractTextFromNode(paragraphNameNode);
            methodName = sanitizeNameForIdentifier(methodName);
            paragraphNames.add(methodName);

            Element method = javaDoc.createElement("method");
            method.setAttribute("name", methodName);
            method.setAttribute("visibility", "private");
            method.setAttribute("returnType", "void");
            javaClass.appendChild(method);

            Element body = javaDoc.createElement("body");
            method.appendChild(body);

            // Processa tutti gli statement nel paragrafo
            NodeList sentences = paragraph.getElementsByTagName("sentence");
            for (int j = 0; j < sentences.getLength(); j++) {
                Element sentence = (Element) sentences.item(j);
                NodeList statements = sentence.getElementsByTagName("statement");
                
                for (int k = 0; k < statements.getLength(); k++) {
                    Element statement = (Element) statements.item(k);
                    // Trova il primo figlio elemento che rappresenta il tipo di statement
                    Node statementTypeNode = getFirstElementChild(statement);
                    if (statementTypeNode != null) {
                        processStatement((Element) statementTypeNode, javaDoc, body);
                    }
                }
            }
        }
        
        // Crea il metodo main
        createMainMethod(paragraphNames, javaDoc, javaClass);
    }

    private static void processStatement(Element stmtNode, Document javaDoc, Element methodBody) {
        String stmtType = stmtNode.getTagName();
        Element javaStmt = javaDoc.createElement("statement");
        javaStmt.setAttribute("cobolType", stmtType);

        switch (stmtType) {
            case "displayStatement":
                processDisplayStatement(stmtNode, javaStmt);
                break;
                
            case "moveStatement":
                processMoveStatement(stmtNode, javaStmt);
                break;
                
            case "performStatement":
                processPerformStatement(stmtNode, javaStmt);
                break;
                
            case "stopStatement":
                javaStmt.setAttribute("type", "exit");
                javaStmt.setAttribute("expression", "System.exit(0)");
                break;
                
            case "addStatement":
                processAddStatement(stmtNode, javaStmt);
                break;
                
            default:
                javaStmt.setAttribute("type", "unknown");
                break;
        }
        
        methodBody.appendChild(javaStmt);
    }

    private static void processDisplayStatement(Element displayStmt, Element javaStmt) {
        javaStmt.setAttribute("type", "println");
        
        NodeList displayOperands = displayStmt.getElementsByTagName("displayOperand");
        StringBuilder expression = new StringBuilder("System.out.println(");
        
        for (int i = 0; i < displayOperands.getLength(); i++) {
            if (i > 0) expression.append(" + ");
            
            Element operand = (Element) displayOperands.item(i);
            Node literalNode = findDeepChildByName(operand, "literal");
            Node identifierNode = findDeepChildByName(operand, "identifier");
            
            if (literalNode != null) {
                String literal = extractTextFromNode(literalNode);
                expression.append(literal);
            } else if (identifierNode != null) {
                String identifier = extractIdentifier(identifierNode);
                expression.append(identifier);
            }
        }
        
        expression.append(")");
        javaStmt.setAttribute("expression", expression.toString());
    }

    private static void processMoveStatement(Element moveStmt, Element javaStmt) {
        javaStmt.setAttribute("type", "assignment");
        
        Node moveToNode = findChildNodeByName(moveStmt, "moveToStatement");
        if (moveToNode != null) {
            // Estrai la sorgente
            Node sendingAreaNode = findDeepChildByName(moveToNode, "moveToSendingArea");
            String source = "";
            
            if (sendingAreaNode != null) {
                Node literalNode = findDeepChildByName(sendingAreaNode, "literal");
                Node identifierNode = findDeepChildByName(sendingAreaNode, "identifier");
                
                if (literalNode != null) {
                    source = extractLiteralValue(literalNode);
                } else if (identifierNode != null) {
                    source = extractIdentifier(identifierNode);
                }
            }
            
            // Estrai il target (può essere dopo TO)
            NodeList identifiers = ((Element)moveToNode).getElementsByTagName("identifier");
            String target = "";
            
            // L'ultimo identifier è tipicamente il target
            if (identifiers.getLength() > 0) {
                Node targetIdentifier = identifiers.item(identifiers.getLength() - 1);
                // Verifica che non sia dentro moveToSendingArea
                if (!isDescendantOf(targetIdentifier, "moveToSendingArea")) {
                    target = extractIdentifier(targetIdentifier);
                }
            }
            
            if (!target.isEmpty() && !source.isEmpty()) {
                javaStmt.setAttribute("expression", target + " = " + source);
            } else {
                javaStmt.setAttribute("expression", "// Move statement parsing error");
            }
        }
    }

    private static void processPerformStatement(Element performStmt, Element javaStmt) {
        javaStmt.setAttribute("type", "method-call");
        
        // Cerca procedureName all'interno di performProcedureStatement
        Node performProcNode = findDeepChildByName(performStmt, "performProcedureStatement");
        if (performProcNode != null) {
            Node procedureNameNode = findDeepChildByName(performProcNode, "procedureName");
            if (procedureNameNode != null) {
                String procedureName = extractTextFromNode(procedureNameNode);
                procedureName = sanitizeNameForIdentifier(procedureName);
                javaStmt.setAttribute("name", procedureName);
                javaStmt.setAttribute("expression", procedureName + "()");
            }
        }
    }

    private static void processAddStatement(Element addStmt, Element javaStmt) {
        javaStmt.setAttribute("type", "arithmetic");
        
        Node addToStmtNode = findChildNodeByName(addStmt, "addToStatement");
        if (addToStmtNode != null) {
            // Estrai l'operando da aggiungere
            Node addFromNode = findChildNodeByName(addToStmtNode, "addFrom");
            String fromValue = "";
            
            if (addFromNode != null) {
                Node literalNode = findDeepChildByName(addFromNode, "literal");
                Node identifierNode = findDeepChildByName(addFromNode, "identifier");
                
                if (literalNode != null) {
                    fromValue = extractLiteralValue(literalNode);
                } else if (identifierNode != null) {
                    fromValue = extractIdentifier(identifierNode);
                }
            }
            
            // Estrai il target
            Node addToNode = findChildNodeByName(addToStmtNode, "addTo");
            String toVariable = "";
            
            if (addToNode != null) {
                Node identifierNode = findDeepChildByName(addToNode, "identifier");
                if (identifierNode != null) {
                    toVariable = extractIdentifier(identifierNode);
                }
            }
            
            if (!fromValue.isEmpty() && !toVariable.isEmpty()) {
                javaStmt.setAttribute("expression", toVariable + " = " + toVariable + " + " + fromValue);
            }
        }
    }

    // Metodi di utilità migliorati

    private static String extractTextFromNode(Node node) {
        StringBuilder text = new StringBuilder();
        NodeList children = node.getChildNodes();
        
        for (int i = 0; i < children.getLength(); i++) {
            Node child = children.item(i);
            if (child.getNodeType() == Node.TEXT_NODE) {
                text.append(child.getTextContent().trim());
            } else if (child.getNodeType() == Node.ELEMENT_NODE && child.getNodeName().equals("Token")) {
                text.append(child.getTextContent().trim());
            } else if (child.getNodeType() == Node.ELEMENT_NODE) {
                text.append(extractTextFromNode(child));
            }
        }
        
        return text.toString().trim();
    }

    private static String extractIdentifier(Node identifierNode) {
        Node dataNameNode = findDeepChildByName(identifierNode, "dataName");
        if (dataNameNode != null) {
            String name = extractTextFromNode(dataNameNode);
            return sanitizeNameForIdentifier(name);
        }
        return "";
    }

    private static String extractLiteralValue(Node literalNode) {
        // Controlla se è una costante figurativa
        Node figurativeNode = findDeepChildByName(literalNode, "figurativeConstant");
        if (figurativeNode != null) {
            String constant = extractTextFromNode(figurativeNode);
            if (constant.equalsIgnoreCase("ZERO") || constant.equalsIgnoreCase("ZEROS")) {
                return "0";
            } else if (constant.equalsIgnoreCase("SPACE") || constant.equalsIgnoreCase("SPACES")) {
                return "\" \"";
            }
        }
        
        // Altrimenti estrai il valore letterale
        String literal = extractTextFromNode(literalNode);
        
        // Se è un numero, restituiscilo così com'è
        if (literal.matches("-?\\d+(\\.\\d+)?")) {
            return literal;
        }
        
        // Se è già quotato, restituiscilo
        if ((literal.startsWith("\"") && literal.endsWith("\"")) || 
            (literal.startsWith("'") && literal.endsWith("'"))) {
            return "\"" + literal.substring(1, literal.length() - 1) + "\"";
        }
        
        // Altrimenti aggiungi le virgolette
        return "\"" + literal + "\"";
    }

    private static String extractPictureString(Node picClauseNode) {
        Node pictureStringNode = findChildNodeByName(picClauseNode, "pictureString");
        if (pictureStringNode != null) {
            return extractTextFromNode(pictureStringNode);
        }
        return "";
    }

    private static String extractValueClause(Node valueClauseNode) {
        Node literalNode = findDeepChildByName(valueClauseNode, "literal");
        if (literalNode != null) {
            return extractLiteralValue(literalNode);
        }
        return "null";
    }

    private static Node findDeepChildByName(Node parent, String name) {
        if (parent == null) return null;
        
        NodeList children = parent.getChildNodes();
        for (int i = 0; i < children.getLength(); i++) {
            Node child = children.item(i);
            if (child.getNodeType() == Node.ELEMENT_NODE) {
                if (name.equals(child.getNodeName())) {
                    return child;
                }
                // Ricerca ricorsiva
                Node found = findDeepChildByName(child, name);
                if (found != null) {
                    return found;
                }
            }
        }
        return null;
    }

    private static boolean isDescendantOf(Node node, String ancestorName) {
        Node parent = node.getParentNode();
        while (parent != null) {
            if (parent.getNodeName().equals(ancestorName)) {
                return true;
            }
            parent = parent.getParentNode();
        }
        return false;
    }

    private static Node getFirstElementChild(Node parent) {
        NodeList children = parent.getChildNodes();
        for (int i = 0; i < children.getLength(); i++) {
            if (children.item(i).getNodeType() == Node.ELEMENT_NODE) {
                return children.item(i);
            }
        }
        return null;
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
        
        // Crea istanza della classe
        Element instanceCreation = javaDoc.createElement("statement");
        instanceCreation.setAttribute("type", "instantiation");
        String className = javaClass.getAttribute("name");
        instanceCreation.setAttribute("expression", className + " app = new " + className + "()");
        body.appendChild(instanceCreation);
        
        // Chiama solo il primo metodo (tipicamente MAIN-PARAGRAPH)
        if (!paragraphsToCall.isEmpty()) {
            Element methodCall = javaDoc.createElement("statement");
            methodCall.setAttribute("type", "method-call");
            methodCall.setAttribute("expression", "app." + paragraphsToCall.get(0) + "()");
            body.appendChild(methodCall);
        }
        
        javaClass.appendChild(mainMethod);
    }

    private static void createGetterSetter(String fieldName, String fieldType, Document javaDoc, Element javaClass) {
        String capitalizedName = fieldName.substring(0, 1).toUpperCase() + fieldName.substring(1);
        
        // Getter
        Element getter = javaDoc.createElement("method");
        getter.setAttribute("name", "get" + capitalizedName);
        getter.setAttribute("visibility", "public");
        getter.setAttribute("returnType", fieldType);
        Element getterBody = javaDoc.createElement("body");
        Element returnStmt = javaDoc.createElement("statement");
        returnStmt.setAttribute("type", "return");
        returnStmt.setAttribute("expression", "return " + fieldName);
        getterBody.appendChild(returnStmt);
        getter.appendChild(getterBody);
        javaClass.appendChild(getter);
        
        // Setter
        Element setter = javaDoc.createElement("method");
        setter.setAttribute("name", "set" + capitalizedName);
        setter.setAttribute("visibility", "public");
        setter.setAttribute("returnType", "void");
        Element param = javaDoc.createElement("parameter");
        param.setAttribute("name", "value");
        param.setAttribute("type", fieldType);
        setter.appendChild(param);
        Element setterBody = javaDoc.createElement("body");
        Element assignment = javaDoc.createElement("statement");
        assignment.setAttribute("type", "assignment");
        assignment.setAttribute("expression", "this." + fieldName + " = value");
        setterBody.appendChild(assignment);
        setter.appendChild(setterBody);
        javaClass.appendChild(setter);
    }

    private static String mapCobolTypeToJava(String picClause) {
        picClause = picClause.toUpperCase().replaceAll("\\s+", "");
        
        if (picClause.contains("X")) {
            return "String";
        } else if (picClause.contains("9")) {
            if (picClause.contains("V") || picClause.contains(".")) {
                return "BigDecimal";
            } else {
                // Conta il numero di cifre
                int digits = countDigits(picClause);
                if (digits > 9) {
                    return "long";
                } else {
                    return "int";
                }
            }
        }
        
        return "String"; // Default
    }

    private static int countDigits(String picClause) {
        int count = 0;
        for (int i = 0; i < picClause.length(); i++) {
            char c = picClause.charAt(i);
            if (c == '9') {
                // Controlla se c'è un moltiplicatore
                if (i + 1 < picClause.length() && picClause.charAt(i + 1) == '(') {
                    int closeIndex = picClause.indexOf(')', i);
                    if (closeIndex > i + 1) {
                        String multiplier = picClause.substring(i + 2, closeIndex);
                        try {
                            count += Integer.parseInt(multiplier);
                            i = closeIndex;
                        } catch (NumberFormatException e) {
                            count++;
                        }
                    }
                } else {
                    count++;
                }
            }
        }
        return count;
    }

    private static String sanitizeNameForClass(String name) {
        String[] parts = name.trim().split("[-_]");
        StringBuilder sb = new StringBuilder();
        for (String part : parts) {
            if (!part.isEmpty()) {
                sb.append(part.substring(0, 1).toUpperCase());
                if (part.length() > 1) {
                    sb.append(part.substring(1).toLowerCase());
                }
            }
        }
        return sb.toString();
    }

    private static String sanitizeNameForIdentifier(String name) {
        String sanitized = sanitizeNameForClass(name);
        if (!sanitized.isEmpty()) {
            return sanitized.substring(0, 1).toLowerCase() + sanitized.substring(1);
        }
        return "undefined";
    }

    private static Node findChildNodeByName(Node parent, String name) {
        NodeList children = parent.getChildNodes();
        for (int i = 0; i < children.getLength(); i++) {
            Node child = children.item(i);
            if (child.getNodeType() == Node.ELEMENT_NODE && name.equals(child.getNodeName())) {
                return child;
            }
        }
        return null;
    }
}