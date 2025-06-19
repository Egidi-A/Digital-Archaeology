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
import java.util.regex.Matcher;
import java.util.regex.Pattern;

public class CobolToJavaXMLTransformer {

    // Map per tenere traccia dei campi dichiarati
    private static Map<String, String> declaredFields = new HashMap<>();
    // Map per tenere traccia dei cursori SQL dichiarati
    private static Map<String, String> declaredCursors = new HashMap<>();
    // Set per tenere traccia delle variabili SQL usate
    private static Set<String> sqlVariables = new HashSet<>();
    
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

        // Aggiungi campo per la connessione database
        addDatabaseConnectionField(javaDoc, javaClass);

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

    private static void addDatabaseConnectionField(Document javaDoc, Element javaClass) {
        Element connField = javaDoc.createElement("field");
        connField.setAttribute("name", "connection");
        connField.setAttribute("type", "java.sql.Connection");
        connField.setAttribute("visibility", "private");
        javaClass.appendChild(connField);
        
        Element sqlCodeField = javaDoc.createElement("field");
        sqlCodeField.setAttribute("name", "sqlcode");
        sqlCodeField.setAttribute("type", "int");
        sqlCodeField.setAttribute("visibility", "private");
        javaClass.appendChild(sqlCodeField);
    }

    private static void processDataDivision(Element dataDiv, Document javaDoc, Element javaClass) {
        // Processa le SQL DECLARE prima
        NodeList sqlDeclares = dataDiv.getElementsByTagName("dataDescriptionEntryExecSql");
        for (int i = 0; i < sqlDeclares.getLength(); i++) {
            Element sqlDeclare = (Element) sqlDeclares.item(i);
            String sqlContent = extractSqlContent(sqlDeclare);
            if (sqlContent.contains("DECLARE") && sqlContent.contains("CURSOR")) {
                processSqlCursorDeclaration(sqlContent, javaDoc, javaClass);
            }
        }
        
        // Processa i campi normali
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

    private static void processSqlCursorDeclaration(String sqlContent, Document javaDoc, Element javaClass) {
        // Estrai il nome del cursore
        Pattern cursorPattern = Pattern.compile("DECLARE\\s+(\\w+)\\s+CURSOR", Pattern.CASE_INSENSITIVE);
        Matcher matcher = cursorPattern.matcher(sqlContent);
        
        if (matcher.find()) {
            String cursorName = matcher.group(1).toLowerCase();
            String cursorFieldName = "cursor" + sanitizeNameForClass(cursorName);
            
            // Aggiungi campo per il PreparedStatement del cursore
            Element cursorField = javaDoc.createElement("field");
            cursorField.setAttribute("name", cursorFieldName);
            cursorField.setAttribute("type", "java.sql.PreparedStatement");
            cursorField.setAttribute("visibility", "private");
            javaClass.appendChild(cursorField);
            
            // Aggiungi campo per il ResultSet
            Element rsField = javaDoc.createElement("field");
            rsField.setAttribute("name", "rs" + sanitizeNameForClass(cursorName));
            rsField.setAttribute("type", "java.sql.ResultSet");
            rsField.setAttribute("visibility", "private");
            javaClass.appendChild(rsField);
            
            // Salva la query SQL per uso successivo
            String sqlQuery = extractSqlQuery(sqlContent);
            declaredCursors.put(cursorName.toUpperCase(), sqlQuery);
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
            method.setAttribute("throws", "SQLException");
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
                
            case "execSqlStatement":
                processExecSqlStatement(stmtNode, javaStmt, javaDoc, methodBody);
                return; // Return early as SQL might generate multiple statements
                
            case "acceptStatement":
                processAcceptStatement(stmtNode, javaStmt);
                break;
                
            case "computeStatement":
                processComputeStatement(stmtNode, javaStmt);
                break;
                
            case "ifStatement":
                processIfStatement(stmtNode, javaStmt, javaDoc, methodBody);
                return; // Return early as IF generates complex structure
                
            case "evaluateStatement":
                processEvaluateStatement(stmtNode, javaStmt, javaDoc, methodBody);
                return; // Return early as EVALUATE generates complex structure
                
            case "openStatement":
                processOpenStatement(stmtNode, javaStmt);
                break;
                
            case "closeStatement":
                processCloseStatement(stmtNode, javaStmt);
                break;
                
            case "writeStatement":
                processWriteStatement(stmtNode, javaStmt);
                break;
                
            case "stringStatement":
                processStringStatement(stmtNode, javaStmt);
                break;
                
            default:
                javaStmt.setAttribute("type", "unknown");
                break;
        }
        
        methodBody.appendChild(javaStmt);
    }

    private static void processExecSqlStatement(Element sqlStmt, Element javaStmt, Document javaDoc, Element methodBody) {
        String sqlContent = extractSqlContent(sqlStmt);
        
        if (sqlContent.contains("CONNECT TO")) {
            processConnectStatement(sqlContent, javaDoc, methodBody);
        } else if (sqlContent.contains("DISCONNECT")) {
            processDisconnectStatement(javaDoc, methodBody);
        } else if (sqlContent.contains("SELECT") && !sqlContent.contains("INSERT")) {
            processSelectStatement(sqlContent, javaDoc, methodBody);
        } else if (sqlContent.contains("INSERT")) {
            processInsertStatement(sqlContent, javaDoc, methodBody);
        } else if (sqlContent.contains("UPDATE")) {
            processUpdateStatement(sqlContent, javaDoc, methodBody);
        } else if (sqlContent.contains("DELETE")) {
            processDeleteStatement(sqlContent, javaDoc, methodBody);
        } else if (sqlContent.contains("OPEN") && sqlContent.contains("CUR")) {
            processOpenCursorStatement(sqlContent, javaDoc, methodBody);
        } else if (sqlContent.contains("FETCH")) {
            processFetchStatement(sqlContent, javaDoc, methodBody);
        } else if (sqlContent.contains("CLOSE") && sqlContent.contains("CUR")) {
            processCloseCursorStatement(sqlContent, javaDoc, methodBody);
        }
    }

    private static void processConnectStatement(String sqlContent, Document javaDoc, Element methodBody) {
        // Try block
        Element tryBlock = javaDoc.createElement("try-block");
        methodBody.appendChild(tryBlock);
        
        // Extract connection parameters
        Pattern pattern = Pattern.compile("'([^']*)'");
        Matcher matcher = pattern.matcher(sqlContent);
        List<String> params = new ArrayList<>();
        while (matcher.find()) {
            params.add(matcher.group(1));
        }
        
        String url = params.size() > 0 ? params.get(0) : "jdbc:postgresql://localhost/database";
        String user = params.size() > 1 ? params.get(1) : "user";
        String password = params.size() > 2 ? params.get(2) : "password";
        
        // Connection statement
        Element connStmt = javaDoc.createElement("statement");
        connStmt.setAttribute("type", "assignment");
        connStmt.setAttribute("expression", 
            String.format("connection = DriverManager.getConnection(\"%s\", \"%s\", \"%s\")", 
                         url, user, password));
        tryBlock.appendChild(connStmt);
        
        // Set SQLCODE to 0
        Element sqlCodeStmt = javaDoc.createElement("statement");
        sqlCodeStmt.setAttribute("type", "assignment");
        sqlCodeStmt.setAttribute("expression", "sqlcode = 0");
        tryBlock.appendChild(sqlCodeStmt);
        
        // Catch block
        Element catchBlock = javaDoc.createElement("catch-block");
        catchBlock.setAttribute("exception", "SQLException");
        catchBlock.setAttribute("variable", "e");
        methodBody.appendChild(catchBlock);
        
        Element catchStmt = javaDoc.createElement("statement");
        catchStmt.setAttribute("type", "assignment");
        catchStmt.setAttribute("expression", "sqlcode = e.getErrorCode()");
        catchBlock.appendChild(catchStmt);
    }

    private static void processDisconnectStatement(Document javaDoc, Element methodBody) {
        Element tryBlock = javaDoc.createElement("try-block");
        methodBody.appendChild(tryBlock);
        
        Element ifStmt = javaDoc.createElement("if");
        ifStmt.setAttribute("condition", "connection != null && !connection.isClosed()");
        tryBlock.appendChild(ifStmt);
        
        Element closeStmt = javaDoc.createElement("statement");
        closeStmt.setAttribute("type", "method-call");
        closeStmt.setAttribute("expression", "connection.close()");
        ifStmt.appendChild(closeStmt);
        
        Element catchBlock = javaDoc.createElement("catch-block");
        catchBlock.setAttribute("exception", "SQLException");
        catchBlock.setAttribute("variable", "e");
        methodBody.appendChild(catchBlock);
        
        Element printStmt = javaDoc.createElement("statement");
        printStmt.setAttribute("type", "method-call");
        printStmt.setAttribute("expression", "e.printStackTrace()");
        catchBlock.appendChild(printStmt);
    }

    private static void processSelectStatement(String sqlContent, Document javaDoc, Element methodBody) {
        // Extract SQL query and variables
        String cleanSql = cleanSqlQuery(sqlContent);
        List<String> intoVars = extractIntoVariables(sqlContent);
        List<String> whereVars = extractWhereVariables(sqlContent);
        
        Element tryBlock = javaDoc.createElement("try-block");
        methodBody.appendChild(tryBlock);
        
        // Create PreparedStatement
        Element pstmtDecl = javaDoc.createElement("statement");
        pstmtDecl.setAttribute("type", "declaration");
        pstmtDecl.setAttribute("varType", "PreparedStatement");
        pstmtDecl.setAttribute("varName", "pstmt");
        pstmtDecl.setAttribute("expression", "connection.prepareStatement(\"" + cleanSql + "\")");
        tryBlock.appendChild(pstmtDecl);
        
        // Set parameters
        int paramIndex = 1;
        for (String var : whereVars) {
            Element setParam = javaDoc.createElement("statement");
            setParam.setAttribute("type", "method-call");
            String varName = sanitizeNameForIdentifier(var);
            String varType = declaredFields.getOrDefault(var.toUpperCase(), "String");
            
            if (varType.equals("String")) {
                setParam.setAttribute("expression", 
                    String.format("pstmt.setString(%d, %s)", paramIndex++, varName));
            } else if (varType.equals("int")) {
                setParam.setAttribute("expression", 
                    String.format("pstmt.setInt(%d, %s)", paramIndex++, varName));
            } else if (varType.equals("BigDecimal")) {
                setParam.setAttribute("expression", 
                    String.format("pstmt.setBigDecimal(%d, %s)", paramIndex++, varName));
            }
            tryBlock.appendChild(setParam);
        }
        
        // Execute query
        Element rsDecl = javaDoc.createElement("statement");
        rsDecl.setAttribute("type", "declaration");
        rsDecl.setAttribute("varType", "ResultSet");
        rsDecl.setAttribute("varName", "rs");
        rsDecl.setAttribute("expression", "pstmt.executeQuery()");
        tryBlock.appendChild(rsDecl);
        
        // Process results
        Element ifNext = javaDoc.createElement("if");
        ifNext.setAttribute("condition", "rs.next()");
        tryBlock.appendChild(ifNext);
        
        // Get values from ResultSet
        int colIndex = 1;
        for (String var : intoVars) {
            Element getVal = javaDoc.createElement("statement");
            getVal.setAttribute("type", "assignment");
            String varName = sanitizeNameForIdentifier(var);
            String varType = declaredFields.getOrDefault(var.toUpperCase(), "String");
            
            if (varType.equals("String")) {
                getVal.setAttribute("expression", 
                    String.format("%s = rs.getString(%d)", varName, colIndex++));
            } else if (varType.equals("int")) {
                getVal.setAttribute("expression", 
                    String.format("%s = rs.getInt(%d)", varName, colIndex++));
            } else if (varType.equals("BigDecimal")) {
                getVal.setAttribute("expression", 
                    String.format("%s = rs.getBigDecimal(%d)", varName, colIndex++));
            }
            ifNext.appendChild(getVal);
        }
        
        // Set SQLCODE = 0 for success
        Element successCode = javaDoc.createElement("statement");
        successCode.setAttribute("type", "assignment");
        successCode.setAttribute("expression", "sqlcode = 0");
        ifNext.appendChild(successCode);
        
        // Else block for no results
        Element elseBlock = javaDoc.createElement("else");
        ifNext.appendChild(elseBlock);
        
        Element notFoundCode = javaDoc.createElement("statement");
        notFoundCode.setAttribute("type", "assignment");
        notFoundCode.setAttribute("expression", "sqlcode = 100");
        elseBlock.appendChild(notFoundCode);
        
        // Close resources
        Element closeRs = javaDoc.createElement("statement");
        closeRs.setAttribute("type", "method-call");
        closeRs.setAttribute("expression", "rs.close()");
        tryBlock.appendChild(closeRs);
        
        Element closePstmt = javaDoc.createElement("statement");
        closePstmt.setAttribute("type", "method-call");
        closePstmt.setAttribute("expression", "pstmt.close()");
        tryBlock.appendChild(closePstmt);
        
        // Catch block
        Element catchBlock = javaDoc.createElement("catch-block");
        catchBlock.setAttribute("exception", "SQLException");
        catchBlock.setAttribute("variable", "e");
        methodBody.appendChild(catchBlock);
        
        Element catchStmt = javaDoc.createElement("statement");
        catchStmt.setAttribute("type", "assignment");
        catchStmt.setAttribute("expression", "sqlcode = e.getErrorCode()");
        catchBlock.appendChild(catchStmt);
    }

    private static void processInsertStatement(String sqlContent, Document javaDoc, Element methodBody) {
        String cleanSql = cleanSqlQuery(sqlContent);
        List<String> valueVars = extractValueVariables(sqlContent);
        
        Element tryBlock = javaDoc.createElement("try-block");
        methodBody.appendChild(tryBlock);
        
        // Create PreparedStatement
        Element pstmtDecl = javaDoc.createElement("statement");
        pstmtDecl.setAttribute("type", "declaration");
        pstmtDecl.setAttribute("varType", "PreparedStatement");
        pstmtDecl.setAttribute("varName", "pstmt");
        pstmtDecl.setAttribute("expression", "connection.prepareStatement(\"" + cleanSql + "\")");
        tryBlock.appendChild(pstmtDecl);
        
        // Set parameters
        int paramIndex = 1;
        for (String var : valueVars) {
            Element setParam = javaDoc.createElement("statement");
            setParam.setAttribute("type", "method-call");
            String varName = sanitizeNameForIdentifier(var);
            String varType = declaredFields.getOrDefault(var.toUpperCase(), "String");
            
            if (varType.equals("String")) {
                setParam.setAttribute("expression", 
                    String.format("pstmt.setString(%d, %s)", paramIndex++, varName));
            } else if (varType.equals("int")) {
                setParam.setAttribute("expression", 
                    String.format("pstmt.setInt(%d, %s)", paramIndex++, varName));
            } else if (varType.equals("BigDecimal")) {
                setParam.setAttribute("expression", 
                    String.format("pstmt.setBigDecimal(%d, %s)", paramIndex++, varName));
            }
            tryBlock.appendChild(setParam);
        }
        
        // Execute update
        Element execUpdate = javaDoc.createElement("statement");
        execUpdate.setAttribute("type", "declaration");
        execUpdate.setAttribute("varType", "int");
        execUpdate.setAttribute("varName", "rowsAffected");
        execUpdate.setAttribute("expression", "pstmt.executeUpdate()");
        tryBlock.appendChild(execUpdate);
        
        // Set SQLCODE
        Element sqlCodeStmt = javaDoc.createElement("statement");
        sqlCodeStmt.setAttribute("type", "assignment");
        sqlCodeStmt.setAttribute("expression", "sqlcode = (rowsAffected > 0) ? 0 : 100");
        tryBlock.appendChild(sqlCodeStmt);
        
        // Close statement
        Element closePstmt = javaDoc.createElement("statement");
        closePstmt.setAttribute("type", "method-call");
        closePstmt.setAttribute("expression", "pstmt.close()");
        tryBlock.appendChild(closePstmt);
        
        // Catch block
        Element catchBlock = javaDoc.createElement("catch-block");
        catchBlock.setAttribute("exception", "SQLException");
        catchBlock.setAttribute("variable", "e");
        methodBody.appendChild(catchBlock);
        
        Element catchStmt = javaDoc.createElement("statement");
        catchStmt.setAttribute("type", "assignment");
        catchStmt.setAttribute("expression", "sqlcode = e.getErrorCode()");
        catchBlock.appendChild(catchStmt);
    }

    private static void processUpdateStatement(String sqlContent, Document javaDoc, Element methodBody) {
        // Similar to INSERT but with UPDATE syntax
        String cleanSql = cleanSqlQuery(sqlContent);
        List<String> setVars = extractSetVariables(sqlContent);
        List<String> whereVars = extractWhereVariables(sqlContent);
        
        Element tryBlock = javaDoc.createElement("try-block");
        methodBody.appendChild(tryBlock);
        
        // Create PreparedStatement
        Element pstmtDecl = javaDoc.createElement("statement");
        pstmtDecl.setAttribute("type", "declaration");
        pstmtDecl.setAttribute("varType", "PreparedStatement");
        pstmtDecl.setAttribute("varName", "pstmt");
        pstmtDecl.setAttribute("expression", "connection.prepareStatement(\"" + cleanSql + "\")");
        tryBlock.appendChild(pstmtDecl);
        
        // Set parameters (SET clause parameters first, then WHERE clause)
        int paramIndex = 1;
        
        // Set SET clause parameters
        for (String var : setVars) {
            Element setParam = javaDoc.createElement("statement");
            setParam.setAttribute("type", "method-call");
            String varName = sanitizeNameForIdentifier(var);
            String varType = declaredFields.getOrDefault(var.toUpperCase(), "String");
            
            if (varType.equals("String")) {
                setParam.setAttribute("expression", 
                    String.format("pstmt.setString(%d, %s)", paramIndex++, varName));
            } else if (varType.equals("int")) {
                setParam.setAttribute("expression", 
                    String.format("pstmt.setInt(%d, %s)", paramIndex++, varName));
            } else if (varType.equals("BigDecimal")) {
                setParam.setAttribute("expression", 
                    String.format("pstmt.setBigDecimal(%d, %s)", paramIndex++, varName));
            }
            tryBlock.appendChild(setParam);
        }
        
        // Set WHERE clause parameters
        for (String var : whereVars) {
            Element setParam = javaDoc.createElement("statement");
            setParam.setAttribute("type", "method-call");
            String varName = sanitizeNameForIdentifier(var);
            String varType = declaredFields.getOrDefault(var.toUpperCase(), "String");
            
            if (varType.equals("String")) {
                setParam.setAttribute("expression", 
                    String.format("pstmt.setString(%d, %s)", paramIndex++, varName));
            } else if (varType.equals("int")) {
                setParam.setAttribute("expression", 
                    String.format("pstmt.setInt(%d, %s)", paramIndex++, varName));
            } else if (varType.equals("BigDecimal")) {
                setParam.setAttribute("expression", 
                    String.format("pstmt.setBigDecimal(%d, %s)", paramIndex++, varName));
            }
            tryBlock.appendChild(setParam);
        }
        
        // Execute update
        Element execUpdate = javaDoc.createElement("statement");
        execUpdate.setAttribute("type", "declaration");
        execUpdate.setAttribute("varType", "int");
        execUpdate.setAttribute("varName", "rowsAffected");
        execUpdate.setAttribute("expression", "pstmt.executeUpdate()");
        tryBlock.appendChild(execUpdate);
        
        // Set SQLCODE
        Element sqlCodeStmt = javaDoc.createElement("statement");
        sqlCodeStmt.setAttribute("type", "assignment");
        sqlCodeStmt.setAttribute("expression", "sqlcode = (rowsAffected > 0) ? 0 : 100");
        tryBlock.appendChild(sqlCodeStmt);
        
        // Close statement
        Element closePstmt = javaDoc.createElement("statement");
        closePstmt.setAttribute("type", "method-call");
        closePstmt.setAttribute("expression", "pstmt.close()");
        tryBlock.appendChild(closePstmt);
        
        // Catch block
        Element catchBlock = javaDoc.createElement("catch-block");
        catchBlock.setAttribute("exception", "SQLException");
        catchBlock.setAttribute("variable", "e");
        methodBody.appendChild(catchBlock);
        
        Element catchStmt = javaDoc.createElement("statement");
        catchStmt.setAttribute("type", "assignment");
        catchStmt.setAttribute("expression", "sqlcode = e.getErrorCode()");
        catchBlock.appendChild(catchStmt);
    }

    private static void processOpenCursorStatement(String sqlContent, Document javaDoc, Element methodBody) {
        Pattern pattern = Pattern.compile("OPEN\\s+(\\w+)", Pattern.CASE_INSENSITIVE);
        Matcher matcher = pattern.matcher(sqlContent);
        
        if (matcher.find()) {
            String cursorName = matcher.group(1).toLowerCase();
            String cursorFieldName = "cursor" + sanitizeNameForClass(cursorName);
            String rsFieldName = "rs" + sanitizeNameForClass(cursorName);
            
            Element tryBlock = javaDoc.createElement("try-block");
            methodBody.appendChild(tryBlock);
            
            // Get the SQL query from declared cursors
            String sqlQuery = declaredCursors.get(cursorName.toUpperCase());
            if (sqlQuery != null) {
                // Create PreparedStatement for cursor
                Element pstmtStmt = javaDoc.createElement("statement");
                pstmtStmt.setAttribute("type", "assignment");
                pstmtStmt.setAttribute("expression", 
                    cursorFieldName + " = connection.prepareStatement(\"" + sqlQuery + "\")");
                tryBlock.appendChild(pstmtStmt);
                
                // Set parameters if any
                List<String> cursorVars = extractWhereVariables(sqlQuery);
                int paramIndex = 1;
                for (String var : cursorVars) {
                    Element setParam = javaDoc.createElement("statement");
                    setParam.setAttribute("type", "method-call");
                    String varName = sanitizeNameForIdentifier(var);
                    String varType = declaredFields.getOrDefault(var.toUpperCase(), "String");
                    
                    if (varType.equals("String")) {
                        setParam.setAttribute("expression", 
                            String.format("%s.setString(%d, %s)", cursorFieldName, paramIndex++, varName));
                    } else if (varType.equals("int")) {
                        setParam.setAttribute("expression", 
                            String.format("%s.setInt(%d, %s)", cursorFieldName, paramIndex++, varName));
                    } else if (varType.equals("BigDecimal")) {
                        setParam.setAttribute("expression", 
                            String.format("%s.setBigDecimal(%d, %s)", cursorFieldName, paramIndex++, varName));
                    }
                    tryBlock.appendChild(setParam);
                }
                
                // Execute query
                Element execStmt = javaDoc.createElement("statement");
                execStmt.setAttribute("type", "assignment");
                execStmt.setAttribute("expression", rsFieldName + " = " + cursorFieldName + ".executeQuery()");
                tryBlock.appendChild(execStmt);
                
                Element sqlCodeStmt = javaDoc.createElement("statement");
                sqlCodeStmt.setAttribute("type", "assignment");
                sqlCodeStmt.setAttribute("expression", "sqlcode = 0");
                tryBlock.appendChild(sqlCodeStmt);
            }
            
            // Catch block
            Element catchBlock = javaDoc.createElement("catch-block");
            catchBlock.setAttribute("exception", "SQLException");
            catchBlock.setAttribute("variable", "e");
            methodBody.appendChild(catchBlock);
            
            Element catchStmt = javaDoc.createElement("statement");
            catchStmt.setAttribute("type", "assignment");
            catchStmt.setAttribute("expression", "sqlcode = e.getErrorCode()");
            catchBlock.appendChild(catchStmt);
        }
    }

    private static void processFetchStatement(String sqlContent, Document javaDoc, Element methodBody) {
        Pattern pattern = Pattern.compile("FETCH\\s+(\\w+)", Pattern.CASE_INSENSITIVE);
        Matcher matcher = pattern.matcher(sqlContent);
        
        if (matcher.find()) {
            String cursorName = matcher.group(1).toLowerCase();
            String rsFieldName = "rs" + sanitizeNameForClass(cursorName);
            
            List<String> intoVars = extractIntoVariables(sqlContent);
            
            Element tryBlock = javaDoc.createElement("try-block");
            methodBody.appendChild(tryBlock);
            
            // Check if ResultSet has next
            Element ifNext = javaDoc.createElement("if");
            ifNext.setAttribute("condition", rsFieldName + ".next()");
            tryBlock.appendChild(ifNext);
            
            // Get values from ResultSet
            int colIndex = 1;
            for (String var : intoVars) {
                Element getVal = javaDoc.createElement("statement");
                getVal.setAttribute("type", "assignment");
                String varName = sanitizeNameForIdentifier(var);
                String varType = declaredFields.getOrDefault(var.toUpperCase(), "String");
                
                if (varType.equals("String")) {
                    getVal.setAttribute("expression", 
                        String.format("%s = %s.getString(%d)", varName, rsFieldName, colIndex++));
                } else if (varType.equals("int")) {
                    getVal.setAttribute("expression", 
                        String.format("%s = %s.getInt(%d)", varName, rsFieldName, colIndex++));
                } else if (varType.equals("BigDecimal")) {
                    getVal.setAttribute("expression", 
                        String.format("%s = %s.getBigDecimal(%d)", varName, rsFieldName, colIndex++));
                }
                ifNext.appendChild(getVal);
            }
            
            // Set SQLCODE = 0 for success
            Element successCode = javaDoc.createElement("statement");
            successCode.setAttribute("type", "assignment");
            successCode.setAttribute("expression", "sqlcode = 0");
            ifNext.appendChild(successCode);
            
            // Else block for end of cursor
            Element elseBlock = javaDoc.createElement("else");
            ifNext.appendChild(elseBlock);
            
            Element endCode = javaDoc.createElement("statement");
            endCode.setAttribute("type", "assignment");
            endCode.setAttribute("expression", "sqlcode = 100");
            elseBlock.appendChild(endCode);
            
            // Catch block
            Element catchBlock = javaDoc.createElement("catch-block");
            catchBlock.setAttribute("exception", "SQLException");
            catchBlock.setAttribute("variable", "e");
            methodBody.appendChild(catchBlock);
            
            Element catchStmt = javaDoc.createElement("statement");
            catchStmt.setAttribute("type", "assignment");
            catchStmt.setAttribute("expression", "sqlcode = e.getErrorCode()");
            catchBlock.appendChild(catchStmt);
        }
    }

    private static void processCloseCursorStatement(String sqlContent, Document javaDoc, Element methodBody) {
        Pattern pattern = Pattern.compile("CLOSE\\s+(\\w+)", Pattern.CASE_INSENSITIVE);
        Matcher matcher = pattern.matcher(sqlContent);
        
        if (matcher.find()) {
            String cursorName = matcher.group(1).toLowerCase();
            String cursorFieldName = "cursor" + sanitizeNameForClass(cursorName);
            String rsFieldName = "rs" + sanitizeNameForClass(cursorName);
            
            Element tryBlock = javaDoc.createElement("try-block");
            methodBody.appendChild(tryBlock);
            
            // Close ResultSet
            Element ifRsNotNull = javaDoc.createElement("if");
            ifRsNotNull.setAttribute("condition", rsFieldName + " != null");
            tryBlock.appendChild(ifRsNotNull);
            
            Element closeRs = javaDoc.createElement("statement");
            closeRs.setAttribute("type", "method-call");
            closeRs.setAttribute("expression", rsFieldName + ".close()");
            ifRsNotNull.appendChild(closeRs);
            
            // Close PreparedStatement
            Element ifPstmtNotNull = javaDoc.createElement("if");
            ifPstmtNotNull.setAttribute("condition", cursorFieldName + " != null");
            tryBlock.appendChild(ifPstmtNotNull);
            
            Element closePstmt = javaDoc.createElement("statement");
            closePstmt.setAttribute("type", "method-call");
            closePstmt.setAttribute("expression", cursorFieldName + ".close()");
            ifPstmtNotNull.appendChild(closePstmt);
            
            Element sqlCodeStmt = javaDoc.createElement("statement");
            sqlCodeStmt.setAttribute("type", "assignment");
            sqlCodeStmt.setAttribute("expression", "sqlcode = 0");
            tryBlock.appendChild(sqlCodeStmt);
            
            // Catch block
            Element catchBlock = javaDoc.createElement("catch-block");
            catchBlock.setAttribute("exception", "SQLException");
            catchBlock.setAttribute("variable", "e");
            methodBody.appendChild(catchBlock);
            
            Element catchStmt = javaDoc.createElement("statement");
            catchStmt.setAttribute("type", "assignment");
            catchStmt.setAttribute("expression", "sqlcode = e.getErrorCode()");
            catchBlock.appendChild(catchStmt);
        }
    }

    private static void processDeleteStatement(String sqlContent, Document javaDoc, Element methodBody) {
        String cleanSql = cleanSqlQuery(sqlContent);
        List<String> whereVars = extractWhereVariables(sqlContent);
        
        Element tryBlock = javaDoc.createElement("try-block");
        methodBody.appendChild(tryBlock);
        
        // Create PreparedStatement
        Element pstmtDecl = javaDoc.createElement("statement");
        pstmtDecl.setAttribute("type", "declaration");
        pstmtDecl.setAttribute("varType", "PreparedStatement");
        pstmtDecl.setAttribute("varName", "pstmt");
        pstmtDecl.setAttribute("expression", "connection.prepareStatement(\"" + cleanSql + "\")");
        tryBlock.appendChild(pstmtDecl);
        
        // Set parameters
        int paramIndex = 1;
        for (String var : whereVars) {
            Element setParam = javaDoc.createElement("statement");
            setParam.setAttribute("type", "method-call");
            String varName = sanitizeNameForIdentifier(var);
            String varType = declaredFields.getOrDefault(var.toUpperCase(), "String");
            
            if (varType.equals("String")) {
                setParam.setAttribute("expression", 
                    String.format("pstmt.setString(%d, %s)", paramIndex++, varName));
            } else if (varType.equals("int")) {
                setParam.setAttribute("expression", 
                    String.format("pstmt.setInt(%d, %s)", paramIndex++, varName));
            } else if (varType.equals("BigDecimal")) {
                setParam.setAttribute("expression", 
                    String.format("pstmt.setBigDecimal(%d, %s)", paramIndex++, varName));
            }
            tryBlock.appendChild(setParam);
        }
        
        // Execute update
        Element execUpdate = javaDoc.createElement("statement");
        execUpdate.setAttribute("type", "declaration");
        execUpdate.setAttribute("varType", "int");
        execUpdate.setAttribute("varName", "rowsAffected");
        execUpdate.setAttribute("expression", "pstmt.executeUpdate()");
        tryBlock.appendChild(execUpdate);
        
        // Set SQLCODE
        Element sqlCodeStmt = javaDoc.createElement("statement");
        sqlCodeStmt.setAttribute("type", "assignment");
        sqlCodeStmt.setAttribute("expression", "sqlcode = (rowsAffected > 0) ? 0 : 100");
        tryBlock.appendChild(sqlCodeStmt);
        
        // Close statement
        Element closePstmt = javaDoc.createElement("statement");
        closePstmt.setAttribute("type", "method-call");
        closePstmt.setAttribute("expression", "pstmt.close()");
        tryBlock.appendChild(closePstmt);
        
        // Catch block
        Element catchBlock = javaDoc.createElement("catch-block");
        catchBlock.setAttribute("exception", "SQLException");
        catchBlock.setAttribute("variable", "e");
        methodBody.appendChild(catchBlock);
        
        Element catchStmt = javaDoc.createElement("statement");
        catchStmt.setAttribute("type", "assignment");
        catchStmt.setAttribute("expression", "sqlcode = e.getErrorCode()");
        catchBlock.appendChild(catchStmt);
    }

    // Metodi helper per l'estrazione di SQL
    
    private static String extractSqlContent(Element sqlNode) {
        StringBuilder sql = new StringBuilder();
        NodeList children = sqlNode.getChildNodes();
        
        for (int i = 0; i < children.getLength(); i++) {
            Node child = children.item(i);
            if (child.getNodeType() == Node.TEXT_NODE) {
                String text = child.getTextContent();
                if (text.contains("EXECSQL")) {
                    text = text.replaceAll("\\*>EXECSQL\\s*", "");
                    text = text.replaceAll("EXEC SQL\\s*", "");
                    text = text.replaceAll("END-EXEC.*", "");
                    sql.append(text).append(" ");
                }
            }
        }
        
        return sql.toString().trim();
    }

    private static String extractSqlQuery(String sqlContent) {
        // Remove DECLARE CURSOR FOR
        String query = sqlContent.replaceAll("(?i)DECLARE\\s+\\w+\\s+CURSOR\\s+FOR\\s+", "");
        return cleanSqlQuery(query);
    }

    private static String cleanSqlQuery(String sql) {
        // Replace COBOL variables with ?
        String cleaned = sql.replaceAll(":\\w+", "?");
        // Remove multiple spaces
        cleaned = cleaned.replaceAll("\\s+", " ");
        // Handle CURRENT_DATE
        cleaned = cleaned.replaceAll("\\?", "?");
        cleaned = cleaned.replaceAll("CURRENT_DATE", "CURRENT_DATE");
        return cleaned.trim();
    }

    private static List<String> extractIntoVariables(String sql) {
        List<String> vars = new ArrayList<>();
        Pattern pattern = Pattern.compile("INTO\\s+:([\\w-]+)(?:\\s*,\\s*:([\\w-]+))*", Pattern.CASE_INSENSITIVE);
        Matcher matcher = pattern.matcher(sql);
        
        if (matcher.find()) {
            // Extract all groups
            String fullMatch = matcher.group(0);
            Pattern varPattern = Pattern.compile(":([\\w-]+)");
            Matcher varMatcher = varPattern.matcher(fullMatch);
            
            while (varMatcher.find()) {
                vars.add(varMatcher.group(1));
            }
        }
        
        return vars;
    }

    private static List<String> extractWhereVariables(String sql) {
        List<String> vars = new ArrayList<>();
        Pattern pattern = Pattern.compile("WHERE.*", Pattern.CASE_INSENSITIVE);
        Matcher matcher = pattern.matcher(sql);
        
        if (matcher.find()) {
            String whereClause = matcher.group(0);
            Pattern varPattern = Pattern.compile(":([\\w-]+)");
            Matcher varMatcher = varPattern.matcher(whereClause);
            
            while (varMatcher.find()) {
                vars.add(varMatcher.group(1));
            }
        }
        
        return vars;
    }

    private static List<String> extractValueVariables(String sql) {
        List<String> vars = new ArrayList<>();
        Pattern pattern = Pattern.compile("VALUES\\s*\\(([^)]+)\\)", Pattern.CASE_INSENSITIVE);
        Matcher matcher = pattern.matcher(sql);
        
        if (matcher.find()) {
            String values = matcher.group(1);
            Pattern varPattern = Pattern.compile(":([\\w-]+)");
            Matcher varMatcher = varPattern.matcher(values);
            
            while (varMatcher.find()) {
                vars.add(varMatcher.group(1));
            }
        }
        
        return vars;
    }

    private static List<String> extractSetVariables(String sql) {
        List<String> vars = new ArrayList<>();
        Pattern pattern = Pattern.compile("SET.*?(?=WHERE|$)", Pattern.CASE_INSENSITIVE);
        Matcher matcher = pattern.matcher(sql);
        
        if (matcher.find()) {
            String setClause = matcher.group(0);
            Pattern varPattern = Pattern.compile(":([\\w-]+)");
            Matcher varMatcher = varPattern.matcher(setClause);
            
            while (varMatcher.find()) {
                vars.add(varMatcher.group(1));
            }
        }
        
        return vars;
    }

    // Altri metodi per processare statement non-SQL

    private static void processAcceptStatement(Element acceptStmt, Element javaStmt) {
        javaStmt.setAttribute("type", "input");
        
        Node identifierNode = findDeepChildByName(acceptStmt, "identifier");
        if (identifierNode != null) {
            String identifier = extractIdentifier(identifierNode);
            String varType = declaredFields.getOrDefault(identifier.toUpperCase(), "String");
            
            if (varType.equals("String")) {
                javaStmt.setAttribute("expression", identifier + " = scanner.nextLine()");
            } else if (varType.equals("int")) {
                javaStmt.setAttribute("expression", identifier + " = scanner.nextInt()");
            } else if (varType.equals("BigDecimal")) {
                javaStmt.setAttribute("expression", identifier + " = scanner.nextBigDecimal()");
            }
        }
    }

    private static void processComputeStatement(Element computeStmt, Element javaStmt) {
        javaStmt.setAttribute("type", "arithmetic");
        
        Node storeNode = findDeepChildByName(computeStmt, "computeStore");
        Node exprNode = findDeepChildByName(computeStmt, "arithmeticExpression");
        
        if (storeNode != null && exprNode != null) {
            String target = extractIdentifier(storeNode);
            String expression = extractArithmeticExpression(exprNode);
            javaStmt.setAttribute("expression", target + " = " + expression);
        }
    }

    private static void processIfStatement(Element ifStmt, Element javaStmt, Document javaDoc, Element methodBody) {
        Element ifElement = javaDoc.createElement("if");
        
        // Extract condition
        Node conditionNode = findDeepChildByName(ifStmt, "condition");
        if (conditionNode != null) {
            String condition = extractCondition(conditionNode);
            ifElement.setAttribute("condition", condition);
        }
        
        // Process THEN part
        Node ifThenNode = findChildNodeByName(ifStmt, "ifThen");
        if (ifThenNode != null) {
            Element thenBlock = javaDoc.createElement("then");
            ifElement.appendChild(thenBlock);
            
            NodeList statements = ((Element)ifThenNode).getElementsByTagName("statement");
            for (int i = 0; i < statements.getLength(); i++) {
                Element statement = (Element) statements.item(i);
                Node statementTypeNode = getFirstElementChild(statement);
                if (statementTypeNode != null) {
                    processStatement((Element) statementTypeNode, javaDoc, thenBlock);
                }
            }
        }
        
        // Process ELSE part
        Node ifElseNode = findChildNodeByName(ifStmt, "ifElse");
        if (ifElseNode != null) {
            Element elseBlock = javaDoc.createElement("else");
            ifElement.appendChild(elseBlock);
            
            NodeList statements = ((Element)ifElseNode).getElementsByTagName("statement");
            for (int i = 0; i < statements.getLength(); i++) {
                Element statement = (Element) statements.item(i);
                Node statementTypeNode = getFirstElementChild(statement);
                if (statementTypeNode != null) {
                    processStatement((Element) statementTypeNode, javaDoc, elseBlock);
                }
            }
        }
        
        methodBody.appendChild(ifElement);
    }

    private static void processEvaluateStatement(Element evalStmt, Element javaStmt, Document javaDoc, Element methodBody) {
        Element switchElement = javaDoc.createElement("switch");
        
        // Extract the variable being evaluated
        Node selectNode = findDeepChildByName(evalStmt, "evaluateSelect");
        if (selectNode != null) {
            String variable = extractIdentifier(selectNode);
            switchElement.setAttribute("expression", variable);
        }
        
        // Process WHEN clauses
        NodeList whenPhrases = evalStmt.getElementsByTagName("evaluateWhenPhrase");
        for (int i = 0; i < whenPhrases.getLength(); i++) {
            Element whenPhrase = (Element) whenPhrases.item(i);
            
            Element caseElement = javaDoc.createElement("case");
            
            // Extract the value
            Node valueNode = findDeepChildByName(whenPhrase, "evaluateValue");
            if (valueNode != null) {
                String value = extractLiteralValue(valueNode);
                caseElement.setAttribute("value", value);
            }
            
            switchElement.appendChild(caseElement);
            
            // Process statements in this WHEN
            NodeList statements = whenPhrase.getElementsByTagName("statement");
            for (int j = 0; j < statements.getLength(); j++) {
                Element statement = (Element) statements.item(j);
                Node statementTypeNode = getFirstElementChild(statement);
                if (statementTypeNode != null) {
                    processStatement((Element) statementTypeNode, javaDoc, caseElement);
                }
            }
            
            // Add break
            Element breakStmt = javaDoc.createElement("statement");
            breakStmt.setAttribute("type", "break");
            caseElement.appendChild(breakStmt);
        }
        
        // Process WHEN OTHER
        Node whenOther = findChildNodeByName(evalStmt, "evaluateWhenOther");
        if (whenOther != null) {
            Element defaultElement = javaDoc.createElement("default");
            switchElement.appendChild(defaultElement);
            
            NodeList statements = ((Element)whenOther).getElementsByTagName("statement");
            for (int i = 0; i < statements.getLength(); i++) {
                Element statement = (Element) statements.item(i);
                Node statementTypeNode = getFirstElementChild(statement);
                if (statementTypeNode != null) {
                    processStatement((Element) statementTypeNode, javaDoc, defaultElement);
                }
            }
        }
        
        methodBody.appendChild(switchElement);
    }

    private static void processOpenStatement(Element openStmt, Element javaStmt) {
        javaStmt.setAttribute("type", "file-open");
        
        Node fileNameNode = findDeepChildByName(openStmt, "fileName");
        if (fileNameNode != null) {
            String fileName = extractTextFromNode(fileNameNode);
            
            // Check if it's OUTPUT or INPUT
            if (openStmt.getElementsByTagName("openOutputStatement").getLength() > 0) {
                javaStmt.setAttribute("expression", 
                    fileName.toLowerCase() + " = new PrintWriter(new FileWriter(\"ESTRATTO-CONTO.TXT\"))");
            } else {
                javaStmt.setAttribute("expression", 
                    fileName.toLowerCase() + " = new BufferedReader(new FileReader(\"ESTRATTO-CONTO.TXT\"))");
            }
        }
    }

    private static void processCloseStatement(Element closeStmt, Element javaStmt) {
        javaStmt.setAttribute("type", "file-close");
        
        Node fileNameNode = findDeepChildByName(closeStmt, "fileName");
        if (fileNameNode != null) {
            String fileName = extractTextFromNode(fileNameNode);
            javaStmt.setAttribute("expression", fileName.toLowerCase() + ".close()");
        }
    }

    private static void processWriteStatement(Element writeStmt, Element javaStmt) {
        javaStmt.setAttribute("type", "file-write");
        
        Node recordNode = findDeepChildByName(writeStmt, "recordName");
        Node fromNode = findDeepChildByName(writeStmt, "writeFromPhrase");
        
        if (recordNode != null) {
            String recordName = extractIdentifier(recordNode);
            
            if (fromNode != null) {
                Node fromIdentifier = findDeepChildByName(fromNode, "identifier");
                if (fromIdentifier != null) {
                    String fromVar = extractIdentifier(fromIdentifier);
                    javaStmt.setAttribute("expression", "reportFile.println(" + fromVar + ")");
                }
            } else {
                javaStmt.setAttribute("expression", "reportFile.println(" + recordName + ")");
            }
        }
    }

    private static void processStringStatement(Element stringStmt, Element javaStmt) {
        javaStmt.setAttribute("type", "string-concat");
        
        StringBuilder expression = new StringBuilder();
        Node intoNode = findDeepChildByName(stringStmt, "stringIntoPhrase");
        
        if (intoNode != null) {
            String intoVar = extractIdentifier(intoNode);
            expression.append(intoVar).append(" = ");
            
            // Get all string sending elements
            NodeList sendingNodes = stringStmt.getElementsByTagName("stringSending");
            for (int i = 0; i < sendingNodes.getLength(); i++) {
                if (i > 0) expression.append(" + ");
                
                Element sending = (Element) sendingNodes.item(i);
                Node literalNode = findDeepChildByName(sending, "literal");
                Node identifierNode = findDeepChildByName(sending, "identifier");
                
                if (literalNode != null) {
                    expression.append(extractLiteralValue(literalNode));
                } else if (identifierNode != null) {
                    expression.append(extractIdentifier(identifierNode));
                }
            }
            
            javaStmt.setAttribute("expression", expression.toString());
        }
    }

    private static String extractCondition(Node conditionNode) {
        // Simplified condition extraction
        StringBuilder condition = new StringBuilder();
        
        Node relationNode = findDeepChildByName(conditionNode, "relationCondition");
        if (relationNode != null) {
            Node leftNode = findDeepChildByName(relationNode, "arithmeticExpression");
            Node operatorNode = findDeepChildByName(relationNode, "relationalOperator");
            
            if (leftNode != null) {
                String left = extractArithmeticExpression(leftNode);
                String operator = extractRelationalOperator(operatorNode);
                
                // Find the right side (second arithmeticExpression)
                NodeList arithNodes = ((Element)relationNode).getElementsByTagName("arithmeticExpression");
                if (arithNodes.getLength() > 1) {
                    String right = extractArithmeticExpression(arithNodes.item(1));
                    condition.append(left).append(" ").append(operator).append(" ").append(right);
                }
            }
        }
        
        // Handle AND/OR conditions
        Node andOrNode = findDeepChildByName(conditionNode, "andOrCondition");
        if (andOrNode != null) {
            String operator = extractTextFromNode(andOrNode).contains("OR") ? " || " : " && ";
            Node abbrevNode = findDeepChildByName(andOrNode, "abbreviation");
            if (abbrevNode != null) {
                condition.append(operator).append(extractArithmeticExpression(abbrevNode));
            }
        }
        
        return condition.toString();
    }

    private static String extractArithmeticExpression(Node exprNode) {
        // Simplified arithmetic expression extraction
        return extractTextFromNode(exprNode).replaceAll("\\s+", " ").trim();
    }

    private static String extractRelationalOperator(Node operatorNode) {
        if (operatorNode == null) return "==";
        
        String operator = extractTextFromNode(operatorNode);
        if (operator.contains("NOT") && operator.contains("=")) return "!=";
        if (operator.contains("=")) return "==";
        if (operator.contains(">=")) return ">=";
        if (operator.contains("<=")) return "<=";
        if (operator.contains(">")) return ">";
        if (operator.contains("<")) return "<";
        
        return "==";
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
        
        // Check for WITH NO ADVANCING
        Node displayWithNode = findChildNodeByName(displayStmt, "displayWith");
        if (displayWithNode != null && extractTextFromNode(displayWithNode).contains("NO ADVANCING")) {
            // Use print instead of println
            expression = new StringBuilder(expression.toString().replace("System.out.println(", "System.out.print("));
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