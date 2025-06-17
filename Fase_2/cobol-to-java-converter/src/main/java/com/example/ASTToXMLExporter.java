package com.example;

import io.proleap.cobol.asg.metamodel.CompilationUnit;
import io.proleap.cobol.asg.metamodel.Program;
import io.proleap.cobol.asg.metamodel.ProgramUnit;
import io.proleap.cobol.asg.metamodel.data.datadescription.DataDescriptionEntry;
import io.proleap.cobol.asg.metamodel.procedure.Paragraph;
import io.proleap.cobol.asg.metamodel.procedure.Statement;

import javax.xml.parsers.DocumentBuilder;
import javax.xml.parsers.DocumentBuilderFactory;
import javax.xml.transform.OutputKeys;
import javax.xml.transform.Transformer;
import javax.xml.transform.TransformerFactory;
import javax.xml.transform.dom.DOMSource;
import javax.xml.transform.stream.StreamResult;
import org.w3c.dom.Document;
import org.w3c.dom.Element;

import java.io.File;

public class ASTToXMLExporter {
    
    public static void exportToXML(Program program, String outputPath) {
        try {
            DocumentBuilderFactory dbFactory = DocumentBuilderFactory.newInstance();
            DocumentBuilder dBuilder = dbFactory.newDocumentBuilder();
            Document doc = dBuilder.newDocument();
            
            // Root element
            Element rootElement = doc.createElement("cobol-program");
            doc.appendChild(rootElement);
            
            // Process compilation units
            for (CompilationUnit cu : program.getCompilationUnits()) {
                Element cuElement = doc.createElement("compilation-unit");
                cuElement.setAttribute("name", cu.getName());
                rootElement.appendChild(cuElement);
                
                if (cu.getProgramUnit() != null) {
                    processProgramUnit(doc, cuElement, cu.getProgramUnit());
                }
            }
            
            // Write to XML file
            TransformerFactory transformerFactory = TransformerFactory.newInstance();
            Transformer transformer = transformerFactory.newTransformer();
            transformer.setOutputProperty(OutputKeys.INDENT, "yes");
            transformer.setOutputProperty("{http://xml.apache.org/xslt}indent-amount", "2");
            
            DOMSource source = new DOMSource(doc);
            StreamResult result = new StreamResult(new File(outputPath));
            transformer.transform(source, result);
            
            System.out.println("XML salvato in: " + outputPath);
            
        } catch (Exception e) {
            e.printStackTrace();
        }
    }
    
    private static void processProgramUnit(Document doc, Element parent, ProgramUnit pu) {
        Element puElement = doc.createElement("program-unit");
        if (pu.getIdentificationDivision() != null && pu.getIdentificationDivision().getProgramIdParagraph() != null) {
            puElement.setAttribute("name", pu.getIdentificationDivision().getProgramIdParagraph().getName());
        }
        parent.appendChild(puElement);
        
        // Identification Division
        if (pu.getIdentificationDivision() != null) {
            Element idDiv = doc.createElement("identification-division");
            idDiv.setAttribute("program-id", pu.getIdentificationDivision()
                .getProgramIdParagraph().getName());
            puElement.appendChild(idDiv);
        }
        
        // Data Division
        if (pu.getDataDivision() != null) {
            Element dataDiv = doc.createElement("data-division");
            puElement.appendChild(dataDiv);
            
            // Working Storage Section
            if (pu.getDataDivision().getWorkingStorageSection() != null) {
                Element wsSection = doc.createElement("working-storage-section");
                dataDiv.appendChild(wsSection);
                
                for (io.proleap.cobol.asg.metamodel.ASGElement element : pu.getDataDivision()
                        .getWorkingStorageSection().getDataDescriptionEntries()) {
                    if (element instanceof DataDescriptionEntry) {
                        processDataEntry(doc, wsSection, (DataDescriptionEntry) element);
                    }
                }
            }
        }
        
        // Procedure Division
        if (pu.getProcedureDivision() != null) {
            Element procDiv = doc.createElement("procedure-division");
            puElement.appendChild(procDiv);
            
            for (Paragraph paragraph : pu.getProcedureDivision().getParagraphs()) {
                Element paraElement = doc.createElement("paragraph");
                paraElement.setAttribute("name", paragraph.getName());
                procDiv.appendChild(paraElement);
                
                for (Statement stmt : paragraph.getStatements()) {
                    Element stmtElement = doc.createElement("statement");
                    stmtElement.setAttribute("type", stmt.getStatementType().toString());
                    paraElement.appendChild(stmtElement);
                }
            }
        }
    }
    
    private static void processDataEntry(Document doc, Element parent, DataDescriptionEntry entry) {
        Element dataElement = doc.createElement("data-entry");
        dataElement.setAttribute("name", entry.getName());
        dataElement.setAttribute("level", String.valueOf(entry.getLevelNumber()));
        
        parent.appendChild(dataElement);
        
        // Ricorsione per le entry figlie
        if (entry.getChildren() != null) {
            for (io.proleap.cobol.asg.metamodel.ASGElement child : entry.getChildren()) {
                if (child instanceof DataDescriptionEntry) {
                    processDataEntry(doc, dataElement, (DataDescriptionEntry) child);
                }
            }
        }
    }
}