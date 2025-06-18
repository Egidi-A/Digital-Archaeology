package com.example;

import io.proleap.cobol.asg.metamodel.Program;
// Import necessari per navigare il Parse Tree
import io.proleap.cobol.CobolParser;
import org.antlr.v4.runtime.tree.ParseTree;

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

            Element rootElement = doc.createElement("cobol-parse-tree");
            doc.appendChild(rootElement);

            // Il 'program' contiene una o più "compilation units".
            // Ogni unit ha un "parse tree context" (ctx), che è la radice del nostro albero.
            // Serializziamo il primo albero di parsing che troviamo.
            if (program.getCompilationUnits() != null && !program.getCompilationUnits().isEmpty()) {
                ParseTree tree = program.getCompilationUnits().get(0).getCtx();
                if (tree != null) {
                    buildXml(doc, rootElement, tree);
                }
            }

            TransformerFactory transformerFactory = TransformerFactory.newInstance();
            Transformer transformer = transformerFactory.newTransformer();
            transformer.setOutputProperty(OutputKeys.INDENT, "yes");
            transformer.setOutputProperty("{http://xml.apache.org/xslt}indent-amount", "2");

            DOMSource source = new DOMSource(doc);
            StreamResult result = new StreamResult(new File(outputPath));
            transformer.transform(source, result);

            System.out.println("XML del Parse Tree (massimo dettaglio) salvato in: " + outputPath);

        } catch (Exception e) {
            e.printStackTrace();
        }
    }

    /**
     * Metodo ricorsivo per costruire l'XML navigando il Parse Tree.
     * @param doc Il documento XML in costruzione.
     * @param parent L'elemento XML a cui aggiungere i nuovi nodi.
     * @param tree Il nodo corrente del Parse Tree da processare.
     */
    private static void buildXml(Document doc, Element parent, ParseTree tree) {
        String ruleName;
        // Ottiene il nome della regola grammaticale per il nodo corrente
        if (tree.getPayload() instanceof org.antlr.v4.runtime.RuleContext) {
            int ruleIndex = ((org.antlr.v4.runtime.RuleContext) tree.getPayload()).getRuleIndex();
            ruleName = CobolParser.ruleNames[ruleIndex];
        } else {
            // È un nodo "foglia" (un token, es. una parola chiave, un nome, un numero)
            ruleName = "Token";
        }

        Element element = doc.createElement(ruleName);
        parent.appendChild(element);

        // Se è un nodo foglia, aggiungiamo il suo contenuto testuale
        if (tree.getChildCount() == 0) {
            String text = tree.getText();
            // Ignoriamo il token di fine file per pulizia
            if (text != null && !text.equalsIgnoreCase("<EOF>")) {
                element.appendChild(doc.createTextNode(text));
            }
        } else {
            // Se è un nodo interno, chiamiamo ricorsivamente la funzione per ogni suo "figlio"
            for (int i = 0; i < tree.getChildCount(); i++) {
                buildXml(doc, element, tree.getChild(i));
            }
        }
    }
}