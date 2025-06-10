package com.DigArch.cobol2java.ast.mapping;

import com.github.javaparser.ast.stmt.*;
import com.github.javaparser.ast.expr.*;
import com.github.javaparser.ast.comments.LineComment;
import com.github.javaparser.ast.NodeList;
import com.github.javaparser.StaticJavaParser;
import java.util.ArrayList;
import java.util.List;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

/**
 * Mapper per la conversione di istruzioni COBOL in istruzioni Java.
 * Questa classe si occupa di tradurre le istruzioni COBOL più comuni
 * nelle loro equivalenti Java utilizzando JavaParser per generare l'AST.
 */
public class StatementMapper {
    
    /** Pattern per riconoscere l'istruzione COBOL DISPLAY */
    private static final Pattern DISPLAY_PATTERN = Pattern.compile("DISPLAY\\s+(.+)", Pattern.CASE_INSENSITIVE);
    
    /** Pattern per riconoscere l'istruzione COBOL MOVE TO */
    private static final Pattern MOVE_PATTERN = Pattern.compile("MOVE\\s+(.+)\\s+TO\\s+(.+)", Pattern.CASE_INSENSITIVE);
    
    /** Pattern per riconoscere l'istruzione COBOL PERFORM */
    private static final Pattern PERFORM_PATTERN = Pattern.compile("PERFORM\\s+(.+)", Pattern.CASE_INSENSITIVE);
    
    /** Pattern per riconoscere l'istruzione COBOL IF */
    private static final Pattern IF_PATTERN = Pattern.compile("IF\\s+(.+)", Pattern.CASE_INSENSITIVE);
    
    /**
     * Converte un'istruzione COBOL in una o più istruzioni Java.
     * Supporta le istruzioni DISPLAY, MOVE, PERFORM e IF.
     * Le istruzioni non riconosciute vengono convertite in commenti.
     *
     * @param cobolStatement L'istruzione COBOL da convertire
     * @return Lista di istruzioni Java equivalenti
     */
    public List<Statement> mapStatement(String cobolStatement) {
        List<Statement> statements = new ArrayList<>();
        String stmt = cobolStatement.trim();
        
        if (stmt.isEmpty()) return statements;
        
        Matcher displayMatcher = DISPLAY_PATTERN.matcher(stmt);
        if (displayMatcher.matches()) {
            statements.add(mapDisplay(displayMatcher.group(1)));
            return statements;
        }
        
        Matcher moveMatcher = MOVE_PATTERN.matcher(stmt);
        if (moveMatcher.matches()) {
            statements.add(mapMove(moveMatcher.group(1), moveMatcher.group(2)));
            return statements;
        }
        
        Matcher performMatcher = PERFORM_PATTERN.matcher(stmt);
        if (performMatcher.matches()) {
            statements.add(mapPerform(performMatcher.group(1)));
            return statements;
        }
        
        // Default: comment with original COBOL
        EmptyStmt emptyStmt = new EmptyStmt();
        LineComment comment = new LineComment(" " + stmt);
        emptyStmt.setComment(comment);
        statements.add(emptyStmt);
        return statements;
    }
    
    /**
     * Converte un'istruzione COBOL DISPLAY in una System.out.println Java.
     *
     * @param value Il valore da visualizzare
     * @return Statement Java equivalente
     */
    private Statement mapDisplay(String value) {
        String output = cleanValue(value);
        return new ExpressionStmt(
            new MethodCallExpr(
                new NameExpr("System.out"),
                "println",
                new NodeList<>(new StringLiteralExpr(output))
            )
        );
    }
    
    /**
     * Converte un'istruzione COBOL MOVE TO in un'assegnazione Java.
     *
     * @param source La variabile sorgente
     * @param target La variabile destinazione
     * @return Statement Java equivalente
     */
    private Statement mapMove(String source, String target) {
        String sourceVar = toJavaIdentifier(source.trim());
        String targetVar = toJavaIdentifier(target.trim());
        
        return new ExpressionStmt(
            new AssignExpr(
                new NameExpr(targetVar),
                new NameExpr(sourceVar),
                AssignExpr.Operator.ASSIGN
            )
        );
    }
    
    /**
     * Converte un'istruzione COBOL PERFORM in una chiamata a metodo Java.
     *
     * @param paragraph Il nome del paragrafo COBOL da eseguire
     * @return Statement Java equivalente
     */
    private Statement mapPerform(String paragraph) {
        String methodName = toJavaIdentifier(paragraph.trim());
        return new ExpressionStmt(
            new MethodCallExpr(methodName)
        );
    }
    
    /**
     * Pulisce un valore rimuovendo le quotature se presenti.
     *
     * @param value Il valore da pulire
     * @return Il valore senza quotature
     */
    private String cleanValue(String value) {
        value = value.trim();
        if ((value.startsWith("\"") && value.endsWith("\"")) ||
            (value.startsWith("'") && value.endsWith("'"))) {
            return value.substring(1, value.length() - 1);
        }
        return value;
    }
    
    /**
     * Converte un identificatore COBOL in un identificatore Java valido.
     * Sostituisce i trattini con underscore e rimuove gli spazi.
     *
     * @param cobolIdentifier L'identificatore COBOL da convertire
     * @return L'identificatore Java equivalente
     */
    private String toJavaIdentifier(String cobolIdentifier) {
        return cobolIdentifier.toLowerCase()
            .replace("-", "_")
            .replace(" ", "");
    }
}