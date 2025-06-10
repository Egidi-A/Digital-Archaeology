package com.DigArch.cobol2java.output;

import com.github.javaparser.ast.CompilationUnit;
import com.DigArch.cobol2java.model.JavaClass;
import com.DigArch.cobol2java.ast.builders.JavaClassBuilder;

/**
 * Generatore di codice Java responsabile della produzione del codice sorgente finale.
 * Questa classe si occupa della generazione del codice Java formattato a partire
 * dal modello della classe, aggiungendo le importazioni necessarie e applicando
 * la formattazione corretta.
 */
public class JavaCodeGenerator {
    
    /** Builder utilizzato per costruire l'AST Java dalla classe modello */
    private final JavaClassBuilder classBuilder;
    
    /**
     * Costruisce un nuovo generatore di codice con un JavaClassBuilder dedicato.
     */
    public JavaCodeGenerator() {
        this.classBuilder = new JavaClassBuilder();
    }
    
    /**
     * Genera il codice Java formattato a partire dal modello della classe.
     * Il metodo:
     * - Costruisce l'AST utilizzando il JavaClassBuilder
     * - Aggiunge le importazioni necessarie (es. BigDecimal)
     * - Genera il codice formattato
     *
     * @param javaClass Il modello della classe Java da generare
     * @return Il codice sorgente Java formattato come stringa
     */
    public String generate(JavaClass javaClass) {
        CompilationUnit cu = classBuilder.build(javaClass);
        
        // Add imports
        cu.addImport("java.math.BigDecimal");
        
        // Generate formatted Java code
        return cu.toString();
    }
}