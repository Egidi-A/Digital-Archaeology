/*
 * NOTA: Questo file Java è stato generato automaticamente da un Abstract Semantic Graph (ASG) in formato XML.
 * Ogni parte del codice, inclusi commenti e Javadoc, è stata derivata dalla struttura dell'XML
 * per riflettere fedelmente la sua rappresentazione del programma.
 */

// La dichiarazione del package è derivata dall'attributo 'package' del tag <java-compilation-unit>.
package com.generated;

/**
 * Rappresenta una classe generata dall'XML ASG.
 * Questa classe, nominata 'HelloWorld' seguendo le convenzioni Java (PascalCase)
 * a partire da 'hello_world' nell'XML, contiene un campo privato, i relativi
 * metodi getter/setter e un metodo main come punto di ingresso.
 * La visibilità della classe è 'public' come specificato nell'ASG.
 */
public class HelloWorld {

    /**
     * Campo privato per memorizzare un messaggio di tipo String.
     * Il nome del campo 'wsMessage' è stato convertito da 'ws_message'
     * per seguire la convenzione camelCase di Java.
     * La visibilità 'private' è definita nell'ASG.
     */
    private String wsMessage;

    /**
     * Metodo getter pubblico per il campo {@code wsMessage}.
     * Restituisce il valore corrente del messaggio.
     * Il nome del metodo 'getWsMessage' è stato convertito da 'getWs_message'.
     *
     * @return Il valore corrente di tipo String del campo wsMessage.
     */
    public String getWsMessage() {
        // L'istruzione di ritorno è derivata dall'attributo 'return="ws_message"' nel tag <body>.
        // Viene usato 'this' per chiarezza, per indicare che si sta accedendo a un campo dell'istanza.
        return this.wsMessage;
    }

    /**
     * Metodo setter pubblico per il campo {@code wsMessage}.
     * Imposta un nuovo valore per il messaggio.
     * Il nome del metodo 'setWsMessage' è stato convertito da 'setWs_message'.
     *
     * @param value Il nuovo valore di tipo String da assegnare al campo wsMessage.
     *              Questo parametro è definito dal tag <parameter> nell'ASG.
     */
    public void setWsMessage(String value) {
        // L'istruzione di assegnazione è derivata dall'attributo 'assignment="ws_message = value"' nel tag <body>.
        this.wsMessage = value;
    }

    /**
     * Il metodo main, punto di ingresso standard per un'applicazione Java.
     * Questo metodo è stato generato come 'public static' con un tipo di ritorno 'void',
     * come specificato nel tag <method> dell'ASG.
     *
     * @param args Argomenti passati dalla riga di comando (definiti dal tag <parameter>).
     *             In questa implementazione, gli argomenti non vengono utilizzati.
     */
    public static void main(String[] args) {
        // Il corpo del metodo main è vuoto, come definito dal tag <body/> vuoto nell'ASG XML.
        // Qui è possibile aggiungere la logica di business per eseguire l'applicazione.
        // Esempio di utilizzo della classe:
        //
        // HelloWorld hello = new HelloWorld();
        // hello.setWsMessage("Ciao Mondo dall'ASG!");
        // System.out.println(hello.getWsMessage());
    }
}