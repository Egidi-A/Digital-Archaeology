/**
 * Generated from COBOL program: GESTIONE-CONTI
 * Author: ANNALISA-EGIDI
 * Migration Date: 2025-04-28
 * 
 * Original COBOL source: bank_system_cobol.txt
 * This class manages bank account operations including account opening,
 * deposits, withdrawals, balance inquiries, and account statements.
 */
package com.miriade.cobol.migrated;

import java.math.BigDecimal;
import java.sql.*;
import java.time.LocalDate;
import java.time.LocalDateTime;
import java.util.Scanner;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

@Service
public class GestioneConti {
    
    private static final Logger logger = LoggerFactory.getLogger(GestioneConti.class);
    
    // Database connection
    @Autowired
    private DataSource dataSource;
    private Connection connection;
    
    // Working Storage Section
    private static int wsScelta = 0;
    private static String wsContinua = "S";
    private static String wsMessaggio = "";
    private static String wsEsito = "";
    
    // Variables for database
    private static String wsNumeroConto = "";
    private static String wsCodiceCliente = "";
    private static BigDecimal wsImporto = BigDecimal.ZERO;
    private static BigDecimal wsSaldo = BigDecimal.ZERO;
    private static String wsTipoMovimento = "";
    private static String wsCausale = "";
    private static LocalDate wsDataSistema;
    private static String wsOraSistema = "";
    
    // Data structure: Cliente
    static class Cliente {
        private String codice;
        private String nome;
        private String cognome;
        private String codiceFiscale;
        private LocalDate dataNascita;
        private String indirizzo;
        private String citta;
        private String cap;
        private String telefono;
        private String email;
        
        // Getters and setters
        public String getCodice() { return codice; }
        public void setCodice(String codice) { this.codice = codice; }
        
        public String getNome() { return nome; }
        public void setNome(String nome) { this.nome = nome; }
        
        public String getCognome() { return cognome; }
        public void setCognome(String cognome) { this.cognome = cognome; }
        
        public String getCodiceFiscale() { return codiceFiscale; }
        public void setCodiceFiscale(String codiceFiscale) { this.codiceFiscale = codiceFiscale; }
        
        // ... other getters/setters
    }
    
    // Data structure: Conto
    static class Conto {
        private String numero;
        private String codiceCliente;
        private String tipo;
        private BigDecimal saldo;
        private LocalDate dataApertura;
        private String stato;
        private BigDecimal fido;
        
        // Getters and setters
        public String getNumero() { return numero; }
        public void setNumero(String numero) { this.numero = numero; }
        
        public BigDecimal getSaldo() { return saldo; }
        public void setSaldo(BigDecimal saldo) { this.saldo = saldo; }
        
        // ... other getters/setters
    }
    
    private static Cliente wsCliente = new Cliente();
    private static Conto wsConto = new Conto();
    private static Scanner scanner = new Scanner(System.in);
    
    // Constructor
    public GestioneConti() {
        logger.info("Initializing GestioneConti");
    }
    
    // Main logic - converted from MAIN-LOGIC paragraph
    public void mainLogic() {
        connettiDatabase();
        
        while ("S".equalsIgnoreCase(wsContinua)) {
            visualizzaMenu();
            elaboraScelta();
            
            System.out.println();
            System.out.print("Continuare? (S/N): ");
            wsContinua = scanner.nextLine();
        }
        
        disconnettiDatabase();
    }
    
    // Connect to database - converted from CONNETTI-DATABASE
    private void connettiDatabase() {
        try {
            connection = dataSource.getConnection();
            logger.info("Connessione al database stabilita");
            System.out.println("Connessione al database stabilita");
        } catch (SQLException e) {
            logger.error("Errore connessione database: {}", e.getMessage());
            System.err.println("Errore connessione database: " + e.getSQLState());
            System.exit(1);
        }
    }
    
    // Disconnect from database - converted from DISCONNETTI-DATABASE
    private void disconnettiDatabase() {
        try {
            if (connection != null && !connection.isClosed()) {
                connection.close();
                logger.info("Disconnesso dal database");
                System.out.println("Disconnesso dal database");
            }
        } catch (SQLException e) {
            logger.error("Errore disconnessione: {}", e.getMessage());
        }
    }
    
    // Display menu - converted from VISUALIZZA-MENU
    private void visualizzaMenu() {
        System.out.println();
        System.out.println("===== SISTEMA GESTIONE CONTI CORRENTI =====");
        System.out.println("1. Apertura nuovo conto");
        System.out.println("2. Deposito");
        System.out.println("3. Prelievo");
        System.out.println("4. Visualizza saldo");
        System.out.println("5. Estratto conto");
        System.out.println("6. Chiusura conto");
        System.out.println("0. Esci");
        System.out.println("===========================================");
        System.out.print("Scelta: ");
        wsScelta = Integer.parseInt(scanner.nextLine());
    }
    
    // Process choice - converted from ELABORA-SCELTA
    private void elaboraScelta() {
        switch (wsScelta) {
            case 1:
                aperturaConto();
                break;
            case 2:
                deposito();
                break;
            case 3:
                prelievo();
                break;
            case 4:
                visualizzaSaldo();
                break;
            case 5:
                estrattoConto();
                break;
            case 6:
                chiusuraConto();
                break;
            case 0:
                wsContinua = "N";
                break;
            default:
                System.out.println("Scelta non valida!");
                break;
        }
    }
    
    // Open new account - converted from APERTURA-CONTO
    private void aperturaConto() {
        System.out.println();
        System.out.println("=== APERTURA NUOVO CONTO ===");
        
        System.out.print("Codice cliente: ");
        wsConto.setCodiceCliente(scanner.nextLine());
        
        // Verify client exists - converted from EXEC SQL
        try {
            String sql = "SELECT codice_cliente, nome, cognome FROM CLIENTI WHERE codice_cliente = ?";
            PreparedStatement pstmt = connection.prepareStatement(sql);
            pstmt.setString(1, wsConto.getCodiceCliente());
            
            ResultSet rs = pstmt.executeQuery();
            if (rs.next()) {
                wsCliente.setCodice(rs.getString("codice_cliente"));
                wsCliente.setNome(rs.getString("nome"));
                wsCliente.setCognome(rs.getString("cognome"));
                System.out.println("Cliente: " + wsCliente.getNome() + " " + wsCliente.getCognome());
            } else {
                System.out.println("Cliente non trovato!");
                return;
            }
            
            rs.close();
            pstmt.close();
        } catch (SQLException e) {
            logger.error("Errore database: {}", e.getMessage());
            System.out.println("Errore database: " + e.getSQLState());
            return;
        }
        
        // Generate new account number
        generaNumeroConto();
        
        System.out.print("Tipo conto (C=Corrente, D=Deposito): ");
        wsConto.setTipo(scanner.nextLine());
        
        System.out.print("Importo iniziale: ");
        wsConto.setSaldo(new BigDecimal(scanner.nextLine()));
        
        System.out.print("Fido accordato: ");
        wsConto.setFido(new BigDecimal(scanner.nextLine()));
        
        wsConto.setDataApertura(LocalDate.now());
        wsConto.setStato("A");
        
        // Insert new account
        try {
            String sql = "INSERT INTO CONTI (numero_conto, codice_cliente, tipo_conto, " +
                        "saldo, data_apertura, stato, fido) VALUES (?, ?, ?, ?, CURRENT_DATE, ?, ?)";
            PreparedStatement pstmt = connection.prepareStatement(sql);
            pstmt.setString(1, wsConto.getNumero());
            pstmt.setString(2, wsConto.getCodiceCliente());
            pstmt.setString(3, wsConto.getTipo());
            pstmt.setBigDecimal(4, wsConto.getSaldo());
            pstmt.setString(5, wsConto.getStato());
            pstmt.setBigDecimal(6, wsConto.getFido());
            
            int rowsAffected = pstmt.executeUpdate();
            pstmt.close();
            
            if (rowsAffected > 0) {
                System.out.println("Conto " + wsConto.getNumero() + " creato con successo!");
                logger.info("Nuovo conto creato: {}", wsConto.getNumero());
                
                // Register initial deposit if amount > 0
                if (wsConto.getSaldo().compareTo(BigDecimal.ZERO) > 0) {
                    wsTipoMovimento = "D";
                    wsCausale = "Deposito iniziale";
                    wsImporto = wsConto.getSaldo();
                    wsNumeroConto = wsConto.getNumero();
                    registraMovimento();
                }
            }
        } catch (SQLException e) {
            logger.error("Errore creazione conto: {}", e.getMessage());
            System.out.println("Errore creazione conto: " + e.getSQLState());
        }
    }
    
    // Generate account number - converted from GENERA-NUMERO-CONTO
    private void generaNumeroConto() {
        try {
            String sql = "SELECT 'IT' || LPAD(" +
                        "CAST(COALESCE(MAX(CAST(SUBSTR(numero_conto, 3) AS INTEGER)), 0) + 1 AS VARCHAR), 10, '0') " +
                        "FROM CONTI WHERE numero_conto LIKE 'IT%'";
            Statement stmt = connection.createStatement();
            ResultSet rs = stmt.executeQuery(sql);
            
            if (rs.next()) {
                wsConto.setNumero(rs.getString(1));
            }
            
            rs.close();
            stmt.close();
        } catch (SQLException e) {
            logger.error("Errore generazione numero conto: {}", e.getMessage());
        }
    }
    
    // Deposit - converted from DEPOSITO
    private void deposito() {
        System.out.println();
        System.out.println("=== DEPOSITO ===");
        
        System.out.print("Numero conto: ");
        wsNumeroConto = scanner.nextLine();
        
        verificaConto();
        if ("KO".equals(wsEsito)) {
            return;
        }
        
        System.out.print("Importo deposito: ");
        wsImporto = new BigDecimal(scanner.nextLine());
        
        if (wsImporto.compareTo(BigDecimal.ZERO) <= 0) {
            System.out.println("Importo non valido!");
            return;
        }
        
        System.out.print("Causale: ");
        wsCausale = scanner.nextLine();
        
        // Update balance
        try {
            String sql = "UPDATE CONTI SET saldo = saldo + ? WHERE numero_conto = ? AND stato = 'A'";
            PreparedStatement pstmt = connection.prepareStatement(sql);
            pstmt.setBigDecimal(1, wsImporto);
            pstmt.setString(2, wsNumeroConto);
            
            int rowsAffected = pstmt.executeUpdate();
            pstmt.close();
            
            if (rowsAffected > 0) {
                wsTipoMovimento = "D";
                registraMovimento();
                System.out.println("Deposito effettuato con successo!");
                logger.info("Deposito di {} su conto {}", wsImporto, wsNumeroConto);
            }
        } catch (SQLException e) {
            logger.error("Errore durante il deposito: {}", e.getMessage());
            System.out.println("Errore durante il deposito: " + e.getSQLState());
        }
    }
    
    // Withdrawal - converted from PRELIEVO
    private void prelievo() {
        System.out.println();
        System.out.println("=== PRELIEVO ===");
        
        System.out.print("Numero conto: ");
        wsNumeroConto = scanner.nextLine();
        
        verificaConto();
        if ("KO".equals(wsEsito)) {
            return;
        }
        
        System.out.print("Importo prelievo: ");
        wsImporto = new BigDecimal(scanner.nextLine());
        
        if (wsImporto.compareTo(BigDecimal.ZERO) <= 0) {
            System.out.println("Importo non valido!");
            return;
        }
        
        // Verify availability
        try {
            String sql = "SELECT saldo, fido FROM CONTI WHERE numero_conto = ? AND stato = 'A'";
            PreparedStatement pstmt = connection.prepareStatement(sql);
            pstmt.setString(1, wsNumeroConto);
            
            ResultSet rs = pstmt.executeQuery();
            if (rs.next()) {
                wsConto.setSaldo(rs.getBigDecimal("saldo"));
                wsConto.setFido(rs.getBigDecimal("fido"));
                
                BigDecimal disponibile = wsConto.getSaldo().add(wsConto.getFido());
                if (wsImporto.compareTo(disponibile) > 0) {
                    System.out.println("Fondi insufficienti!");
                    System.out.println("Saldo attuale: " + wsConto.getSaldo());
                    System.out.println("Fido disponibile: " + wsConto.getFido());
                    return;
                }
            }
            
            rs.close();
            pstmt.close();
        } catch (SQLException e) {
            logger.error("Errore verifica disponibilità: {}", e.getMessage());
            return;
        }
        
        System.out.print("Causale: ");
        wsCausale = scanner.nextLine();
        
        // Update balance
        try {
            String sql = "UPDATE CONTI SET saldo = saldo - ? WHERE numero_conto = ? AND stato = 'A'";
            PreparedStatement pstmt = connection.prepareStatement(sql);
            pstmt.setBigDecimal(1, wsImporto);
            pstmt.setString(2, wsNumeroConto);
            
            int rowsAffected = pstmt.executeUpdate();
            pstmt.close();
            
            if (rowsAffected > 0) {
                wsTipoMovimento = "P";
                registraMovimento();
                System.out.println("Prelievo effettuato con successo!");
                logger.info("Prelievo di {} da conto {}", wsImporto, wsNumeroConto);
            }
        } catch (SQLException e) {
            logger.error("Errore durante il prelievo: {}", e.getMessage());
            System.out.println("Errore durante il prelievo: " + e.getSQLState());
        }
    }
    
    // View balance - converted from VISUALIZZA-SALDO
    private void visualizzaSaldo() {
        System.out.println();
        System.out.println("=== VISUALIZZA SALDO ===");
        
        System.out.print("Numero conto: ");
        wsNumeroConto = scanner.nextLine();
        
        try {
            String sql = "SELECT c.saldo, c.fido, cl.nome, cl.cognome " +
                        "FROM CONTI c JOIN CLIENTI cl ON c.codice_cliente = cl.codice_cliente " +
                        "WHERE c.numero_conto = ? AND c.stato = 'A'";
            PreparedStatement pstmt = connection.prepareStatement(sql);
            pstmt.setString(1, wsNumeroConto);
            
            ResultSet rs = pstmt.executeQuery();
            if (rs.next()) {
                wsConto.setSaldo(rs.getBigDecimal("saldo"));
                wsConto.setFido(rs.getBigDecimal("fido"));
                wsCliente.setNome(rs.getString("nome"));
                wsCliente.setCognome(rs.getString("cognome"));
                
                System.out.println();
                System.out.println("Intestatario: " + wsCliente.getNome() + " " + wsCliente.getCognome());
                System.out.println("Saldo attuale: EUR " + wsConto.getSaldo());
                System.out.println("Fido accordato: EUR " + wsConto.getFido());
                
                BigDecimal disponibile = wsConto.getSaldo().add(wsConto.getFido());
                System.out.println("Disponibile: EUR " + disponibile);
            } else {
                System.out.println("Conto non trovato o non attivo!");
            }
            
            rs.close();
            pstmt.close();
        } catch (SQLException e) {
            logger.error("Errore database: {}", e.getMessage());
            System.out.println("Errore database: " + e.getSQLState());
        }
    }
    
    // Account statement - converted from ESTRATTO-CONTO
    private void estrattoConto() {
        System.out.println();
        System.out.println("=== ESTRATTO CONTO ===");
        
        System.out.print("Numero conto: ");
        wsNumeroConto = scanner.nextLine();
        
        verificaConto();
        if ("KO".equals(wsEsito)) {
            return;
        }
        
        // Generate statement report
        AccountStatementGenerator generator = new AccountStatementGenerator(connection);
        generator.generateStatement(wsNumeroConto, wsCliente);
        
        System.out.println("Estratto conto salvato in ESTRATTO-CONTO.TXT");
    }
    
    // Close account - converted from CHIUSURA-CONTO
    private void chiusuraConto() {
        System.out.println();
        System.out.println("=== CHIUSURA CONTO ===");
        
        System.out.print("Numero conto da chiudere: ");
        wsNumeroConto = scanner.nextLine();
        
        verificaConto();
        if ("KO".equals(wsEsito)) {
            return;
        }
        
        // Verify zero balance
        if (wsConto.getSaldo().compareTo(BigDecimal.ZERO) != 0) {
            System.out.println("Impossibile chiudere: saldo non zero!");
            System.out.println("Saldo attuale: EUR " + wsConto.getSaldo());
            return;
        }
        
        System.out.print("Confermare chiusura conto (S/N): ");
        String conferma = scanner.nextLine();
        
        if ("S".equalsIgnoreCase(conferma)) {
            try {
                String sql = "UPDATE CONTI SET stato = 'C', data_chiusura = CURRENT_DATE " +
                            "WHERE numero_conto = ?";
                PreparedStatement pstmt = connection.prepareStatement(sql);
                pstmt.setString(1, wsNumeroConto);
                
                int rowsAffected = pstmt.executeUpdate();
                pstmt.close();
                
                if (rowsAffected > 0) {
                    System.out.println("Conto chiuso con successo!");
                    logger.info("Conto {} chiuso", wsNumeroConto);
                }
            } catch (SQLException e) {
                logger.error("Errore chiusura conto: {}", e.getMessage());
                System.out.println("Errore chiusura conto: " + e.getSQLState());
            }
        } else {
            System.out.println("Chiusura annullata");
        }
    }
    
    // Verify account - converted from VERIFICA-CONTO
    private void verificaConto() {
        wsEsito = "OK";
        
        try {
            String sql = "SELECT c.saldo, c.stato, cl.nome, cl.cognome " +
                        "FROM CONTI c JOIN CLIENTI cl ON c.codice_cliente = cl.codice_cliente " +
                        "WHERE c.numero_conto = ?";
            PreparedStatement pstmt = connection.prepareStatement(sql);
            pstmt.setString(1, wsNumeroConto);
            
            ResultSet rs = pstmt.executeQuery();
            if (rs.next()) {
                wsConto.setSaldo(rs.getBigDecimal("saldo"));
                wsConto.setStato(rs.getString("stato"));
                wsCliente.setNome(rs.getString("nome"));
                wsCliente.setCognome(rs.getString("cognome"));
                
                if (!"A".equals(wsConto.getStato())) {
                    System.out.println("Conto non attivo!");
                    wsEsito = "KO";
                }
            } else {
                System.out.println("Conto non trovato!");
                wsEsito = "KO";
            }
            
            rs.close();
            pstmt.close();
        } catch (SQLException e) {
            logger.error("Errore database: {}", e.getMessage());
            System.out.println("Errore database: " + e.getSQLState());
            wsEsito = "KO";
        }
    }
    
    // Register movement - converted from REGISTRA-MOVIMENTO
    private void registraMovimento() {
        try {
            // Get current balance
            String sqlSelect = "SELECT saldo FROM CONTI WHERE numero_conto = ?";
            PreparedStatement pstmtSelect = connection.prepareStatement(sqlSelect);
            pstmtSelect.setString(1, wsNumeroConto);
            ResultSet rs = pstmtSelect.executeQuery();
            
            if (rs.next()) {
                wsSaldo = rs.getBigDecimal("saldo");
            }
            rs.close();
            pstmtSelect.close();
            
            // Insert movement
            String sql = "INSERT INTO MOVIMENTI (numero_conto, tipo_movimento, importo, " +
                        "causale, saldo_dopo, eseguito_da) VALUES (?, ?, ?, ?, ?, ?)";
            PreparedStatement pstmt = connection.prepareStatement(sql);
            pstmt.setString(1, wsNumeroConto);
            pstmt.setString(2, wsTipoMovimento);
            pstmt.setBigDecimal(3, wsImporto);
            pstmt.setString(4, wsCausale);
            pstmt.setBigDecimal(5, wsSaldo);
            pstmt.setString(6, "SISTEMA");
            
            pstmt.executeUpdate();
            pstmt.close();
            
            logger.debug("Movimento registrato: {} {} su conto {}", wsTipoMovimento, wsImporto, wsNumeroConto);
        } catch (SQLException e) {
            logger.error("Errore registrazione movimento: {}", e.getMessage());
        }
    }
    
    // Main method
    public static void main(String[] args) {
        GestioneConti app = new GestioneConti();
        app.mainLogic();
    }
}