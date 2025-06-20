import os
import sys
import argparse
import google.generativeai as genai
from pathlib import Path

def setup_genai():
    """Configura l'API di Google Generative AI e restituisce il modello configurato."""
    API_KEY = "YOUR_API_KEY"  # Considera di spostare API_KEY in un posto più sicuro
    genai.configure(api_key=API_KEY)

    generation_config = {
        "temperature": 0.1,
        "top_p": 0.9,
        "top_k": 30,
        "max_output_tokens": 25000,
        "response_mime_type": "text/plain",
    }
    
    model = genai.GenerativeModel(
        model_name="gemini-2.5-pro-preview-06-05",
        generation_config=generation_config,
    )
    return model

def read_java_file(file_path):
    """Legge il contenuto del file Java"""
    try:
        with open(file_path, 'r', encoding='utf-8') as file:
            return file.read()
    except FileNotFoundError:
        print(f"❌ Errore: File '{file_path}' non trovato")
        sys.exit(1)
    except Exception as e:
        print(f"❌ Errore nella lettura del file: {e}")
        sys.exit(1)

def analyze_java_methods(java_code):
    """Analizza i metodi Java e la loro visibilità"""
    lines = java_code.split('\n')
    methods_info = []
    
    for i, line in enumerate(lines):
        line = line.strip()
        if ('(' in line and ')' in line and 
            ('public' in line or 'private' in line or 'protected' in line) and
            ('void' in line or 'int' in line or 'String' in line or 'boolean' in line or 'double' in line)):
            
            # Determina la visibilità
            visibility = 'package'  # default
            if 'public' in line:
                visibility = 'public'
            elif 'private' in line:
                visibility = 'private'
            elif 'protected' in line:
                visibility = 'protected'
            
            # Estrai il nome del metodo
            method_name = extract_method_name(line)
            if method_name:
                methods_info.append({
                    'name': method_name,
                    'visibility': visibility,
                    'line': i + 1,
                    'signature': line
                })
    
    return methods_info

def extract_method_name(line):
    """Estrae il nome del metodo dalla riga"""
    try:
        # Rimuovi modificatori e tipo di ritorno
        parts = line.strip().split('(')[0].split()
        for part in reversed(parts):
            if not part in ['public', 'private', 'protected', 'static', 'final', 'void', 'int', 'String', 'boolean', 'double', 'float', 'long', 'short', 'byte', 'char']:
                return part
    except:
        pass
    return None

def create_prompt(java_code, class_name, methods_info):
    """Crea il prompt per l'AI"""
    
    # Analizza quali metodi sono testabili
    public_methods = [m for m in methods_info if m['visibility'] == 'public']
    private_methods = [m for m in methods_info if m['visibility'] == 'private']
    
    visibility_note = ""
    if private_methods:
        visibility_note = f"""
ATTENZIONE - VISIBILITÀ DEI METODI:
I seguenti metodi sono PRIVATE e NON possono essere testati direttamente:
{', '.join([m['name'] for m in private_methods])}

I seguenti metodi sono PUBLIC e possono essere testati:
{', '.join([m['name'] for m in public_methods]) if public_methods else 'NESSUNO'}
"""

    return f"""
Analizza il seguente codice Java e genera unit test completi usando JUnit 5 che siano COMPATIBILI con Java 21.

{visibility_note}

CODICE JAVA:
```java
{java_code}
```

REQUISITI CRITICI per Java 21 e Mockito:
1. USA SOLO JUnit 5 (Jupiter) - NON usare @ExtendWith(MockitoExtension.class)
2. NON usare @Mock annotations - USA SOLO Mockito.mock() programmaticamente
3. Per java.sql.Connection usa ALWAYS un approccio alternativo:
   - Usa @MockBean se disponibile
   - Oppure usa MockDataSource invece di Connection
   - Oppure evita completamente il mocking di Connection e usa test integration
4. Inizializza i mock manualmente in @BeforeEach con Mockito.mock()
5. Chiudi i mock manualmente in @AfterEach se necessario

STRATEGIE SPECIFICHE PER MOCKITO + JAVA 21:
```java
// INVECE DI:
@Mock
private Connection connection;

// USA:
private Connection connection;

@BeforeEach
void setUp() {{
    // Opzione 1: Mock manuale (può fallire su Java 21)
    // connection = Mockito.mock(Connection.class);
    
    // Opzione 2: Usa H2 in-memory database (RACCOMANDATO)
    // connection = DriverManager.getConnection("jdbc:h2:mem:testdb");
    
    // Opzione 3: Stub semplice senza mock
    connection = createTestConnection();
}}

private Connection createTestConnection() {{
    // Implementazione stub o H2 database
    return null; // Da implementare secondo necessità
}}
```

APPROCCIO RACCOMANDATO:
1. Usa H2 Database in-memory per i test invece di mockare Connection
2. Testa i metodi public usando reflection per accedere ai private se necessario
3. Evita @ExtendWith(MockitoExtension.class)
4. Usa MockWebServer per test HTTP se necessario
5. Usa TestContainers per database reali se appropriato

CONFIGURAZIONE DEPENDENCY per H2:
```xml
<dependency>
    <groupId>com.h2database</groupId>
    <artifactId>h2</artifactId>
    <version>2.2.224</version>
    <scope>test</scope>
</dependency>
```

EXAMPLE TEST STRUCTURE:
```java
class {class_name}Test {{
    
    private {class_name} target;
    private Connection connection;
    
    @BeforeEach
    void setUp() throws SQLException {{
        // Usa H2 in-memory database invece di mock
        connection = DriverManager.getConnection("jdbc:h2:mem:testdb;DB_CLOSE_DELAY=-1");
        target = new {class_name}();
        setupTestDatabase();
    }}
    
    @AfterEach
    void tearDown() throws SQLException {{
        if (connection != null && !connection.isClosed()) {{
            connection.close();
        }}
    }}
    
    private void setupTestDatabase() throws SQLException {{
        // Crea tabelle di test
        try (Statement stmt = connection.createStatement()) {{
            stmt.execute("CREATE TABLE IF NOT EXISTS test_table (id INT PRIMARY KEY, name VARCHAR(50))");
        }}
    }}
    
    @Test
    void testMethodUsingDatabase() throws Exception {{
        // Test che usa database reale invece di mock
    }}
    
    @Test
    void testPrivateMethodUsingReflection() throws Exception {{
        // Usa reflection per testare metodi private
        Method privateMethod = {class_name}.class.getDeclaredMethod("privateMethodName");
        privateMethod.setAccessible(true);
        Object result = privateMethod.invoke(target);
        assertThat(result).isNotNull();
    }}
}}
```

Genera una classe di test completa che:
- NON usa @ExtendWith(MockitoExtension.class)
- NON usa @Mock annotations
- USA H2 database per test database invece di mockare Connection
- Testa tutti i metodi accessibili
- È compatibile con Java 21
- Include setup e teardown appropriati
- Usa reflection per metodi private se necessario

Formato di output: solo il codice Java della classe di test, senza spiegazioni aggiuntive.
"""

def generate_unit_tests(model, java_code, class_name):
    """Genera gli unit test usando l'AI"""
    try:
        print("🔍 Analisi metodi Java...")
        methods_info = analyze_java_methods(java_code)
        
        public_methods = [m for m in methods_info if m['visibility'] == 'public']
        private_methods = [m for m in methods_info if m['visibility'] == 'private']
        
        print(f"📊 Metodi trovati: {len(methods_info)}")
        print(f"   - Public: {len(public_methods)}")
        print(f"   - Private: {len(private_methods)}")
        
        if private_methods:
            print("⚠️  Metodi private rilevati - userò reflection per i test")
        
        print("⚠️  Configurazione per Java 21 - eviterò problemi con Mockito")
        print("🤖 Generazione unit test in corso...")
        prompt = create_prompt(java_code, class_name, methods_info)
        
        response = model.generate_content(prompt)
        return response.text
    
    except Exception as e:
        print(f"❌ Errore nella generazione: {e}")
        sys.exit(1)

def extract_class_name(java_code):
    """Estrae il nome della classe principale dal codice Java"""
    lines = java_code.split('\n')
    for line in lines:
        line = line.strip()
        if line.startswith('public class ') or line.startswith('class '):
            # Estrae il nome della classe
            parts = line.split()
            class_idx = parts.index('class') + 1
            if class_idx < len(parts):
                class_name = parts[class_idx].split('{')[0].split('<')[0]
                return class_name
    return "UnknownClass"

def save_test_file(test_code, output_path):
    """Salva il file di test"""
    try:
        # Crea la directory se non esiste
        output_dir = os.path.dirname(output_path)
        if output_dir:
            os.makedirs(output_dir, exist_ok=True)
        
        with open(output_path, 'w', encoding='utf-8') as file:
            file.write(test_code)
        
        print(f"✅ Unit test salvati in: {output_path}")
    
    except Exception as e:
        print(f"❌ Errore nel salvataggio: {e}")
        sys.exit(1)

def clean_generated_code(generated_text):
    """Pulisce il codice generato rimuovendo markdown e testo extra"""
    lines = generated_text.split('\n')
    java_lines = []
    in_code_block = False
    
    for line in lines:
        if line.strip().startswith('```java'):
            in_code_block = True
            continue
        elif line.strip().startswith('```') and in_code_block:
            in_code_block = False
            continue
        elif in_code_block or (line.strip().startswith('import ') or 
                              line.strip().startswith('package ') or 
                              'class' in line or 
                              line.strip().startswith('@') or
                              line.strip().startswith('public ') or
                              line.strip().startswith('private ') or
                              line.strip().startswith('protected ') or
                              line.strip().startswith('}') or
                              line.strip().startswith('{')):
            java_lines.append(line)
    
    # Se non abbiamo trovato blocchi di codice, prendiamo tutto
    if not java_lines:
        return generated_text
    
    return '\n'.join(java_lines)

def main():
    parser = argparse.ArgumentParser(
        description='Genera unit test per file Java usando Google Generative AI (Java 21 compatible)',
        formatter_class=argparse.RawDescriptionHelpFormatter,
        epilog="""
Esempi di utilizzo:
  python java_test_generator.py MyClass.java
  python java_test_generator.py src/main/java/MyClass.java -o test/
  python java_test_generator.py MyClass.java --output MyClassTest.java
  python java_test_generator.py MyClass.java --class-name CustomClass

Nota: Questo script genera test compatibili con Java 21 evitando problemi con Mockito
        """
    )
    
    parser.add_argument('--java', type=str, default="output/File_Java.sql",
                        help='Path del file java'
    )
    
    parser.add_argument(
        '-o', '--output',
        help='File o directory di output (default: <ClassName>Test.java nella directory corrente)'
    )
    
    parser.add_argument(
        '--class-name',
        help='Nome della classe (se non specificato, viene estratto automaticamente)'
    )
    
    parser.add_argument(
        '--verbose', '-v',
        action='store_true',
        help='Output verboso'
    )
    
    args = parser.parse_args()
    
    # Verifica che il file di input esista
    if not os.path.isfile(args.java):
        print(f"❌ Errore: File '{args.java}' non trovato")
        sys.exit(1)
    
    print(f"📁 Lettura file: {args.java}")
    
    # Leggi il file Java
    java_code = read_java_file(args.java)
    
    if args.verbose:
        print(f"📊 Dimensione file: {len(java_code)} caratteri")
    
    # Estrai il nome della classe
    class_name = args.class_name or extract_class_name(java_code)
    print(f"🏷️  Classe rilevata: {class_name}")
    
    # Determina il file di output
    if args.output:
        if os.path.isdir(args.output):
            output_path = os.path.join(args.output, f"{class_name}Test.java")
        else:
            output_path = args.output
    else:
        output_path = f"{class_name}Test.java"
    
    # Configura l'AI
    print("🔧 Configurazione Google Generative AI...")
    model = setup_genai()
    
    # Genera i test
    test_code = generate_unit_tests(model, java_code, class_name)
    
    # Pulisci il codice generato
    clean_test_code = clean_generated_code(test_code)
    
    if args.verbose:
        print("📝 Anteprima test generati:")
        print("-" * 50)
        print(clean_test_code[:500] + "..." if len(clean_test_code) > 500 else clean_test_code)
        print("-" * 50)
    
    # Salva il file
    save_test_file(clean_test_code, output_path)
    
    print("\n🎉 Generazione completata!")
    print(f"📋 Classe testata: {class_name}")
    print(f"📄 File di test: {output_path}")
    print(f"📏 Righe generate: {len(clean_test_code.split())}")
    
    # Suggerimenti post-generazione
    print("\n💡 Suggerimenti per Java 21:")
    print("   - I test evitano problemi con Mockito/Byte Buddy")
    print("   - Usa H2 database in-memory invece di mock per Connection")
    print("   - Aggiungi H2 dependency al pom.xml se necessario")
    print("   - I test usano reflection per metodi private")

if __name__ == "__main__":
    main()