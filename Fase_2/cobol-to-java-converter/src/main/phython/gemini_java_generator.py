import google.generativeai as genai
import os
import sys

# Configura la tua API key
API_KEY = "YOUR_API_KEY"  # Sostituisci con la tua API key
genai.configure(api_key=API_KEY)

def read_xml_file(filepath):
    """Legge il contenuto del file XML"""
    try:
        with open(filepath, 'r', encoding='utf-8') as file:
            return file.read()
    except FileNotFoundError:
        print(f"Errore: File {filepath} non trovato")
        sys.exit(1)

def generate_java_code(xml_content):
    """Usa Gemini per generare codice Java dall'XML ASG"""
    
    # Configura il modello con parametri di generazione
    generation_config = {
        "temperature": 0.2,  # Bassa per codice più deterministico
        "top_p": 0.95,
        "top_k": 40,
        "max_output_tokens": 20000,
        "response_mime_type": "text/plain",
    }
    
    model = genai.GenerativeModel(
        model_name="gemini-2.5-pro-preview-06-05",  # Modello sperimentale più avanzato
        generation_config=generation_config,
    )
    
    # Crea il prompt
    prompt = f"""
Dato il seguente XML che rappresenta un Abstract Semantic Graph (ASG) di un programma Java,
genera il codice sorgente Java corrispondente. L'XML contiene:
- class: definizione della classe
- field: campi privati con getter/setter
- method: metodi con visibilità, parametri e body
- statement: vari tipi di statement nel body dei metodi

XML ASG:
{xml_content}

IMPORTANTE: Genera SOLO codice Java valido e compilabile.
- Includi JavaDoc completo per classe, campi e metodi.
- Aggiungi commenti inline dove appropriato per spiegare la logica.
- Usa convenzioni di naming Java (PascalCase per classi, camelCase per metodi/variabili).

- **Gestione Specifica degli SQL Statement**: L'ASG contiene tag come `<statement cobolType="execSqlStatement" type="unknown"/>`. Questi rappresentano operazioni di database. Quando ne incontri uno, devi simulare l'operazione in Java. La simulazione deve:
    1.  Includere un commento che indichi l'intento originale (es. `// ASG: execSqlStatement (SELECT su cliente)`).
    2.  Simulare l'esito dell'operazione impostando la variabile `sqlcode` (es. `sqlcode = 0;` per successo, `sqlcode = 100;` per record non trovato).
    3.  Se la query era una `SELECT` che doveva popolare delle variabili (es. `:WS-CLI-NOME`), assegna a queste variabili dei valori fittizi e plausibili (es. `wsCliNome = "Mario"; wsCliCognome = "Rossi";`).
    4.  Questa simulazione è cruciale perché l'ASG Java non contiene la query SQL esplicita.

- Qualsiasi spiegazione o nota NON di codice deve essere inserita come commento Java (// o /* */).
- NON includere testo fuori dal codice Java - tutto deve essere codice valido o commenti.

Il file deve essere immediatamente compilabile con javac senza modifiche.
"""
    
    try:
        # Genera la risposta
        response = model.generate_content(prompt)
        return response.text
    except Exception as e:
        print(f"Errore nella generazione: {e}")
        return None

def save_java_file(java_code, output_path):
    """Salva il codice Java generato"""
    try:
        # Split per linee e rimuovi quelle con solo backtick
        lines = java_code.split('\n')
        cleaned_lines = []
        in_code_block = False
        
        for line in lines:
            # Rileva inizio/fine blocco codice
            if line.strip() == '```java' or line.strip() == '```':
                in_code_block = not in_code_block
                continue
            
            # Se siamo in un blocco di codice o la linea non inizia con ###, mantienila
            if in_code_block or not line.strip().startswith('###'):
                cleaned_lines.append(line)
        
        # Ricostruisci il codice
        java_code = '\n'.join(cleaned_lines).strip()
        
        with open(output_path, 'w', encoding='utf-8') as file:
            file.write(java_code)
        print(f"Codice Java salvato in: {output_path}")
    except Exception as e:
        print(f"Errore nel salvataggio: {e}")

def main():
    # Path dei file
    input_xml = "output/ASG_Java.xml"  # o "ExampleASGJava.xml" se hai rinominato
    output_java = "output/GeneratedJava.java"
    
    print("Lettura XML ASG...")
    xml_content = read_xml_file(input_xml)
    
    print("Generazione codice Java con Gemini...")
    java_code = generate_java_code(xml_content)
    
    if java_code:
        print("\nCodice Java generato:")
        print("-" * 80)
        print(java_code)
        print("-" * 80)
        
        save_java_file(java_code, output_java)
    else:
        print("Errore nella generazione del codice")

if __name__ == "__main__":
    main()