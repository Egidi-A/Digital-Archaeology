#!/usr/bin/env python3

import os
import sys
import shutil
import subprocess
import argparse
import re
from pathlib import Path
import google.generativeai as genai

def setup_project_structure(project_name):
    """Crea la struttura standard Maven del progetto"""
    base_dir = Path(project_name)
    
    # Crea le directory necessarie
    dirs = [
        base_dir / "src" / "main" / "java" / "com",
        base_dir / "src" / "main" / "resources",
        base_dir / "src" / "test" / "java" / "com",
        base_dir / "target"
    ]
    
    for dir_path in dirs:
        dir_path.mkdir(parents=True, exist_ok=True)
    
    return base_dir

def get_class_name(java_file):
    """Estrae il nome della classe principale dal file Java"""
    with open(java_file, 'r') as f:
        content = f.read()
        
    # Cerca la classe pubblica
    public_class = re.search(r'public\s+class\s+(\w+)', content)
    if public_class:
        return public_class.group(1)
    
    # Se non c'è una classe pubblica, cerca qualsiasi classe
    any_class = re.search(r'class\s+(\w+)', content)
    if any_class:
        return any_class.group(1)
    
    # Usa il nome del file come fallback
    return Path(java_file).stem

def add_package_declaration(java_file, package_name="com"):
    """Aggiunge la dichiarazione del package come prima linea"""
    with open(java_file, 'r') as f:
        content = f.read()
    
    # Verifica se c'è già una dichiarazione package
    if re.match(r'^\s*package\s+', content):
        # Sostituisci il package esistente
        content = re.sub(r'^\s*package\s+[^;]+;', f'package {package_name};', content)
    else:
        # Aggiungi il package all'inizio
        content = f'package {package_name};\n\n' + content
    
    return content

def analyze_java_with_gemini(java_content, api_key):
    """Usa Gemini per analizzare il codice Java e generare il pom.xml"""
    genai.configure(api_key=api_key)
    
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

    prompt = f"""
    Analizza il seguente codice Java e genera un file pom.xml completo per Maven.
    
    IMPORTANTE: Esamina ATTENTAMENTE tutti gli import e il codice per identificare TUTTE le dipendenze necessarie.
    In particolare:
    - Se vedi import java.sql.* e stringhe JDBC (jdbc:postgresql, jdbc:mysql, etc.), aggiungi il driver appropriato
    - Se vedi import javax.servlet.*, aggiungi servlet-api
    - Se vedi import org.json.*, aggiungi json library
    - Per OGNI import non standard di Java, identifica e aggiungi la dipendenza Maven corrispondente
    
    Il pom.xml deve:
    1. Includere una sezione <dependencies> con TUTTE le dipendenze necessarie basandoti sugli import e l'analisi del codice
    2. Per driver JDBC: postgresql (42.7.1), mysql (8.0.33), h2 (2.2.224), sqlite (3.43.2.2)
    3. Usare groupId: com
    4. Specificare la versione Java appropriata analizzando il codice (es. se usa var, lambda, stream = Java 8+)
    5. Includere il plugin maven-jar per creare un JAR eseguibile
    6. Includere il plugin maven-assembly per creare un JAR con tutte le dipendenze
    7. Restituire SOLO il contenuto XML del pom.xml, senza spiegazioni o markdown
    
    Codice Java:
    {java_content}
    """
    
    response = model.generate_content(prompt)
    
    # Pulisci la risposta rimuovendo eventuali marcatori di codice
    pom_content = response.text.strip()
    pom_content = re.sub(r'```xml\s*', '', pom_content)
    pom_content = re.sub(r'```\s*$', '', pom_content)
    
    return pom_content

def create_pom_file(project_dir, java_file, class_name, api_key):
    """Crea il file pom.xml usando Gemini"""
    with open(java_file, 'r') as f:
        java_content = f.read()
    
    # Genera il pom.xml con Gemini
    pom_content = analyze_java_with_gemini(java_content, api_key)
    
    # Se Gemini fallisce o restituisce qualcosa di strano, usa un template di base
    if not pom_content or not pom_content.startswith('<?xml'):
        print("⚠️  Usando template pom.xml di base (Gemini potrebbe aver avuto problemi)")
        pom_content = f"""<?xml version="1.0" encoding="UTF-8"?>
<project xmlns="http://maven.apache.org/POM/4.0.0"
         xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance"
         xsi:schemaLocation="http://maven.apache.org/POM/4.0.0
         http://maven.apache.org/xsd/maven-4.0.0.xsd">
    
    <modelVersion>4.0.0</modelVersion>
    
    <groupId>com</groupId>
    <artifactId>{project_dir.name}</artifactId>
    <version>1.0.0</version>
    <packaging>jar</packaging>
    
    <properties>
        <maven.compiler.source>11</maven.compiler.source>
        <maven.compiler.target>11</maven.compiler.target>
        <project.build.sourceEncoding>UTF-8</project.build.sourceEncoding>
    </properties>
    
    <build>
        <plugins>
            <plugin>
                <groupId>org.apache.maven.plugins</groupId>
                <artifactId>maven-jar-plugin</artifactId>
                <version>3.3.0</version>
                <configuration>
                    <archive>
                        <manifest>
                            <mainClass>com.{class_name}</mainClass>
                        </manifest>
                    </archive>
                </configuration>
            </plugin>
            <plugin>
                <groupId>org.apache.maven.plugins</groupId>
                <artifactId>maven-assembly-plugin</artifactId>
                <version>3.4.2</version>
                <configuration>
                    <archive>
                        <manifest>
                            <mainClass>com.{class_name}</mainClass>
                        </manifest>
                    </archive>
                    <descriptorRefs>
                        <descriptorRef>jar-with-dependencies</descriptorRef>
                    </descriptorRefs>
                </configuration>
                <executions>
                    <execution>
                        <id>make-assembly</id>
                        <phase>package</phase>
                        <goals>
                            <goal>single</goal>
                        </goals>
                    </execution>
                </executions>
            </plugin>
        </plugins>
    </build>
</project>"""
    
    # Scrivi il pom.xml
    pom_path = project_dir / "pom.xml"
    with open(pom_path, 'w') as f:
        f.write(pom_content)
    
    return pom_path

def build_jar(project_dir):
    """Esegue Maven per creare il JAR"""
    os.chdir(project_dir)
    
    # Verifica se Maven è installato
    try:
        subprocess.run(["mvn", "--version"], check=True, capture_output=True)
    except (subprocess.CalledProcessError, FileNotFoundError):
        print("❌ Maven non è installato! Installa Maven prima di eseguire questo script.")
        return False
    
    # Compila e crea il JAR
    print("📦 Compilazione e creazione del JAR...")
    result = subprocess.run(["mvn", "clean", "package"], capture_output=True, text=True)
    
    if result.returncode == 0:
        print("✅ JAR creato con successo!")
        return True
    else:
        print("❌ Errore durante la compilazione:")
        print(result.stderr)
        return False

def main():
    parser = argparse.ArgumentParser(description='Automatizza la conversione di un file Java in JAR usando Maven')
    parser.add_argument('java_file', help='Path del file .java da convertire')
    parser.add_argument('--project-name', help='Nome del progetto (default: nome del file)', default=None)
    parser.add_argument('--gemini-api-key', help='API key di Gemini', required=True)
    
    args = parser.parse_args()
    
    # Verifica che il file esista
    java_file = Path(args.java_file)
    if not java_file.exists() or not java_file.suffix == '.java':
        print(f"❌ File non trovato o non è un file .java: {java_file}")
        sys.exit(1)
    
    # Determina il nome del progetto e della classe
    project_name = args.project_name or java_file.stem
    class_name = get_class_name(java_file)
    
    print(f"🚀 Avvio conversione di {java_file.name} in JAR...")
    print(f"📁 Nome progetto: {project_name}")
    print(f"📝 Classe principale: {class_name}")
    
    # 1. Crea la struttura del progetto
    print("\n1️⃣ Creazione struttura del progetto...")
    project_dir = setup_project_structure(project_name)
    
    # 2. Aggiungi il package declaration
    print("2️⃣ Aggiunta package declaration...")
    modified_content = add_package_declaration(java_file, "com")
    
    # 3. Copia il file nella posizione corretta
    print("3️⃣ Spostamento file Java...")
    target_java_file = project_dir / "src" / "main" / "java" / "com" / f"{class_name}.java"
    with open(target_java_file, 'w') as f:
        f.write(modified_content)
    
    # 4. Crea il pom.xml usando Gemini
    print("4️⃣ Generazione pom.xml con Gemini...")
    try:
        pom_path = create_pom_file(project_dir, target_java_file, class_name, args.gemini_api_key)
        print(f"   ✅ pom.xml creato: {pom_path}")
    except Exception as e:
        print(f"   ❌ Errore nella generazione del pom.xml: {e}")
        sys.exit(1)
    
    # 5. Compila e crea il JAR
    print("\n5️⃣ Compilazione e creazione JAR con Maven...")
    if build_jar(project_dir):
        print(f"\n✨ Completato! I JAR sono stati creati in:")
        print(f"   - {project_dir}/target/{project_name}-1.0.0.jar")
        print(f"   - {project_dir}/target/{project_name}-1.0.0-jar-with-dependencies.jar")
        print(f"\n🏃 Per eseguire: java -jar {project_dir}/target/{project_name}-1.0.0-jar-with-dependencies.jar")
    else:
        print("\n❌ Compilazione fallita. Controlla gli errori sopra.")
        sys.exit(1)

if __name__ == "__main__":
    main()