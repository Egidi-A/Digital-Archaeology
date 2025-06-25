#!/usr/bin/env python3

import os
import sys
import shutil
import subprocess
import argparse
import re
from pathlib import Path
import google.generativeai as genai

API_KEY = "YOUR_API_KEY"
genai.configure(api_key=API_KEY)

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

def analyze_java_with_gemini(java_content):
    """Usa Gemini per analizzare il codice Java e generare il pom.xml"""
    generation_config = {
        "temperature": 0.1,
        "top_p": 0.9,
        "top_k": 30,
        "max_output_tokens": 25000,
        "response_mime_type": "text/plain",
    }
    
    model = genai.GenerativeModel(
        model_name="gemini-2.0-flash-exp",
        generation_config=generation_config,
    )

    prompt = f"""
    Analizza il seguente codice Java e genera un file pom.xml completo per Maven.
    
    Il pom.xml DEVE:
    1. Analizzare TUTTI gli import nel codice Java e includere SOLO le dipendenze Maven effettivamente necessarie
    2. Usare groupId: com
    3. Usare version: 1.0.0 (NON usare SNAPSHOT, voglio una versione release)
    4. Specificare maven.compiler.source e target appropriati (11 se non diversamente indicato)
    5. Includere maven-jar-plugin con la classe main corretta e outputDirectory impostato su ${{project.basedir}}
    6. Includere maven-assembly-plugin per creare JAR con dipendenze e outputDirectory impostato su ${{project.basedir}}
    7. Per le dipendenze:
       - Includi SOLO librerie esterne effettivamente usate nel codice
       - NON includere dipendenze per classi standard Java (java.*, javax.* che sono parte del JDK)
       - Se vedi jdbc:postgresql -> aggiungi org.postgresql:postgresql (ultima versione stabile)
       - Se vedi jdbc:mysql -> aggiungi mysql driver
       - Se vedi import di librerie esterne (non JDK) -> aggiungi la dipendenza appropriata
    
    IMPORTANTE: 
    - Non aggiungere dipendenze "preventive" o "utili in generale"
    - Aggiungi SOLO le dipendenze per le librerie effettivamente importate e usate nel codice
    - Per PostgreSQL usa versione 42.7.1 o superiore
    
    Restituisci SOLO l'XML del pom.xml, senza spiegazioni o commenti.
    
    Codice Java:
    {java_content}
    """
    
    response = model.generate_content(prompt)
    
    # Pulisci la risposta
    pom_content = response.text.strip()
    pom_content = re.sub(r'```xml\s*', '', pom_content)
    pom_content = re.sub(r'```\s*$', '', pom_content)
    
    return pom_content

def ensure_output_directory(pom_content):
    """Assicura che outputDirectory sia configurato per mettere i JAR nella root del progetto"""
    # Pattern per trovare i plugin maven-jar e maven-assembly
    jar_plugin_pattern = r'(<plugin>\s*<groupId>org\.apache\.maven\.plugins</groupId>\s*<artifactId>maven-jar-plugin</artifactId>.*?<configuration>)(.*?)(</configuration>.*?</plugin>)'
    assembly_plugin_pattern = r'(<plugin>\s*<groupId>org\.apache\.maven\.plugins</groupId>\s*<artifactId>maven-assembly-plugin</artifactId>.*?<configuration>)(.*?)(</configuration>.*?</plugin>)'
    
    # Aggiungi outputDirectory al maven-jar-plugin se non presente
    if '<outputDirectory>' not in pom_content:
        def add_output_dir(match):
            config_start = match.group(1)
            config_content = match.group(2)
            config_end = match.group(3)
            if '<outputDirectory>' not in config_content:
                config_content = '\n                    <outputDirectory>${project.basedir}</outputDirectory>' + config_content
            return config_start + config_content + config_end
        
        pom_content = re.sub(jar_plugin_pattern, add_output_dir, pom_content, flags=re.DOTALL)
        pom_content = re.sub(assembly_plugin_pattern, add_output_dir, pom_content, flags=re.DOTALL)
    
    return pom_content

def create_pom_file(project_dir, java_file, class_name):
    """Crea il file pom.xml usando Gemini"""
    with open(java_file, 'r') as f:
        java_content = f.read()
    
    print(f"    📤 Invio richiesta a Gemini API...")
    
    # Genera il pom.xml con Gemini
    pom_content = analyze_java_with_gemini(java_content)
    
    # Se Gemini fallisce o restituisce qualcosa di strano, usa un template di base
    if not pom_content or not pom_content.startswith('<?xml'):
        print("    ⚠️  Usando template pom.xml di base")
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
                    <outputDirectory>${{project.basedir}}</outputDirectory>
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
                    <outputDirectory>${{project.basedir}}</outputDirectory>
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
    else:
        print(f"    ✓ Risposta ricevuta da Gemini")
    
    # Assicura che outputDirectory sia configurato correttamente
    pom_content = ensure_output_directory(pom_content)
    
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
        print("    ❌ Maven non è installato")
        return False
    
    # Compila e crea il JAR
    print(f" 🔨 Compilazione e creazione JAR...")
    result = subprocess.run(["mvn", "clean", "package"], capture_output=True, text=True)
    
    if result.returncode == 0:
        print(f"    ✓ JAR creato con successo")
        # Filtra l'output Maven per mostrare solo info essenziali
        if result.stdout:
            for line in result.stdout.split('\n'):
                if '[INFO]' in line and any(x in line for x in ['BUILD', 'Building', 'SUCCESS']):
                    msg = line.split('] ', 1)[-1] if '] ' in line else line
                    print(f"    ℹ️  {msg}")
        return True
    else:
        print(f"    ❌ Errore durante la compilazione")
        # Mostra errori principali
        if result.stderr:
            for line in result.stderr.split('\n'):
                if '[ERROR]' in line:
                    msg = line.split('] ', 1)[-1] if '] ' in line else line
                    print(f"      {msg}")
        if result.stdout:
            # Mostra anche gli errori dal stdout Maven
            error_section = False
            for line in result.stdout.split('\n'):
                if '[ERROR]' in line or error_section:
                    error_section = True
                    if line.strip():
                        print(f"      {line}")
        return False

def stampa_albero_directory(root_dir, prefix=""):
    """Stampa una rappresentazione ad albero della directory specificata."""
    from pathlib import Path
    
    path = Path(root_dir)
    if not path.is_dir():
        print(f"    ❌ {root_dir} non è una directory valida")
        return

    # Ottieni la lista di file/cartelle e ordinali
    items = sorted(list(path.iterdir()))
    
    for i, item in enumerate(items):
        # Determina il connettore da usare per l'albero
        if i == len(items) - 1:
            connector = "└── "
            new_prefix = prefix + "    "
        else:
            connector = "├── "
            new_prefix = prefix + "│   "
            
        print(f"    {prefix}{connector}{item.name}")
        
        # Se è una directory, chiama la funzione ricorsivamente
        if item.is_dir():
            stampa_albero_directory(item, prefix=new_prefix)

def main():
    parser = argparse.ArgumentParser(description='Automatizza la conversione di un file Java in JAR usando Maven')
    parser.add_argument('java_file', help='Path del file .java da convertire')
    parser.add_argument('--project-name', help='Nome del progetto (default: nome del file)', default=None)
    
    args = parser.parse_args()
    
    # Verifica che il file esista
    java_file = Path(args.java_file)
    if not java_file.exists() or not java_file.suffix == '.java':
        print(f"❌ File non trovato o non è un file .java: {java_file}")
        sys.exit(1)
    
    # Determina il nome del progetto e della classe
    project_name = args.project_name or java_file.stem
    class_name = get_class_name(java_file)
    
    print(f"    📄 File sorgente: {java_file.name}")
    print(f"    ✓ Nome progetto: {project_name}")
    print(f"    ✓ Classe principale: {class_name}")
    
    # 1. Crea la struttura del progetto
    print(f" 🏗️  Creazione struttura Maven...")
    project_dir = setup_project_structure(project_name)
    print(f"    ✓ Directory create: {project_name}/")
    
    # 2. Aggiungi il package declaration
    print(f" 📝 Preparazione file Java...")
    modified_content = add_package_declaration(java_file, "com")
    
    # 3. Copia il file nella posizione corretta
    target_java_file = project_dir / "src" / "main" / "java" / "com" / f"{class_name}.java"
    with open(target_java_file, 'w') as f:
        f.write(modified_content)
    print(f"    ✓ Package declaration aggiunto")
    print(f"    ✓ File Java copiato")
    
    # 4. Crea il pom.xml usando Gemini
    print(f" 📄 Generazione pom.xml...")
    try:
        pom_path = create_pom_file(project_dir, target_java_file, class_name)
        print(f"    ✓ pom.xml creato")
    except Exception as e:
        print(f"    ❌ Errore nella generazione del pom.xml: {e}")
        sys.exit(1)
    
    # 5. Compila e crea il JAR
    if build_jar(project_dir):
        # Cerca i JAR generati nella directory del progetto
        os.chdir('..')  # Torna alla directory originale
        jar_files = list(project_dir.glob("*.jar"))
        
        if jar_files:
            print(f" ✅ Processo completato")
            
            # Trova il JAR con dipendenze
            jar_with_deps = [j for j in jar_files if "with-dependencies" in j.name]
            if jar_with_deps:
                print(f"    ✓ JAR creato: {jar_with_deps[0].name}")
            else:
                print(f"    ✓ JAR creato: {jar_files[0].name}")
        else:
            print(f"  ⚠️ I JAR potrebbero essere in target/")
    else:
        print(f" ❌ Compilazione fallita")
        sys.exit(1)
    
    print()  # Riga vuota finale per separazione

if __name__ == "__main__":
    main()