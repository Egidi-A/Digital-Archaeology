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

def detect_dependencies(java_content):
    """Rileva automaticamente le dipendenze dal codice Java"""
    dependencies = []
    
    # Controllo per PostgreSQL
    if 'jdbc:postgresql' in java_content or 'postgresql' in java_content.lower():
        dependencies.append({
            'groupId': 'org.postgresql',
            'artifactId': 'postgresql',
            'version': '42.7.1'
        })
    
    # Controllo per MySQL
    if 'jdbc:mysql' in java_content or 'com.mysql' in java_content:
        dependencies.append({
            'groupId': 'com.mysql.cj',
            'artifactId': 'mysql-connector-java',
            'version': '8.0.33'
        })
    
    # Controllo per H2
    if 'jdbc:h2' in java_content:
        dependencies.append({
            'groupId': 'com.h2database',
            'artifactId': 'h2',
            'version': '2.2.224'
        })
    
    # Controllo per SQLite
    if 'jdbc:sqlite' in java_content:
        dependencies.append({
            'groupId': 'org.xerial',
            'artifactId': 'sqlite-jdbc',
            'version': '3.43.2.2'
        })
    
    # Controllo per Servlet API
    if 'javax.servlet' in java_content or 'jakarta.servlet' in java_content:
        dependencies.append({
            'groupId': 'jakarta.servlet',
            'artifactId': 'jakarta.servlet-api',
            'version': '6.0.0',
            'scope': 'provided'
        })
    
    # Controllo per JSON
    if 'org.json' in java_content:
        dependencies.append({
            'groupId': 'org.json',
            'artifactId': 'json',
            'version': '20231013'
        })
    
    # Controllo per Logging
    if 'org.slf4j' in java_content:
        dependencies.append({
            'groupId': 'org.slf4j',
            'artifactId': 'slf4j-api',
            'version': '2.0.9'
        })
    
    # Controllo per JUnit
    if 'org.junit' in java_content or 'import junit' in java_content:
        dependencies.append({
            'groupId': 'junit',
            'artifactId': 'junit',
            'version': '4.13.2',
            'scope': 'test'
        })
    
    return dependencies

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
    
    # Rileva automaticamente le dipendenze
    detected_deps = detect_dependencies(java_content)
    deps_list = "\n".join([f"- {d['groupId']}:{d['artifactId']}:{d['version']}" for d in detected_deps])

    prompt = f"""
    Genera un file pom.xml completo per Maven per il seguente codice Java.
    
    DIPENDENZE RILEVATE CHE DEVONO ESSERE INCLUSE:
    {deps_list}
    
    Il pom.xml DEVE:
    1. Includere TUTTE le dipendenze elencate sopra nella sezione <dependencies>
    2. Usare groupId: com
    3. Usare version: 1.0.0 (NON usare SNAPSHOT, voglio una versione release)
    4. Specificare maven.compiler.source e target appropriati (11 se non diversamente indicato)
    5. Includere maven-jar-plugin con la classe main corretta e outputDirectory impostato su ${{project.basedir}}
    6. Includere maven-assembly-plugin per creare JAR con dipendenze e outputDirectory impostato su ${{project.basedir}}
    7. NON dimenticare la sezione <dependencies> con le dipendenze rilevate!
    
    Restituisci SOLO l'XML del pom.xml, senza spiegazioni.
    
    Codice Java:
    {java_content}
    """
    
    response = model.generate_content(prompt)
    
    # Pulisci la risposta
    pom_content = response.text.strip()
    pom_content = re.sub(r'```xml\s*', '', pom_content)
    pom_content = re.sub(r'```\s*$', '', pom_content)
    
    # Verifica che ci sia la sezione dependencies, altrimenti aggiungila forzatamente
    if detected_deps and '<dependencies>' not in pom_content:
        print("⚠️  Gemini non ha incluso le dipendenze. Le aggiungo manualmente...")
        pom_content = inject_dependencies(pom_content, detected_deps)
    
    return pom_content

def inject_dependencies(pom_content, dependencies):
    """Inietta forzatamente le dipendenze nel pom.xml se mancano"""
    deps_xml = "    <dependencies>\n"
    for dep in dependencies:
        deps_xml += "        <dependency>\n"
        deps_xml += f"            <groupId>{dep['groupId']}</groupId>\n"
        deps_xml += f"            <artifactId>{dep['artifactId']}</artifactId>\n"
        deps_xml += f"            <version>{dep['version']}</version>\n"
        if 'scope' in dep:
            deps_xml += f"            <scope>{dep['scope']}</scope>\n"
        deps_xml += "        </dependency>\n"
    deps_xml += "    </dependencies>\n\n"
    
    # Inserisci le dipendenze prima di <build>
    if '<build>' in pom_content:
        pom_content = pom_content.replace('<build>', deps_xml + '    <build>')
    else:
        # Inserisci prima di </project>
        pom_content = pom_content.replace('</project>', deps_xml + '</project>')
    
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
    
    # Genera il pom.xml con Gemini
    pom_content = analyze_java_with_gemini(java_content)
    
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
    # parser.add_argument('--gemini-api-key', help='API key di Gemini', required=True)
    
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
        pom_path = create_pom_file(project_dir, target_java_file, class_name)
        print(f"   ✅ pom.xml creato: {pom_path}")
    except Exception as e:
        print(f"   ❌ Errore nella generazione del pom.xml: {e}")
        sys.exit(1)
    
    # 5. Compila e crea il JAR
    print("\n5️⃣ Compilazione e creazione JAR con Maven...")
    if build_jar(project_dir):
        # Cerca i JAR generati nella directory del progetto
        jar_files = list(project_dir.glob("*.jar"))
        
        if jar_files:
            print(f"\n✨ Completato! I JAR sono stati creati:")
            for jar in jar_files:
                print(f"   - {jar.name}")
            
            # Trova il JAR con dipendenze
            jar_with_deps = [j for j in jar_files if "with-dependencies" in j.name]
            if jar_with_deps:
                print(f"\n🏃 Per eseguire:")
                print(f"   cd {project_dir}")
                print(f"   java -jar {jar_with_deps[0].name}")
            else:
                print(f"\n🏃 Per eseguire:")
                print(f"   cd {project_dir}")
                print(f"   java -jar {jar_files[0].name}")
        else:
            print(f"\n⚠️  I JAR sono stati creati ma potrebbero essere in target/")
            print(f"   Controlla manualmente la directory del progetto")
    else:
        print("\n❌ Compilazione fallita. Controlla gli errori sopra.")
        sys.exit(1)

if __name__ == "__main__":
    main()