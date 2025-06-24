#!/usr/bin/env python3

import os
import sys
import subprocess
import shutil
import argparse
from pathlib import Path

def find_input_files(input_dir):
    """Trova i file .cbl e .sql nella directory di input."""
    cobol_file, sql_file = None, None
    for file in input_dir.iterdir():
        if file.suffix.lower() in ['.cbl', '.cob']:
            if cobol_file:
                raise FileExistsError("Trovati più file COBOL. Lasciane solo uno nella cartella 'input'.")
            cobol_file = file
        elif file.suffix.lower() == '.sql':
            if sql_file:
                raise FileExistsError("Trovati più file SQL. Lasciane solo uno nella cartella 'input'.")
            sql_file = file
    
    if not cobol_file:
        raise FileNotFoundError("Nessun file COBOL (.cbl, .cob) trovato nella cartella 'input'.")
    if not sql_file:
        raise FileNotFoundError("Nessun file SQL (.sql) trovato nella cartella 'input'.")
        
    return cobol_file, sql_file

def run_command(command, step_name):
    """Esegue un comando di sistema e gestisce l'output."""
    print(f"--- Inizio: {step_name} ---")
    try:
        # Usiamo sys.executable per essere sicuri di usare lo stesso interprete Python
        process = subprocess.run(
            [sys.executable] + command,
            check=True,
            capture_output=True,
            text=True,
            encoding='utf-8'
        )
        print(process.stdout)
        if process.stderr:
            print("--- WARNING/STDERR ---")
            print(process.stderr)
        print(f"--- ✅ Successo: {step_name} completato ---")
        return True
    except subprocess.CalledProcessError as e:
        print(f"--- ❌ ERRORE durante {step_name} ---")
        print("--- STDOUT ---")
        print(e.stdout)
        print("--- STDERR ---")
        print(e.stderr)
        return False
    except Exception as e:
        print(f"--- ❌ ERRORE CATASTROFICO durante {step_name} ---")
        print(e)
        return False

def main():
    """
    Funzione principale per orchestrare la conversione da COBOL a JAR.
    """
    parser = argparse.ArgumentParser(
        description="Processo completo per convertire un progetto COBOL+SQL in un JAR eseguibile.",
        formatter_class=argparse.RawTextHelpFormatter
    )
    parser.add_argument(
        "--project-name",
        type=str,
        help="Nome del progetto Maven e del JAR finale. Se non specificato, verrà derivato dal nome del file COBOL."
    )
    args = parser.parse_args()

    # --- SETUP INIZIALE ---
    print("\n\n🚀 Inizio del processo di automazione - COBOL to Java Converter 🚀\n")
    base_dir = Path(__file__).parent.resolve()
    input_dir = base_dir / "input"
    output_dir = base_dir / "output"
    output_dir.mkdir(exist_ok=True) # Crea la cartella di output se non esiste

    try:
        cobol_file, sql_file = find_input_files(input_dir)
    except (FileNotFoundError, FileExistsError) as e:
        print(f"❌ Errore di pre-esecuzione: {e}")
        sys.exit(1)
        
    project_name = args.project_name or cobol_file.stem
    java_output_file = output_dir / f"{project_name}.java"
    final_project_dir = base_dir / project_name
    
    print(f"  ▶️  File COBOL: {cobol_file.name}")
    print(f"  ▶️  File SQL  : {sql_file.name}")
    print(f"  ▶️  Progetto  : {project_name}\n")

    # --- PASSO 1: TRADUZIONE ---
    translator_command = [
        "Translator_GenAI.py",
        "--cobol", str(cobol_file),
        "--sql", str(sql_file),
        "--output", str(java_output_file)
    ]
    if not run_command(translator_command, "[PASSO 1] Traduzione da COBOL a Java"):
        sys.exit(1)
    print("✅ [PASSO 1] Completato.\n")


    if not java_output_file.exists():
        print(f"❌ ERRORE: Il file Java '{java_output_file}' non è stato creato. Interruzione.")
        sys.exit(1)
        
    # --- PASSO 2: PACCHETTIZZAZIONE ---
    jar_creator_command = [ "java_to_jar.py", str(java_output_file), "--project-name", project_name ]
    if not run_command(jar_creator_command, "[PASSO 2] Creazione del Progetto Maven e del JAR"):
        sys.exit(1)
    print("✅ [PASSO 2] Completato.\n")


    # --- PASSO 3: ARCHIVIAZIONE ---
    print("[PASSO 3] Archiviazione degli input originali...")
    if final_project_dir.exists() and final_project_dir.is_dir():
        final_input_storage = final_project_dir / "input"
        final_input_storage.mkdir(exist_ok=True)
        
        shutil.copy2(cobol_file, final_input_storage)
        shutil.copy2(sql_file, final_input_storage)
        
        print(f"  ✔︎ File originali copiati in '{final_input_storage}'")
    else:
        print(f"  ⚠️  ATTENZIONE: La directory del progetto '{final_project_dir}' non è stata trovata.")
    print("✅ [PASSO 3] Completato.\n")

    # --- CONCLUSIONE ---
    jar_with_deps_name = f"{project_name}-1.0.0-jar-with-dependencies.jar"
    final_jar_path = final_project_dir / jar_with_deps_name
    
    print("\n✨ PROCESSO COMPLETATO ✨")
    print("\nIl progetto Maven è stato creato e il JAR eseguibile è pronto.")
    print(f"\n  📁 Directory Progetto: {final_project_dir.resolve()}")
    print(f"  📦 JAR Eseguibile:   {jar_with_deps_name}")
    print("\nPer eseguire il programma:")
    print(f"  cd {project_name}")
    print(f"  java -jar {jar_with_deps_name}")
    print("\nAssicurati che il database PostgreSQL sia in esecuzione e configurato.")


if __name__ == "__main__":
    main()