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

def run_command(command, description=""):
    """Esegue un comando di sistema e gestisce l'output."""
    try:
        # Usiamo sys.executable per essere sicuri di usare lo stesso interprete Python
        process = subprocess.run(
            [sys.executable] + command,
            check=True,
            capture_output=True,
            text=True,
            encoding='utf-8'
        )
        
        # Mostra l'output del comando sottostante
        if process.stdout:
            for line in process.stdout.strip().split('\n'):
                print(f"  {line}")
                
        if process.stderr:
            print("  ⚠ Warning:")
            for line in process.stderr.strip().split('\n'):
                print(f"    {line}")
                
        return True
    except subprocess.CalledProcessError as e:
        print(f"  ✗ Errore nell'esecuzione")
        if e.stdout:
            for line in e.stdout.strip().split('\n'):
                print(f"    {line}")
        if e.stderr:
            for line in e.stderr.strip().split('\n'):
                print(f"    {line}")
        return False
    except Exception as e:
        print(f"  ✗ Errore critico: {e}")
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
    print("\n═══════════════════════════════════════════════════════════════")
    print("           COBOL to Java Converter - Processo Completo")
    print("═══════════════════════════════════════════════════════════════\n")
    
    base_dir = Path(__file__).parent.resolve()
    input_dir = base_dir / "input"
    output_dir = base_dir / "output"
    output_dir.mkdir(exist_ok=True)

    print("[PREPARAZIONE] Verifica file di input")
    print(f"  ↳ Ricerca in: {input_dir}")
    
    try:
        cobol_file, sql_file = find_input_files(input_dir)
        print(f"    ✓ File COBOL: {cobol_file.name}")
        print(f"    ✓ File SQL: {sql_file.name}")
    except (FileNotFoundError, FileExistsError) as e:
        print(f"    ✗ {e}")
        sys.exit(1)
        
    project_name = args.project_name or cobol_file.stem
    java_output_file = output_dir / f"{project_name}.java"
    final_project_dir = base_dir / project_name
    
    print(f"  ↳ Nome progetto: {project_name}")
    print()

    # --- FASE 1: TRADUZIONE ---
    print("[FASE 1/3] Traduzione COBOL → Java")
    translator_command = [
        "Translator_GenAI.py",
        "--cobol", str(cobol_file),
        "--sql", str(sql_file),
        "--output", str(java_output_file)
    ]
    
    if not run_command(translator_command):
        print("✗ Fase 1 fallita\n")
        sys.exit(1)
    
    if not java_output_file.exists():
        print(f"  ✗ File Java non creato: {java_output_file}")
        sys.exit(1)
        
    print("✓ Fase 1 completata\n")

    # --- FASE 2: CREAZIONE JAR ---
    print("[FASE 2/3] Creazione progetto Maven e JAR")
    jar_creator_command = [
        "java_to_jar.py", 
        str(java_output_file), 
        "--project-name", 
        project_name
    ]
    
    if not run_command(jar_creator_command):
        print("✗ Fase 2 fallita\n")
        sys.exit(1)
        
    print("✓ Fase 2 completata\n")

    # --- FASE 3: ARCHIVIAZIONE ---
    print("[FASE 3/3] Archiviazione file originali")
    if final_project_dir.exists() and final_project_dir.is_dir():
        final_input_storage = final_project_dir / "input"
        final_input_storage.mkdir(exist_ok=True)
        
        print(f"  ↳ Copia file in: {final_input_storage.name}/")
        shutil.copy2(cobol_file, final_input_storage)
        shutil.copy2(sql_file, final_input_storage)
        print(f"    ✓ {cobol_file.name}")
        print(f"    ✓ {sql_file.name}")
    else:
        print(f"  ⚠ Directory progetto non trovata: {final_project_dir}")
    
    print("✓ Fase 3 completata\n")

    # --- RIEPILOGO FINALE ---
    jar_with_deps_name = f"{project_name}-1.0.0-jar-with-dependencies.jar"
    
    print("═══════════════════════════════════════════════════════════════")
    print("                    CONVERSIONE COMPLETATA")
    print("═══════════════════════════════════════════════════════════════")
    print()
    print(f"📁 Progetto creato in: {project_name}/")
    print(f"📦 JAR eseguibile: {jar_with_deps_name}")
    print()
    print("Per eseguire il programma:")
    print(f"  $ cd {project_name}")
    print(f"  $ java -jar {jar_with_deps_name}")
    print()
    print("⚠ Assicurati che PostgreSQL sia in esecuzione e configurato")
    print("  con le credenziali specificate nel codice Java generato.")
    print()

if __name__ == "__main__":
    main()