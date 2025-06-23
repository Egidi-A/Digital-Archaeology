#!/usr/bin/env python3
"""
test_runner.py - Helper per eseguire test Java
"""

import os
import sys
import subprocess
import argparse

def run_maven_tests(test_class=None, verbose=False):
    """Esegui test con Maven"""
    cmd = ["mvn", "test"]
    if test_class:
        cmd.extend(["-Dtest=" + test_class])
    if verbose:
        cmd.append("-X")
    
    print(f"🚀 Eseguendo: {' '.join(cmd)}")
    result = subprocess.run(cmd, capture_output=True, text=True)
    
    print(result.stdout)
    if result.stderr:
        print("❌ Errori:", result.stderr)
    
    return result.returncode == 0

def run_gradle_tests(test_class=None, verbose=False):
    """Esegui test con Gradle"""
    cmd = ["./gradlew", "test"]
    if test_class:
        cmd.extend(["--tests", test_class])
    if verbose:
        cmd.append("--info")
    
    print(f"🚀 Eseguendo: {' '.join(cmd)}")
    result = subprocess.run(cmd, capture_output=True, text=True)
    
    print(result.stdout)
    if result.stderr:
        print("❌ Errori:", result.stderr)
    
    return result.returncode == 0

def detect_build_tool():
    """Rileva il build tool utilizzato"""
    if os.path.exists("pom.xml"):
        return "maven"
    elif os.path.exists("build.gradle") or os.path.exists("build.gradle.kts"):
        return "gradle"
    else:
        return None

def main():
    parser = argparse.ArgumentParser(description='Helper per eseguire test Java')
    parser.add_argument('--test-class', '-t', help='Classe di test specifica da eseguire')
    parser.add_argument('--verbose', '-v', action='store_true', help='Output verboso')
    parser.add_argument('--tool', choices=['maven', 'gradle'], help='Forza build tool specifico')
    
    args = parser.parse_args()
    
    build_tool = args.tool or detect_build_tool()
    
    if not build_tool:
        print("❌ Nessun build tool rilevato (pom.xml o build.gradle)")
        print("💡 Usa --tool per specificare manualmente")
        sys.exit(1)
    
    print(f"🔧 Build tool rilevato: {build_tool}")
    
    success = False
    if build_tool == "maven":
        success = run_maven_tests(args.test_class, args.verbose)
    elif build_tool == "gradle":
        success = run_gradle_tests(args.test_class, args.verbose)
    
    if success:
        print("✅ Test completati con successo!")
    else:
        print("❌ Alcuni test sono falliti")
        sys.exit(1)

if __name__ == "__main__":
    main()