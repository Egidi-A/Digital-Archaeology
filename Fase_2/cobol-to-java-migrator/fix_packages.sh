#!/bin/bash

# Crea tutti i file __init__.py necessari per rendere i moduli importabili

echo "🔧 Creazione file __init__.py per i package Python..."

# File __init__.py per la directory src (root package)
cat > src/__init__.py << 'EOF'
"""
COBOL to Java Migration Tool
Root package
"""

__version__ = '1.0.0'
EOF

# File __init__.py per analyzer (già esistente, ma assicuriamoci che sia corretto)
cat > src/analyzer/__init__.py << 'EOF'
"""
COBOL to Java Migration Tool - Analyzer Module
Moduli per l'analisi dell'AST COBOL
"""

from .data_analyzer import DataAnalyzer
from .procedure_analyzer import ProcedureAnalyzer
from .sql_analyzer import SqlAnalyzer

__all__ = ['DataAnalyzer', 'ProcedureAnalyzer', 'SqlAnalyzer']
__version__ = '1.0.0'
EOF

# File __init__.py per generator (già esistente, ma assicuriamoci che sia corretto)
cat > src/generator/__init__.py << 'EOF'
"""
COBOL to Java Migration Tool - Generator Module
Moduli per la generazione del codice Java e configurazioni
"""

from .java_generator import JavaGenerator
from .gradle_generator import GradleGenerator
from .docker_generator import DockerGenerator

__all__ = ['JavaGenerator', 'GradleGenerator', 'DockerGenerator']
__version__ = '1.0.0'
EOF

# File __init__.py per parser (già esistente, ma assicuriamoci che sia corretto)
cat > src/parser/__init__.py << 'EOF'
"""
COBOL to Java Migration Tool - Parser Module
This module handles parsing of COBOL source files
"""

from .cobol_lexer import CobolLexer
from .cobol_parser import CobolParser
from .sql_schema_parser import SqlSchemaParser
from .ast_nodes import *

__all__ = ['CobolLexer', 'CobolParser', 'SqlSchemaParser']
__version__ = '1.0.0'
EOF

echo "✅ File __init__.py creati con successo!"

# Verifica la struttura
echo ""
echo "📁 Struttura package verificata:"
find src -name "__init__.py" -type f