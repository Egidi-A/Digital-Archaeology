#!/usr/bin/env python3
"""
COBOL to Java Migration Tool
Main entry point per il processo di migrazione
"""

import os
import sys
import yaml
import click
from pathlib import Path
from loguru import logger

# Aggiungi il path src al PYTHONPATH se necessario
current_dir = os.path.dirname(os.path.abspath(__file__))
src_dir = os.path.dirname(current_dir) if 'src' in current_dir else current_dir
if src_dir not in sys.path:
    sys.path.insert(0, src_dir)

# Import assoluti invece di relativi
try:
    from parser.cobol_parser import CobolParser
    from analyzer.data_analyzer import DataAnalyzer
    from analyzer.procedure_analyzer import ProcedureAnalyzer
    from analyzer.sql_analyzer import SqlAnalyzer
    from parser.sql_schema_parser import SqlSchemaParser
    from generator.java_generator import JavaGenerator
    from generator.gradle_generator import GradleGenerator
    from generator.docker_generator import DockerGenerator
except ImportError as e:
    # Fallback per import alternativi
    try:
        sys.path.insert(0, '/app/src')
        from parser.cobol_parser import CobolParser
        from analyzer.data_analyzer import DataAnalyzer
        from analyzer.procedure_analyzer import ProcedureAnalyzer
        from analyzer.sql_analyzer import SqlAnalyzer
        from parser.sql_schema_parser import SqlSchemaParser
        from generator.java_generator import JavaGenerator
        from generator.gradle_generator import GradleGenerator
        from generator.docker_generator import DockerGenerator
    except ImportError as e2:
        print(f"Errore import: {e}")
        print(f"Errore import fallback: {e2}")
        print(f"PYTHONPATH: {sys.path}")
        print(f"Current dir: {current_dir}")
        print(f"Src dir: {src_dir}")
        sys.exit(1)


class CobolToJavaMigrator:
    """Classe principale per la migrazione COBOL to Java"""
    
    def __init__(self, config_path="config/migration-config.yaml"):
        """Inizializza il migrator con la configurazione"""
        # Prova diversi percorsi per il file di configurazione
        config_paths = [
            config_path,
            f"/app/{config_path}",
            f"/app/config/migration-config.yaml",
            "migration-config.yaml"
        ]
        
        config_found = False
        for path in config_paths:
            if os.path.exists(path):
                self.config = self._load_config(path)
                config_found = True
                logger.info(f"Configurazione caricata da: {path}")
                break
        
        if not config_found:
            logger.warning("File di configurazione non trovato, uso configurazione di default")
            self.config = self._get_default_config()
        
        self.parser = CobolParser()
        self.parser.build()
        self.sql_schema_parser = SqlSchemaParser()
        
        # Inizializza gli analizzatori
        self.data_analyzer = DataAnalyzer(self.config)
        self.procedure_analyzer = ProcedureAnalyzer(self.config)
        self.sql_analyzer = SqlAnalyzer(self.config)
        
        # Inizializza i generatori
        self.java_generator = JavaGenerator(self.config)
        self.gradle_generator = GradleGenerator(self.config)
        self.docker_generator = DockerGenerator(self.config)
        
        # Configura logging
        self._setup_logging()
    
    def _load_config(self, config_path):
        """Carica la configurazione dal file YAML"""
        try:
            with open(config_path, 'r') as f:
                return yaml.safe_load(f)
        except Exception as e:
            logger.error(f"Errore nel caricamento della configurazione: {e}")
            return self._get_default_config()
    
    def _get_default_config(self):
        """Configurazione di default"""
        return {
            'migration': {
                'source_encoding': 'UTF-8',
                'target_java_version': '11'
            },
            'java': {
                'package_base': 'com.migrated.app',
                'use_lombok': True
            },
            'spring': {
                'version': '2.7.0'
            },
            'database': {
                'type': 'postgresql'
            },
            'docker': {
                'base_image': 'openjdk:11-jre-slim',
                'expose_port': 8080
            },
            'logging': {
                'level': 'INFO',
                'file': 'migration.log'
            }
        }
    
    def _setup_logging(self):
        """Configura il sistema di logging"""
        log_level = self.config.get('logging', {}).get('level', 'INFO')
        log_file = self.config.get('logging', {}).get('file', '/app/logs/migration.log')
        
        # Assicurati che la directory dei log esista
        log_dir = os.path.dirname(log_file)
        if log_dir and not os.path.exists(log_dir):
            os.makedirs(log_dir, exist_ok=True)
        
        # Rimuovi handler esistenti
        logger.remove()
        
        # Aggiungi console handler
        logger.add(
            sys.stderr,
            format="<green>{time:YYYY-MM-DD HH:mm:ss}</green> | <level>{level: <8}</level> | <cyan>{name}</cyan>:<cyan>{function}</cyan>:<cyan>{line}</cyan> - <level>{message}</level>",
            level=log_level
        )
        
        # Aggiungi file handler se possibile
        try:
            logger.add(
                log_file,
                rotation="10 MB",
                retention="7 days",
                level=log_level
            )
        except Exception as e:
            logger.warning(f"Impossibile creare file di log {log_file}: {e}")
    
    def migrate_file(self, cobol_file_path, sql_file_path, output_dir):
        """Migra un singolo file COBOL con schema SQL"""
        logger.info(f"Inizio migrazione del file: {cobol_file_path}")
        logger.info(f"Schema SQL: {sql_file_path}")
        
        try:
            # Verifica che i file esistano
            if not os.path.exists(cobol_file_path):
                logger.error(f"File COBOL non trovato: {cobol_file_path}")
                return False
            
            # 1. Leggi il file COBOL
            with open(cobol_file_path, 'r', encoding=self.config['migration']['source_encoding']) as f:
                cobol_code = f.read()
            
            logger.info(f"File COBOL letto: {len(cobol_code)} caratteri")
            
            # 2. Leggi e parsa lo schema SQL
            sql_schema = None
            if sql_file_path and os.path.exists(sql_file_path):
                logger.info("Parsing dello schema SQL...")
                with open(sql_file_path, 'r', encoding='utf-8') as f:
                    sql_content = f.read()
                sql_schema = self.sql_schema_parser.parse(sql_content)
            else:
                logger.warning(f"Schema SQL non trovato: {sql_file_path}")
            
            # 3. Parse del codice COBOL
            logger.info("Parsing del codice COBOL...")
            ast = self.parser.parse(cobol_code)
            
            if not ast:
                logger.error("Parsing fallito - AST vuoto")
                return False
            
            # 4. Analisi dell'AST con schema SQL
            logger.info("Analisi dell'AST...")
            analysis_result = self._analyze_ast(ast, sql_schema)
            
            # 5. Generazione codice Java
            logger.info("Generazione codice Java...")
            java_project = self.java_generator.generate(ast, analysis_result)
            
            # 6. Generazione struttura Gradle
            logger.info("Generazione struttura Gradle...")
            gradle_files = self.gradle_generator.generate(java_project)
            
            # 7. Generazione configurazione Docker
            logger.info("Generazione configurazione Docker...")
            docker_files = self.docker_generator.generate(java_project)
            
            # 8. Scrittura dei file
            logger.info(f"Scrittura dei file in: {output_dir}")
            self._write_output(output_dir, java_project, gradle_files, docker_files)
            
            logger.success(f"Migrazione completata con successo!")
            return True
            
        except Exception as e:
            logger.error(f"Errore durante la migrazione: {str(e)}")
            import traceback
            logger.debug(traceback.format_exc())
            raise
    
    def _analyze_ast(self, ast, sql_schema=None):
        """Esegue l'analisi completa dell'AST"""
        result = {
            'program_info': {},
            'data_structures': {},
            'procedures': {},
            'sql_statements': {},
            'file_operations': {},
            'sql_schema': sql_schema
        }
        
        # Passa lo schema SQL agli analizzatori
        if sql_schema:
            self.data_analyzer.set_sql_schema(sql_schema)
            self.sql_analyzer.set_sql_schema(sql_schema)
        
        # Analisi delle strutture dati
        data_analysis = self.data_analyzer.analyze(ast)
        result['data_structures'] = data_analysis
        
        # Analisi delle procedure
        procedure_analysis = self.procedure_analyzer.analyze(ast)
        result['procedures'] = procedure_analysis
        
        # Analisi SQL
        sql_analysis = self.sql_analyzer.analyze(ast)
        result['sql_statements'] = sql_analysis
        
        # Informazioni generali del programma
        result['program_info'] = {
            'program_id': ast.program_id if hasattr(ast, 'program_id') else 'Unknown',
            'author': ast.author if hasattr(ast, 'author') else 'Unknown',
            'date_written': ast.date_written if hasattr(ast, 'date_written') else 'Unknown'
        }
        
        return result
    
    def _write_output(self, output_dir, java_project, gradle_files, docker_files):
        """Scrive tutti i file generati nella directory di output"""
        output_path = Path(output_dir)
        
        # Assicurati che la directory di output esista
        output_path.mkdir(parents=True, exist_ok=True)
        
        # Crea la struttura delle directory
        project_name = java_project.get('project_name', 'migrated-app')
        project_root = output_path / project_name
        
        # Struttura directory Java
        src_main_java = project_root / 'src' / 'main' / 'java'
        src_main_resources = project_root / 'src' / 'main' / 'resources'
        src_test_java = project_root / 'src' / 'test' / 'java'
        
        # Crea tutte le directory
        for dir_path in [src_main_java, src_main_resources, src_test_java]:
            dir_path.mkdir(parents=True, exist_ok=True)
        
        # Scrivi i file Java
        package_path = java_project['package_base'].replace('.', '/')
        java_src_dir = src_main_java / package_path
        java_src_dir.mkdir(parents=True, exist_ok=True)
        
        for file_info in java_project['java_files']:
            file_path = java_src_dir / file_info['package'] / file_info['filename']
            file_path.parent.mkdir(parents=True, exist_ok=True)
            
            with open(file_path, 'w', encoding='utf-8') as f:
                f.write(file_info['content'])
            logger.info(f"Creato: {file_path}")
        
        # Scrivi i file di configurazione Spring
        for file_info in java_project.get('config_files', []):
            file_path = src_main_resources / file_info['filename']
            file_path.parent.mkdir(parents=True, exist_ok=True)
            
            with open(file_path, 'w', encoding='utf-8') as f:
                f.write(file_info['content'])
            logger.info(f"Creato: {file_path}")
        
        # Scrivi i file Gradle
        for file_info in gradle_files:
            file_path = project_root / file_info['filename']
            file_path.parent.mkdir(parents=True, exist_ok=True)
            
            with open(file_path, 'w', encoding='utf-8') as f:
                f.write(file_info['content'])
            logger.info(f"Creato: {file_path}")
        
        # Scrivi i file Docker
        for file_info in docker_files:
            file_path = project_root / file_info['filename']
            file_path.parent.mkdir(parents=True, exist_ok=True)
            
            with open(file_path, 'w', encoding='utf-8') as f:
                f.write(file_info['content'])
            logger.info(f"Creato: {file_path}")
        
        # Crea README
        readme_content = self._generate_readme(project_name)
        with open(project_root / 'README.md', 'w', encoding='utf-8') as f:
            f.write(readme_content)
        logger.info(f"Creato: {project_root / 'README.md'}")
    
    def _generate_readme(self, project_name):
        """Genera un README per il progetto migrato"""
        return f"""# {project_name}

Questo progetto è stato migrato automaticamente da COBOL a Java usando il COBOL to Java Migration Tool.

## Struttura del Progetto

- `src/main/java` - Codice sorgente Java
- `src/main/resources` - File di configurazione
- `src/test/java` - Test unitari
- `build.gradle` - Configurazione build Gradle
- `Dockerfile` - Configurazione Docker
- `docker-compose.yml` - Orchestrazione servizi

## Requisiti

- Java 11+
- Gradle 7+
- Docker (opzionale)
- PostgreSQL

## Build e Esecuzione

### Locale

```bash
# Build del progetto
./gradlew build

# Run dell'applicazione
./gradlew bootRun
```

### Docker

```bash
# Build dell'immagine
docker-compose build

# Avvio dei servizi
docker-compose up
```

## Configurazione Database

L'applicazione si aspetta un database PostgreSQL. 
Configura le credenziali in `src/main/resources/application.properties`.

## API Endpoints

L'applicazione espone REST API per le operazioni bancarie:

- `POST /api/accounts` - Apertura conto
- `POST /api/accounts/{{id}}/deposit` - Deposito
- `POST /api/accounts/{{id}}/withdraw` - Prelievo
- `GET /api/accounts/{{id}}/balance` - Saldo
- `GET /api/accounts/{{id}}/statement` - Estratto conto

## Note sulla Migrazione

- Le procedure COBOL sono state convertite in metodi Java
- Le strutture dati COBOL sono state mappate su classi Java/JPA
- Gli statement SQL embedded sono stati convertiti in JDBC/JPA
- La logica di business è stata preservata
"""


@click.command()
@click.option('--input', '-i', 'input_file', required=True, 
              help='File COBOL di input (.cbl)')
@click.option('--schema', '-s', 'schema_file', 
              help='File SQL con lo schema del database (.sql)')
@click.option('--output', '-o', 'output_dir', required=True,
              help='Directory di output per il progetto Java')
@click.option('--config', '-c', 'config_file', 
              default='config/migration-config.yaml',
              help='File di configurazione')
@click.option('--debug', is_flag=True, help='Abilita debug mode')
def main(input_file, schema_file, output_dir, config_file, debug):
    """COBOL to Java Migration Tool"""
    
    # Configura il livello di logging
    if debug:
        logger.remove()
        logger.add(sys.stderr, level="DEBUG")
    
    # Header
    click.echo(click.style("""
╔═══════════════════════════════════════════╗
║       COBOL to Java Migration Tool        ║
║              Version 1.0.0                ║
╚═══════════════════════════════════════════╝
""", fg='cyan'))
    
    # Verifica che il file di input esista
    if not os.path.exists(input_file):
        click.echo(click.style(f"❌ File non trovato: {input_file}", fg='red'))
        return
    
    # Verifica schema SQL se fornito
    if schema_file and not os.path.exists(schema_file):
        click.echo(click.style(f"❌ Schema SQL non trovato: {schema_file}", fg='red'))
        return
    
    # Crea la directory di output se non esiste
    os.makedirs(output_dir, exist_ok=True)
    
    # Esegui la migrazione
    migrator = CobolToJavaMigrator(config_file)
    
    try:
        click.echo(f"📄 Input: {input_file}")
        if schema_file:
            click.echo(f"🗄️  Schema: {schema_file}")
        click.echo(f"📁 Output: {output_dir}")
        click.echo(f"⚙️  Config: {config_file}")
        click.echo("")
        
        with click.progressbar(length=100, label='Migrazione in corso') as bar:
            # Simula progresso (in produzione useremmo callback reali)
            bar.update(10)
            success = migrator.migrate_file(input_file, schema_file, output_dir)
            bar.update(90)
        
        if success:
            click.echo(click.style("\n✅ Migrazione completata con successo!", fg='green'))
            click.echo(f"\n📁 Progetto Java generato in: {output_dir}")
            click.echo("\nProssimi passi:")
            click.echo("1. cd " + output_dir)
            click.echo("2. ./gradlew build")
            click.echo("3. docker-compose up")
        else:
            click.echo(click.style("\n❌ Migrazione fallita!", fg='red'))
            
    except Exception as e:
        click.echo(click.style(f"\n❌ Errore: {str(e)}", fg='red'))
        if debug:
            import traceback
            traceback.print_exc()


if __name__ == '__main__':
    main()
