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

# Setup del PYTHONPATH per Docker
if '/app/src' not in sys.path:
    sys.path.insert(0, '/app/src')
if '/app' not in sys.path:
    sys.path.insert(0, '/app')

# Import con gestione errori migliorata
def import_modules():
    """Importa i moduli necessari con fallback multipli"""
    modules = {}
    
    # Lista di moduli da importare
    module_imports = [
        ('src.parser.cobol_parser', 'CobolParser'),
        ('src.analyzer.data_analyzer', 'DataAnalyzer'),
        ('src.analyzer.procedure_analyzer', 'ProcedureAnalyzer'),
        ('src.analyzer.sql_analyzer', 'SqlAnalyzer'),
        ('src.parser.sql_schema_parser', 'SqlSchemaParser'),
        ('src.generator.java_generator', 'JavaGenerator'),
        ('src.generator.gradle_generator', 'GradleGenerator'),
        ('src.generator.docker_generator', 'DockerGenerator'),
    ]
    
    # Tentativi di import con diversi path
    import_paths = [
        lambda mod: __import__(mod, fromlist=[''])
    ]
    
    for module_path, class_name in module_imports:
        success = False
        for import_func in import_paths:
            try:
                module = import_func(module_path)
                modules[class_name] = getattr(module, class_name)
                success = True
                break
            except (ImportError, AttributeError) as e:
                continue
        
        if not success:
            logger.error(f"Impossibile importare {class_name} da {module_path}")
            raise ImportError(f"Modulo {class_name} non trovato")
    
    return modules

# Esegui gli import
try:
    imported_modules = import_modules()
    CobolParser = imported_modules['CobolParser']
    DataAnalyzer = imported_modules['DataAnalyzer']
    ProcedureAnalyzer = imported_modules['ProcedureAnalyzer']
    SqlAnalyzer = imported_modules['SqlAnalyzer']
    SqlSchemaParser = imported_modules['SqlSchemaParser']
    JavaGenerator = imported_modules['JavaGenerator']
    GradleGenerator = imported_modules['GradleGenerator']
    DockerGenerator = imported_modules['DockerGenerator']
    logger.info("Tutti i moduli importati con successo")
except Exception as e:
    logger.error(f"Errore fatale negli import: {e}")
    logger.error(f"PYTHONPATH: {sys.path}")
    logger.error(f"Working directory: {os.getcwd()}")
    logger.error(f"Files in /app/src: {os.listdir('/app/src') if os.path.exists('/app/src') else 'Directory non esistente'}")
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
            "migration-config.yaml",
            "/app/config.yaml"
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
        
        # Inizializza i componenti
        try:
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
            
            logger.info("Tutti i componenti inizializzati con successo")
            
        except Exception as e:
            logger.error(f"Errore nell'inizializzazione dei componenti: {e}")
            raise
        
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
            'type_mappings': {
                'PIC 9': 'int',
                'PIC X': 'String',
                'PIC A': 'String'
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
                'file': '/app/logs/migration.log'
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
            logger.info(f"Log file configurato: {log_file}")
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
            try:
                with open(cobol_file_path, 'r', encoding=self.config['migration']['source_encoding']) as f:
                    cobol_code = f.read()
                logger.info(f"File COBOL letto: {len(cobol_code)} caratteri")
            except Exception as e:
                logger.error(f"Errore nella lettura del file COBOL: {e}")
                return False
            
            # 2. Leggi e parsa lo schema SQL
            sql_schema = None
            if sql_file_path and os.path.exists(sql_file_path):
                try:
                    logger.info("Parsing dello schema SQL...")
                    with open(sql_file_path, 'r', encoding='utf-8') as f:
                        sql_content = f.read()
                    sql_schema = self.sql_schema_parser.parse(sql_content)
                    logger.info(f"Schema SQL parsato: {len(sql_schema.get('tables', {}))} tabelle trovate")
                except Exception as e:
                    logger.warning(f"Errore nel parsing dello schema SQL: {e}")
            else:
                logger.warning(f"Schema SQL non trovato: {sql_file_path}")
            
            # 3. Parse del codice COBOL
            try:
                logger.info("Parsing del codice COBOL...")
                ast = self.parser.parse(cobol_code, debug=True)
                
                if not ast:
                    logger.error("Parsing fallito - AST vuoto")
                    return False
                logger.info("AST COBOL creato con successo")
            except Exception as e:
                logger.error(f"Errore nel parsing COBOL: {e}")
                return False
            
            # 4. Analisi dell'AST con schema SQL
            try:
                logger.info("Analisi dell'AST...")
                analysis_result = self._analyze_ast(ast, sql_schema)
                logger.info("Analisi AST completata")
            except Exception as e:
                logger.error(f"Errore nell'analisi AST: {e}")
                return False
            
            # 5. Generazione codice Java
            try:
                logger.info("Generazione codice Java...")
                java_project = self.java_generator.generate(ast, analysis_result)
                logger.info(f"Progetto Java generato: {java_project.get('project_name', 'unknown')}")
            except Exception as e:
                logger.error(f"Errore nella generazione Java: {e}")
                return False
            
            # 6. Generazione struttura Gradle
            try:
                logger.info("Generazione struttura Gradle...")
                gradle_files = self.gradle_generator.generate(java_project)
                logger.info(f"File Gradle generati: {len(gradle_files)}")
            except Exception as e:
                logger.error(f"Errore nella generazione Gradle: {e}")
                return False
            
            # 7. Generazione configurazione Docker
            try:
                logger.info("Generazione configurazione Docker...")
                docker_files = self.docker_generator.generate(java_project)
                logger.info(f"File Docker generati: {len(docker_files)}")
            except Exception as e:
                logger.error(f"Errore nella generazione Docker: {e}")
                return False
            
            # 8. Scrittura dei file
            try:
                logger.info(f"Scrittura dei file in: {output_dir}")
                self._write_output(output_dir, java_project, gradle_files, docker_files)
                logger.info("File scritti con successo")
            except Exception as e:
                logger.error(f"Errore nella scrittura dei file: {e}")
                return False
            
            logger.success(f"Migrazione completata con successo!")
            return True
            
        except Exception as e:
            logger.error(f"Errore durante la migrazione: {str(e)}")
            import traceback
            logger.debug(traceback.format_exc())
            return False
    
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
        try:
            data_analysis = self.data_analyzer.analyze(ast)
            result['data_structures'] = data_analysis
            logger.debug("Analisi dati completata")
        except Exception as e:
            logger.warning(f"Errore nell'analisi dati: {e}")
            result['data_structures'] = {'data_items': {}, 'entities': [], 'dtos': []}
        
        # Analisi delle procedure
        try:
            procedure_analysis = self.procedure_analyzer.analyze(ast)
            result['procedures'] = procedure_analysis
            logger.debug("Analisi procedure completata")
        except Exception as e:
            logger.warning(f"Errore nell'analisi procedure: {e}")
            result['procedures'] = {'procedures': {}, 'services': []}
        
        # Analisi SQL
        try:
            sql_analysis = self.sql_analyzer.analyze(ast)
            result['sql_statements'] = sql_analysis
            logger.debug("Analisi SQL completata")
        except Exception as e:
            logger.warning(f"Errore nell'analisi SQL: {e}")
            result['sql_statements'] = {'statements': [], 'repositories': []}
        
        # Informazioni generali del programma
        result['program_info'] = {
            'program_id': getattr(ast, 'program_id', 'Unknown'),
            'author': getattr(ast, 'author', 'Unknown'),
            'date_written': getattr(ast, 'date_written', 'Unknown')
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
        
        files_written = 0
        for file_info in java_project.get('java_files', []):
            try:
                file_path = java_src_dir / file_info['package'] / file_info['filename']
                file_path.parent.mkdir(parents=True, exist_ok=True)
                
                with open(file_path, 'w', encoding='utf-8') as f:
                    f.write(file_info['content'])
                logger.info(f"Creato: {file_path}")
                files_written += 1
            except Exception as e:
                logger.error(f"Errore nella scrittura di {file_info['filename']}: {e}")
        
        # Scrivi i file di configurazione Spring
        for file_info in java_project.get('config_files', []):
            try:
                file_path = src_main_resources / file_info['filename']
                file_path.parent.mkdir(parents=True, exist_ok=True)
                
                with open(file_path, 'w', encoding='utf-8') as f:
                    f.write(file_info['content'])
                logger.info(f"Creato: {file_path}")
                files_written += 1
            except Exception as e:
                logger.error(f"Errore nella scrittura di {file_info['filename']}: {e}")
        
        # Scrivi i file Gradle
        for file_info in gradle_files:
            try:
                file_path = project_root / file_info['filename']
                file_path.parent.mkdir(parents=True, exist_ok=True)
                
                with open(file_path, 'w', encoding='utf-8') as f:
                    f.write(file_info['content'])
                logger.info(f"Creato: {file_path}")
                files_written += 1
            except Exception as e:
                logger.error(f"Errore nella scrittura di {file_info['filename']}: {e}")
        
        # Scrivi i file Docker
        for file_info in docker_files:
            try:
                file_path = project_root / file_info['filename']
                file_path.parent.mkdir(parents=True, exist_ok=True)
                
                with open(file_path, 'w', encoding='utf-8') as f:
                    f.write(file_info['content'])
                logger.info(f"Creato: {file_path}")
                files_written += 1
            except Exception as e:
                logger.error(f"Errore nella scrittura di {file_info['filename']}: {e}")
        
        # Crea README
        try:
            readme_content = self._generate_readme(project_name)
            with open(project_root / 'README.md', 'w', encoding='utf-8') as f:
                f.write(readme_content)
            logger.info(f"Creato: {project_root / 'README.md'}")
            files_written += 1
        except Exception as e:
            logger.error(f"Errore nella scrittura README: {e}")
        
        logger.info(f"Totale file scritti: {files_written}")
    
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
    
    # Debug info
    logger.debug(f"PYTHONPATH: {sys.path}")
    logger.debug(f"Working directory: {os.getcwd()}")
    
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
    try:
        migrator = CobolToJavaMigrator(config_file)
        
        click.echo(f"📄 Input: {input_file}")
        if schema_file:
            click.echo(f"🗄️  Schema: {schema_file}")
        click.echo(f"📁 Output: {output_dir}")
        click.echo(f"⚙️  Config: {config_file}")
        click.echo("")
        
        with click.progressbar(length=100, label='Migrazione in corso') as bar:
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
