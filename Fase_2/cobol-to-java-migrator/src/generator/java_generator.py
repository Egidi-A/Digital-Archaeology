"""
Java Generator - Genera il codice Java dal codice COBOL analizzato
"""

import os
from typing import Dict, List, Any
from datetime import datetime
from jinja2 import Environment, FileSystemLoader, Template
from loguru import logger


class JavaGenerator:
    """Genera il codice Java dall'analisi COBOL"""
    
    def __init__(self, config: Dict[str, Any]):
        self.config = config
        self.package_base = config.get('java', {}).get('package_base', 'com.migrated.app')
        self.use_lombok = config.get('java', {}).get('use_lombok', True)
        
        # Inizializza i template Jinja2
        self.templates = self._load_templates()
        
    def _load_templates(self) -> Dict[str, str]:
        """Carica i template predefiniti"""
        templates = {
            'entity': self._get_entity_template(),
            'repository': self._get_repository_template(),
            'service': self._get_service_template(),
            'service_impl': self._get_service_impl_template(),
            'controller': self._get_controller_template(),
            'dto': self._get_dto_template(),
            'exception': self._get_exception_template(),
            'main': self._get_main_template(),
            'application_properties': self._get_application_properties_template()
        }
        return templates
    
    def generate(self, ast: Any, analysis: Dict[str, Any]) -> Dict[str, Any]:
        """Genera il progetto Java completo"""
        logger.info("Inizio generazione codice Java")
        
        project_name = self._get_project_name(analysis)
        java_files = []
        config_files = []
        
        # 1. Genera le entità JPA
        entities = self._generate_entities(analysis)
        java_files.extend(entities)
        
        # 2. Genera i repository
        repositories = self._generate_repositories(analysis)
        java_files.extend(repositories)
        
        # 3. Genera i DTO
        dtos = self._generate_dtos(analysis)
        java_files.extend(dtos)
        
        # 4. Genera i servizi
        services = self._generate_services(analysis)
        java_files.extend(services)
        
        # 5. Genera i controller REST
        controllers = self._generate_controllers(analysis)
        java_files.extend(controllers)
        
        # 6. Genera le eccezioni personalizzate
        exceptions = self._generate_exceptions()
        java_files.extend(exceptions)
        
        # 7. Genera la classe principale Spring Boot
        main_class = self._generate_main_class(project_name)
        java_files.append(main_class)
        
        # 8. Genera i file di configurazione
        config_files.append(self._generate_application_properties(analysis))
        config_files.append(self._generate_application_yml(analysis))
        
        result = {
            'project_name': project_name,
            'package_base': self.package_base,
            'java_files': java_files,
            'config_files': config_files,
            'dependencies': self._get_dependencies()
        }
        
        logger.info(f"Generati {len(java_files)} file Java")
        logger.info(f"Generati {len(config_files)} file di configurazione")
        
        return result
    
    def _get_project_name(self, analysis: Dict[str, Any]) -> str:
        """Determina il nome del progetto"""
        program_id = analysis.get('program_info', {}).get('program_id', 'MigratedApp')
        # Converti da COBOL naming a Java naming
        return program_id.lower().replace('_', '-').replace(' ', '-')
    
    def _generate_entities(self, analysis: Dict[str, Any]) -> List[Dict[str, Any]]:
        """Genera le entità JPA"""
        entities = []
        data_structures = analysis.get('data_structures', {})
        
        for entity_info in data_structures.get('entities', []):
            entity_code = self._render_entity(entity_info)
            
            entities.append({
                'package': 'entity',
                'filename': f"{entity_info['class_name']}.java",
                'content': entity_code
            })
        
        return entities
    
    def _render_entity(self, entity_info: Dict[str, Any]) -> str:
        """Renderizza una singola entità"""
        template = Template(self.templates['entity'])
        
        # Prepara i dati per il template
        data = {
            'package': f"{self.package_base}.entity",
            'class_name': entity_info['class_name'],
            'table_name': entity_info['table_name'].lower(),
            'fields': entity_info['fields'],
            'relationships': entity_info.get('relationships', []),
            'use_lombok': self.use_lombok,
            'has_id': any(field.get('id') for field in entity_info['fields']),
            'imports': self._get_entity_imports(entity_info)
        }
        
        return template.render(**data)
    
    def _get_entity_imports(self, entity_info: Dict[str, Any]) -> List[str]:
        """Determina gli import necessari per un'entità"""
        imports = [
            'javax.persistence.*',
            'java.io.Serializable'
        ]
        
        # Aggiungi import per i tipi di campo
        field_types = set(field['type'] for field in entity_info['fields'])
        
        if 'BigDecimal' in field_types:
            imports.append('java.math.BigDecimal')
        if 'LocalDate' in field_types:
            imports.append('java.time.LocalDate')
        if 'LocalDateTime' in field_types:
            imports.append('java.time.LocalDateTime')
        
        if self.use_lombok:
            imports.extend(['lombok.Data', 'lombok.NoArgsConstructor', 'lombok.AllArgsConstructor'])
        
        # Import per le relazioni
        if entity_info.get('relationships'):
            imports.append('java.util.List')
            imports.append('java.util.ArrayList')
        
        return sorted(imports)
    
    def _generate_repositories(self, analysis: Dict[str, Any]) -> List[Dict[str, Any]]:
        """Genera i repository JPA"""
        repositories = []
        sql_analysis = analysis.get('sql_statements', {})
        
        for repo_info in sql_analysis.get('repositories', []):
            repo_code = self._render_repository(repo_info)
            
            repositories.append({
                'package': 'repository',
                'filename': f"{repo_info['name']}.java",
                'content': repo_code
            })
        
        return repositories
    
    def _render_repository(self, repo_info: Dict[str, Any]) -> str:
        """Renderizza un repository"""
        template = Template(self.templates['repository'])
        
        data = {
            'package': f"{self.package_base}.repository",
            'repository_name': repo_info['name'],
            'entity_name': repo_info['entity'],
            'entity_package': f"{self.package_base}.entity",
            'custom_methods': repo_info.get('methods', [])
        }
        
        return template.render(**data)
    
    def _generate_dtos(self, analysis: Dict[str, Any]) -> List[Dict[str, Any]]:
        """Genera i DTO"""
        dtos = []
        data_structures = analysis.get('data_structures', {})
        
        for dto_info in data_structures.get('dtos', []):
            dto_code = self._render_dto(dto_info)
            
            dtos.append({
                'package': 'dto',
                'filename': f"{dto_info['name']}.java",
                'content': dto_code
            })
        
        return dtos
    
    def _render_dto(self, dto_info: Dict[str, Any]) -> str:
        """Renderizza un DTO"""
        template = Template(self.templates['dto'])
        
        data = {
            'package': f"{self.package_base}.dto",
            'class_name': dto_info['name'],
            'fields': dto_info['fields'],
            'use_lombok': self.use_lombok,
            'imports': self._get_dto_imports(dto_info)
        }
        
        return template.render(**data)
    
    def _get_dto_imports(self, dto_info: Dict[str, Any]) -> List[str]:
        """Determina gli import necessari per un DTO"""
        imports = []
        
        # Import per validazione
        has_validation = any(field.get('validation') for field in dto_info['fields'])
        if has_validation:
            imports.append('javax.validation.constraints.*')
        
        # Import per i tipi
        field_types = set(field['type'] for field in dto_info['fields'])
        if 'BigDecimal' in field_types:
            imports.append('java.math.BigDecimal')
        if 'LocalDate' in field_types:
            imports.append('java.time.LocalDate')
        
        if self.use_lombok:
            imports.extend(['lombok.Data', 'lombok.NoArgsConstructor', 'lombok.AllArgsConstructor'])
        
        return sorted(imports)
    
    def _generate_services(self, analysis: Dict[str, Any]) -> List[Dict[str, Any]]:
        """Genera i servizi"""
        services = []
        procedure_analysis = analysis.get('procedures', {})
        
        for service_info in procedure_analysis.get('services', []):
            # Genera interfaccia
            interface_code = self._render_service_interface(service_info)
            services.append({
                'package': 'service',
                'filename': f"{service_info['name']}.java",
                'content': interface_code
            })
            
            # Genera implementazione
            impl_code = self._render_service_impl(service_info, analysis)
            services.append({
                'package': 'service/impl',
                'filename': f"{service_info['name']}Impl.java",
                'content': impl_code
            })
        
        return services
    
    def _render_service_interface(self, service_info: Dict[str, Any]) -> str:
        """Renderizza l'interfaccia del servizio"""
        template = Template(self.templates['service'])
        
        data = {
            'package': f"{self.package_base}.service",
            'service_name': service_info['name'],
            'methods': service_info['methods'],
            'imports': self._get_service_imports(service_info)
        }
        
        return template.render(**data)
    
    def _render_service_impl(self, service_info: Dict[str, Any], analysis: Dict[str, Any]) -> str:
        """Renderizza l'implementazione del servizio"""
        template = Template(self.templates['service_impl'])
        
        # Mappa le procedure COBOL sui metodi Java
        method_implementations = []
        procedures = analysis.get('procedures', {}).get('procedures', {})
        
        for method in service_info['methods']:
            original_proc = method.get('original_procedure')
            if original_proc and original_proc in procedures:
                proc_info = procedures[original_proc]
                method['implementation'] = self._convert_procedure_to_java(proc_info)
            method_implementations.append(method)
        
        data = {
            'package': f"{self.package_base}.service.impl",
            'service_name': service_info['name'],
            'methods': method_implementations,
            'repositories': self._get_required_repositories(service_info),
            'imports': self._get_service_impl_imports(service_info)
        }
        
        return template.render(**data)
    
    def _convert_procedure_to_java(self, proc_info: Dict[str, Any]) -> str:
        """Converte una procedura COBOL in implementazione Java"""
        # Questa è una versione semplificata
        # In un sistema reale, questa conversione sarebbe molto più complessa
        
        impl_lines = []
        
        # Gestisci operazioni SQL
        for sql_op in proc_info.get('sql_operations', []):
            if sql_op['type'] == 'SELECT':
                impl_lines.append(f"// TODO: Implementare SELECT da {sql_op.get('table')}")
            elif sql_op['type'] == 'INSERT':
                impl_lines.append(f"// TODO: Implementare INSERT in {sql_op.get('table')}")
            elif sql_op['type'] == 'UPDATE':
                impl_lines.append(f"// TODO: Implementare UPDATE di {sql_op.get('table')}")
        
        # Gestisci logica business
        for logic in proc_info.get('business_logic', []):
            impl_lines.append(f"// TODO: Implementare logica {logic.get('type')}")
        
        return '\n'.join(impl_lines) if impl_lines else "// TODO: Implementare logica"
    
    def _get_required_repositories(self, service_info: Dict[str, Any]) -> List[str]:
        """Determina quali repository sono necessari per un servizio"""
        # Per semplicità, includiamo i repository principali
        return ['ContiRepository', 'ClientiRepository', 'MovimentiRepository']
    
    def _get_service_imports(self, service_info: Dict[str, Any]) -> List[str]:
        """Import per l'interfaccia del servizio"""
        imports = [f"{self.package_base}.dto.*"]
        
        # Import per eccezioni
        if any(method.get('throws') for method in service_info['methods']):
            imports.append(f"{self.package_base}.exception.*")
        
        # Import per collezioni
        if any('List' in method.get('return', '') for method in service_info['methods']):
            imports.append('java.util.List')
        
        return sorted(imports)
    
    def _get_service_impl_imports(self, service_info: Dict[str, Any]) -> List[str]:
        """Import per l'implementazione del servizio"""
        imports = [
            'org.springframework.stereotype.Service',
            'org.springframework.transaction.annotation.Transactional',
            'org.springframework.beans.factory.annotation.Autowired',
            'org.slf4j.Logger',
            'org.slf4j.LoggerFactory',
            f"{self.package_base}.service.{service_info['name']}",
            f"{self.package_base}.repository.*",
            f"{self.package_base}.entity.*",
            f"{self.package_base}.dto.*",
            f"{self.package_base}.exception.*"
        ]
        
        return sorted(imports)
    
    def _generate_controllers(self, analysis: Dict[str, Any]) -> List[Dict[str, Any]]:
        """Genera i controller REST"""
        controllers = []
        
        # Controller principale per le operazioni bancarie
        controller_info = {
            'name': 'BankAccountController',
            'path': '/api/accounts',
            'service': 'BankAccountService',
            'endpoints': self._get_rest_endpoints(analysis)
        }
        
        controller_code = self._render_controller(controller_info)
        controllers.append({
            'package': 'controller',
            'filename': f"{controller_info['name']}.java",
            'content': controller_code
        })
        
        return controllers
    
    def _get_rest_endpoints(self, analysis: Dict[str, Any]) -> List[Dict[str, Any]]:
        """Definisce gli endpoint REST basati sulle procedure COBOL"""
        endpoints = [
            {
                'method': 'POST',
                'path': '',
                'operation': 'createAccount',
                'description': 'Crea un nuovo conto corrente'
            },
            {
                'method': 'POST',
                'path': '/{accountNumber}/deposit',
                'operation': 'deposit',
                'description': 'Effettua un deposito'
            },
            {
                'method': 'POST',
                'path': '/{accountNumber}/withdraw',
                'operation': 'withdraw',
                'description': 'Effettua un prelievo'
            },
            {
                'method': 'GET',
                'path': '/{accountNumber}/balance',
                'operation': 'getBalance',
                'description': 'Ottiene il saldo del conto'
            },
            {
                'method': 'GET',
                'path': '/{accountNumber}/statement',
                'operation': 'getStatement',
                'description': 'Ottiene l\'estratto conto'
            },
            {
                'method': 'DELETE',
                'path': '/{accountNumber}',
                'operation': 'closeAccount',
                'description': 'Chiude un conto'
            }
        ]
        
        return endpoints
    
    def _render_controller(self, controller_info: Dict[str, Any]) -> str:
        """Renderizza un controller"""
        template = Template(self.templates['controller'])
        
        data = {
            'package': f"{self.package_base}.controller",
            'controller_name': controller_info['name'],
            'base_path': controller_info['path'],
            'service_name': controller_info['service'],
            'endpoints': controller_info['endpoints'],
            'imports': [
                'org.springframework.web.bind.annotation.*',
                'org.springframework.beans.factory.annotation.Autowired',
                'org.springframework.http.ResponseEntity',
                'org.springframework.http.HttpStatus',
                'javax.validation.Valid',
                f"{self.package_base}.service.{controller_info['service']}",
                f"{self.package_base}.dto.*",
                'org.slf4j.Logger',
                'org.slf4j.LoggerFactory'
            ]
        }
        
        return template.render(**data)
    
    def _generate_exceptions(self) -> List[Dict[str, Any]]:
        """Genera le classi di eccezione personalizzate"""
        exceptions = []
        
        exception_types = [
            {
                'name': 'BusinessException',
                'extends': 'RuntimeException',
                'description': 'Eccezione generica di business'
            },
            {
                'name': 'AccountNotFoundException',
                'extends': 'BusinessException',
                'description': 'Conto non trovato'
            },
            {
                'name': 'InsufficientFundsException',
                'extends': 'BusinessException',
                'description': 'Fondi insufficienti'
            }
        ]
        
        for exc_info in exception_types:
            exc_code = self._render_exception(exc_info)
            exceptions.append({
                'package': 'exception',
                'filename': f"{exc_info['name']}.java",
                'content': exc_code
            })
        
        return exceptions
    
    def _render_exception(self, exc_info: Dict[str, Any]) -> str:
        """Renderizza una classe di eccezione"""
        template = Template(self.templates['exception'])
        
        data = {
            'package': f"{self.package_base}.exception",
            'class_name': exc_info['name'],
            'extends': exc_info['extends'],
            'description': exc_info['description']
        }
        
        return template.render(**data)
    
    def _generate_main_class(self, project_name: str) -> Dict[str, Any]:
        """Genera la classe principale Spring Boot"""
        template = Template(self.templates['main'])
        
        class_name = self._to_class_name(project_name) + 'Application'
        
        data = {
            'package': self.package_base,
            'class_name': class_name,
            'description': f'Applicazione migrata da COBOL - {project_name}'
        }
        
        content = template.render(**data)
        
        return {
            'package': '',
            'filename': f"{class_name}.java",
            'content': content
        }
    
    def _generate_application_properties(self, analysis: Dict[str, Any]) -> Dict[str, Any]:
        """Genera application.properties"""
        template = Template(self.templates['application_properties'])
        
        # Estrai informazioni sul database dallo schema SQL
        db_info = analysis.get('sql_schema', {})
        
        data = {
            'app_name': analysis.get('program_info', {}).get('program_id', 'migrated-app'),
            'server_port': self.config.get('docker', {}).get('expose_port', 8080),
            'db_type': self.config.get('database', {}).get('type', 'postgresql'),
            'db_name': 'banca',  # Estratto dallo schema SQL se disponibile
            'db_host': 'localhost',
            'db_port': 5432,
            'db_user': 'postgres',
            'db_password': 'password'
        }
        
        content = template.render(**data)
        
        return {
            'filename': 'application.properties',
            'content': content
        }
    
    def _generate_application_yml(self, analysis: Dict[str, Any]) -> Dict[str, Any]:
        """Genera application.yml come alternativa"""
        content = f"""# Configurazione Spring Boot
spring:
  application:
    name: {analysis.get('program_info', {}).get('program_id', 'migrated-app')}
  
  datasource:
    url: jdbc:postgresql://localhost:5432/banca
    username: postgres
    password: password
    driver-class-name: org.postgresql.Driver
  
  jpa:
    hibernate:
      ddl-auto: validate
    properties:
      hibernate:
        dialect: org.hibernate.dialect.PostgreSQLDialect
        format_sql: true
    show-sql: true

server:
  port: {self.config.get('docker', {}).get('expose_port', 8080)}

logging:
  level:
    root: INFO
    {self.package_base}: DEBUG
"""
        
        return {
            'filename': 'application.yml',
            'content': content
        }
    
    def _get_dependencies(self) -> List[str]:
        """Restituisce le dipendenze Maven/Gradle necessarie"""
        return self.config.get('spring', {}).get('dependencies', [
            'spring-boot-starter-web',
            'spring-boot-starter-data-jpa',
            'postgresql',
            'lombok',
            'spring-boot-starter-validation'
        ])
    
    def _to_class_name(self, name: str) -> str:
        """Converte un nome in PascalCase"""
        parts = name.replace('-', '_').split('_')
        return ''.join(p.capitalize() for p in parts)
    
    # Template methods
    def _get_entity_template(self) -> str:
        return """package {{ package }};

{% for import in imports %}
import {{ import }};
{% endfor %}

/**
 * Entità JPA per la tabella {{ table_name }}
 * Generata automaticamente dal COBOL to Java Migration Tool
 */
{% if use_lombok %}
@Data
@NoArgsConstructor
@AllArgsConstructor
{% endif %}
@Entity
@Table(name = "{{ table_name }}")
public class {{ class_name }} implements Serializable {
    
    private static final long serialVersionUID = 1L;
    
{% for field in fields %}
    {% if field.id %}
    @Id
    {% if field.auto_increment %}
    @GeneratedValue(strategy = GenerationType.IDENTITY)
    {% endif %}
    {% endif %}
    {% if field.unique and not field.id %}
    @Column(unique = true)
    {% endif %}
    {% if field.db_name != field.name %}
    @Column(name = "{{ field.db_name }}"{% if field.nullable == false %}, nullable = false{% endif %}{% if field.length %}, length = {{ field.length }}{% endif %})
    {% endif %}
    private {{ field.type }} {{ field.name }};
    
{% endfor %}
{% for rel in relationships %}
    @{{ rel.type }}
    @JoinColumn(name = "{{ rel.join_column }}", referencedColumnName = "{{ rel.referenced_column }}")
    private {{ rel.target_entity }} {{ rel.field_name }};
    
{% endfor %}
{% if not use_lombok %}
    // Getters and setters
    {% for field in fields %}
    public {{ field.type }} get{{ field.name|capitalize }}() {
        return {{ field.name }};
    }
    
    public void set{{ field.name|capitalize }}({{ field.type }} {{ field.name }}) {
        this.{{ field.name }} = {{ field.name }};
    }
    
    {% endfor %}
{% endif %}
}"""
    
    def _get_repository_template(self) -> str:
        return """package {{ package }};

import {{ entity_package }}.{{ entity_name }};
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Query;
import org.springframework.data.repository.query.Param;
import org.springframework.stereotype.Repository;

import java.util.List;
import java.util.Optional;

/**
 * Repository JPA per {{ entity_name }}
 * Generato automaticamente dal COBOL to Java Migration Tool
 */
@Repository
public interface {{ repository_name }} extends JpaRepository<{{ entity_name }}, String> {
    
{% for method in custom_methods %}
    {% if method.query %}
    @Query("{{ method.query }}")
    {% endif %}
    {{ method.return }} {{ method.name }}({% for param in method.params %}{% if loop.index > 1 %}, {% endif %}@Param("{{ param.name }}") {{ param.type }} {{ param.name }}{% endfor %});
    
{% endfor %}
}"""
    
    def _get_service_template(self) -> str:
        return """package {{ package }};

{% for import in imports %}
import {{ import }};
{% endfor %}

/**
 * Servizio per le operazioni bancarie
 * Generato automaticamente dal COBOL to Java Migration Tool
 */
public interface {{ service_name }} {
    
{% for method in methods %}
    /**
     * {{ method.name }} - Migrato da procedura COBOL {{ method.original_procedure }}
     */
    {{ method.return }} {{ method.name }}({% for param in method.params %}{% if loop.index > 1 %}, {% endif %}{{ param.type }} {{ param.name }}{% endfor %}){% if method.throws %} throws {% for exc in method.throws %}{% if loop.index > 1 %}, {% endif %}{{ exc }}{% endfor %}{% endif %};
    
{% endfor %}
}"""
    
    def _get_service_impl_template(self) -> str:
        return """package {{ package }};

{% for import in imports %}
import {{ import }};
{% endfor %}

/**
 * Implementazione del servizio per le operazioni bancarie
 * Generata automaticamente dal COBOL to Java Migration Tool
 */
@Service
@Transactional
public class {{ service_name }}Impl implements {{ service_name }} {
    
    private static final Logger logger = LoggerFactory.getLogger({{ service_name }}Impl.class);
    
{% for repo in repositories %}
    @Autowired
    private {{ repo }} {{ repo|lower }};
    
{% endfor %}
    
{% for method in methods %}
    @Override
    public {{ method.return }} {{ method.name }}({% for param in method.params %}{% if loop.index > 1 %}, {% endif %}{{ param.type }} {{ param.name }}{% endfor %}){% if method.throws %} throws {% for exc in method.throws %}{% if loop.index > 1 %}, {% endif %}{{ exc }}{% endfor %}{% endif %} {
        logger.info("Esecuzione {{ method.name }}");
        
        {% if method.implementation %}
        {{ method.implementation }}
        {% else %}
        // TODO: Implementare la logica migrata dalla procedura COBOL
        throw new UnsupportedOperationException("Metodo non ancora implementato");
        {% endif %}
    }
    
{% endfor %}
}"""
    
    def _get_controller_template(self) -> str:
        return """package {{ package }};

{% for import in imports %}
import {{ import }};
{% endfor %}

/**
 * REST Controller per le operazioni bancarie
 * Generato automaticamente dal COBOL to Java Migration Tool
 */
@RestController
@RequestMapping("{{ base_path }}")
public class {{ controller_name }} {
    
    private static final Logger logger = LoggerFactory.getLogger({{ controller_name }}.class);
    
    @Autowired
    private {{ service_name }} {{ service_name|lower }};
    
{% for endpoint in endpoints %}
    /**
     * {{ endpoint.description }}
     */
    @{{ endpoint.method }}Mapping("{{ endpoint.path }}")
    public ResponseEntity<?> {{ endpoint.operation }}(
        {% if '{accountNumber}' in endpoint.path %}@PathVariable String accountNumber{% if endpoint.method == 'POST' %}, {% endif %}{% endif %}
        {% if endpoint.method == 'POST' %}@Valid @RequestBody Object request{% endif %}
    ) {
        try {
            logger.info("{{ endpoint.method }} {{ base_path }}{{ endpoint.path }}");
            
            // TODO: Implementare chiamata al servizio
            
            return ResponseEntity.ok().build();
        } catch (Exception e) {
            logger.error("Errore in {{ endpoint.operation }}", e);
            return ResponseEntity.status(HttpStatus.INTERNAL_SERVER_ERROR).build();
        }
    }
    
{% endfor %}
}"""
    
    def _get_dto_template(self) -> str:
        return """package {{ package }};

{% for import in imports %}
import {{ import }};
{% endfor %}

/**
 * DTO per {{ class_name }}
 * Generato automaticamente dal COBOL to Java Migration Tool
 */
{% if use_lombok %}
@Data
@NoArgsConstructor
@AllArgsConstructor
{% endif %}
public class {{ class_name }} {
    
{% for field in fields %}
    {% if field.validation %}
    @{{ field.validation }}
    {% endif %}
    private {{ field.type }} {{ field.name }};
    
{% endfor %}
{% if not use_lombok %}
    // Getters and setters
    {% for field in fields %}
    public {{ field.type }} get{{ field.name|capitalize }}() {
        return {{ field.name }};
    }
    
    public void set{{ field.name|capitalize }}({{ field.type }} {{ field.name }}) {
        this.{{ field.name }} = {{ field.name }};
    }
    
    {% endfor %}
{% endif %}
}"""
    
    def _get_exception_template(self) -> str:
        return """package {{ package }};

/**
 * {{ description }}
 * Generata automaticamente dal COBOL to Java Migration Tool
 */
public class {{ class_name }} extends {{ extends }} {
    
    public {{ class_name }}() {
        super();
    }
    
    public {{ class_name }}(String message) {
        super(message);
    }
    
    public {{ class_name }}(String message, Throwable cause) {
        super(message, cause);
    }
    
    public {{ class_name }}(Throwable cause) {
        super(cause);
    }
}"""
    
    def _get_main_template(self) -> str:
        return """package {{ package }};

import org.springframework.boot.SpringApplication;
import org.springframework.boot.autoconfigure.SpringBootApplication;

/**
 * {{ description }}
 * Generata automaticamente dal COBOL to Java Migration Tool
 */
@SpringBootApplication
public class {{ class_name }} {
    
    public static void main(String[] args) {
        SpringApplication.run({{ class_name }}.class, args);
    }
}"""
    
    def _get_application_properties_template(self) -> str:
        return """# Configurazione applicazione
spring.application.name={{ app_name }}
server.port={{ server_port }}

# Configurazione database
spring.datasource.url=jdbc:{{ db_type }}://{{ db_host }}:{{ db_port }}/{{ db_name }}
spring.datasource.username={{ db_user }}
spring.datasource.password={{ db_password }}
spring.datasource.driver-class-name=org.postgresql.Driver

# Configurazione JPA
spring.jpa.hibernate.ddl-auto=validate
spring.jpa.properties.hibernate.dialect=org.hibernate.dialect.PostgreSQLDialect
spring.jpa.properties.hibernate.format_sql=true
spring.jpa.show-sql=true

# Configurazione logging
logging.level.root=INFO
logging.level.{{ package_base }}=DEBUG
logging.pattern.console=%d{yyyy-MM-dd HH:mm:ss} - %msg%n

# Configurazione Jackson
spring.jackson.serialization.write-dates-as-timestamps=false
spring.jackson.time-zone=Europe/Rome"""