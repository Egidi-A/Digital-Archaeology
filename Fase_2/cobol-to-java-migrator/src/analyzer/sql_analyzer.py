"""
SQL Analyzer - Analizza gli statement SQL embedded nel codice COBOL
"""

from typing import Dict, List, Any, Set
from loguru import logger
from ..parser.ast_nodes import *


class SqlAnalyzer:
    """Analizza gli statement SQL nell'AST COBOL"""
    
    def __init__(self, config: Dict[str, Any]):
        self.config = config
        self.sql_statements = []
        self.cursors = {}
        self.host_variables = set()
        self.tables_used = set()
        self.sql_schema = None
        
    def set_sql_schema(self, sql_schema: Dict[str, Any]):
        """Imposta lo schema SQL parsato"""
        self.sql_schema = sql_schema
        
    def analyze(self, ast: ASTNode) -> Dict[str, Any]:
        """Analizza l'AST e estrae gli statement SQL"""
        logger.info("Inizio analisi degli statement SQL")
        
        # Reset
        self.sql_statements = []
        self.cursors = {}
        self.host_variables = set()
        self.tables_used = set()
        
        # Analizza ricorsivamente l'AST
        self._analyze_node(ast)
        
        # Organizza i risultati
        result = {
            'statements': self._organize_statements(),
            'cursors': self.cursors,
            'host_variables': list(self.host_variables),
            'tables_used': list(self.tables_used),
            'repositories': self._generate_repositories(),
            'queries': self._generate_query_methods()
        }
        
        logger.info(f"Trovati {len(self.sql_statements)} statement SQL")
        logger.info(f"Trovati {len(self.cursors)} cursori")
        logger.info(f"Tabelle utilizzate: {', '.join(self.tables_used)}")
        
        return result
    
    def _analyze_node(self, node: ASTNode):
        """Analizza ricorsivamente un nodo dell'AST"""
        if isinstance(node, SqlStatementNode):
            self._analyze_sql_statement(node)
        
        # Analizza i figli
        for child in node.children:
            self._analyze_node(child)
    
    def _analyze_sql_statement(self, stmt: SqlStatementNode):
        """Analizza un singolo statement SQL"""
        sql_info = {
            'type': stmt.sql_type,
            'sql_text': stmt.sql_text,
            'table_name': stmt.table_name,
            'line_number': stmt.line_number,
            'host_variables': stmt.host_variables,
            'into_variables': stmt.into_variables,
            'is_cursor': stmt.is_cursor,
            'cursor_name': stmt.cursor_name
        }
        
        # Aggiungi le tabelle utilizzate
        if stmt.table_name:
            self.tables_used.add(stmt.table_name.upper())
        
        # Aggiungi le host variables
        for var in stmt.host_variables:
            self.host_variables.add(var)
        for var in stmt.into_variables:
            self.host_variables.add(var)
        
        # Gestisci i cursori
        if stmt.is_cursor and stmt.cursor_name:
            if stmt.sql_type == 'DECLARE_CURSOR':
                self.cursors[stmt.cursor_name] = {
                    'name': stmt.cursor_name,
                    'query': stmt.sql_text,
                    'operations': ['DECLARE']
                }
            elif stmt.cursor_name in self.cursors:
                self.cursors[stmt.cursor_name]['operations'].append(stmt.sql_type)
        
        # Analizza e migliora lo statement SQL
        sql_info['java_equivalent'] = self._generate_java_equivalent(stmt)
        sql_info['jpa_method'] = self._generate_jpa_method(stmt)
        
        self.sql_statements.append(sql_info)
    
    def _organize_statements(self) -> Dict[str, List[Dict[str, Any]]]:
        """Organizza gli statement per tipo"""
        organized = {
            'select': [],
            'insert': [],
            'update': [],
            'delete': [],
            'cursor': [],
            'other': []
        }
        
        for stmt in self.sql_statements:
            sql_type = stmt['type'].lower()
            if sql_type in ['select']:
                organized['select'].append(stmt)
            elif sql_type in ['insert']:
                organized['insert'].append(stmt)
            elif sql_type in ['update']:
                organized['update'].append(stmt)
            elif sql_type in ['delete']:
                organized['delete'].append(stmt)
            elif 'cursor' in sql_type:
                organized['cursor'].append(stmt)
            else:
                organized['other'].append(stmt)
        
        return organized
    
    def _generate_repositories(self) -> List[Dict[str, Any]]:
        """Genera i repository JPA per le tabelle utilizzate"""
        repositories = []
        
        for table_name in self.tables_used:
            entity_name = self._to_class_name(table_name)
            repo_name = f"{entity_name}Repository"
            
            repository = {
                'name': repo_name,
                'entity': entity_name,
                'table': table_name,
                'methods': []
            }
            
            # Aggiungi metodi personalizzati basati sulle query trovate
            for stmt in self.sql_statements:
                if stmt['table_name'] and stmt['table_name'].upper() == table_name:
                    method = self._create_repository_method(stmt, entity_name)
                    if method and method not in repository['methods']:
                        repository['methods'].append(method)
            
            repositories.append(repository)
        
        return repositories
    
    def _create_repository_method(self, stmt: Dict[str, Any], entity_name: str) -> Dict[str, Any]:
        """Crea un metodo repository da uno statement SQL"""
        sql_type = stmt['type'].upper()
        
        if sql_type == 'SELECT':
            # Analizza la query per determinare il metodo
            sql_text = stmt['sql_text'].upper()
            
            if 'WHERE' in sql_text:
                # Estrai le condizioni WHERE
                where_part = sql_text.split('WHERE')[1].strip()
                
                # Casi comuni
                if 'NUMERO_CONTO =' in where_part:
                    return {
                        'name': 'findByNumeroConto',
                        'params': [{'name': 'numeroConto', 'type': 'String'}],
                        'return': f'Optional<{entity_name}>',
                        'query': stmt['sql_text']
                    }
                elif 'CODICE_CLIENTE =' in where_part:
                    return {
                        'name': 'findByCodiceCliente',
                        'params': [{'name': 'codiceCliente', 'type': 'String'}],
                        'return': f'List<{entity_name}>',
                        'query': stmt['sql_text']
                    }
                elif 'STATO =' in where_part:
                    return {
                        'name': 'findByStato',
                        'params': [{'name': 'stato', 'type': 'String'}],
                        'return': f'List<{entity_name}>',
                        'query': stmt['sql_text']
                    }
        
        return None
    
    def _generate_query_methods(self) -> List[Dict[str, Any]]:
        """Genera metodi per query complesse"""
        query_methods = []
        
        # Analizza i cursori per query complesse
        for cursor_name, cursor_info in self.cursors.items():
            query = cursor_info['query']
            
            # Esempio: cursore per movimenti
            if 'MOVIMENTI' in query and 'ORDER BY' in query:
                method = {
                    'name': 'findMovimentiByContoOrdered',
                    'params': [
                        {'name': 'numeroConto', 'type': 'String'},
                        {'name': 'pageable', 'type': 'Pageable'}
                    ],
                    'return': 'Page<Movimento>',
                    'query_annotation': '@Query',
                    'original_cursor': cursor_name
                }
                query_methods.append(method)
        
        return query_methods
    
    def _generate_java_equivalent(self, stmt: SqlStatementNode) -> str:
        """Genera l'equivalente Java di uno statement SQL"""
        sql_type = stmt.sql_type.upper()
        
        if sql_type == 'SELECT':
            if stmt.into_variables:
                # Query singola con INTO
                return f"// Query: {stmt.sql_text}\n// TODO: Implementare con JPA o JDBC"
            else:
                # Query per cursore
                return f"// Cursor query: {stmt.sql_text}\n// TODO: Implementare con JPA Pageable"
        
        elif sql_type == 'INSERT':
            return f"// Insert: repository.save(entity);"
        
        elif sql_type == 'UPDATE':
            return f"// Update: repository.save(entity);"
        
        elif sql_type == 'DELETE':
            return f"// Delete: repository.delete(entity);"
        
        return f"// SQL: {stmt.sql_text}"
    
    def _generate_jpa_method(self, stmt: SqlStatementNode) -> str:
        """Genera il metodo JPA equivalente"""
        sql_type = stmt.sql_type.upper()
        
        if sql_type == 'SELECT' and stmt.table_name:
            entity = self._to_class_name(stmt.table_name)
            
            # Semplici query by field
            if 'WHERE' in stmt.sql_text.upper():
                where_clause = stmt.sql_text.upper().split('WHERE')[1]
                
                if '=' in where_clause:
                    field = where_clause.split('=')[0].strip()
                    java_field = self._to_camel_case(field)
                    return f"repository.findBy{java_field.capitalize()}(...)"
        
        return None
    
    def _to_class_name(self, name: str) -> str:
        """Converte un nome in PascalCase per le classi Java"""
        parts = name.replace('-', '_').split('_')
        return ''.join(p.capitalize() for p in parts)
    
    def _to_camel_case(self, snake_str: str) -> str:
        """Converte snake_case in camelCase"""
        components = snake_str.lower().split('_')
        return components[0] + ''.join(x.title() for x in components[1:])