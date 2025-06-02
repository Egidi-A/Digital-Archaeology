"""
SQL Schema Parser - Estrae la struttura delle tabelle dal file SQL
"""

import re
from typing import Dict, List, Any
from loguru import logger


class SqlSchemaParser:
    """Parser per estrarre la struttura del database dal file SQL"""
    
    def __init__(self):
        self.tables = {}
        self.foreign_keys = {}
        self.indexes = {}
        
    def parse(self, sql_content: str) -> Dict[str, Any]:
        """Parsa il contenuto SQL e estrae la struttura"""
        logger.info("Parsing dello schema SQL...")
        
        # Reset
        self.tables = {}
        self.foreign_keys = {}
        self.indexes = {}
        
        # Rimuovi commenti
        sql_content = self._remove_comments(sql_content)
        
        # Estrai le definizioni delle tabelle
        self._extract_tables(sql_content)
        
        # Estrai gli indici
        self._extract_indexes(sql_content)
        
        result = {
            'tables': self.tables,
            'foreign_keys': self.foreign_keys,
            'indexes': self.indexes
        }
        
        logger.info(f"Trovate {len(self.tables)} tabelle nel database")
        
        return result
    
    def _remove_comments(self, sql: str) -> str:
        """Rimuove i commenti SQL"""
        # Rimuovi commenti su singola linea (--)
        sql = re.sub(r'--.*$', '', sql, flags=re.MULTILINE)
        # Rimuovi commenti multilinea (/* */)
        sql = re.sub(r'/\*.*?\*/', '', sql, flags=re.DOTALL)
        return sql
    
    def _extract_tables(self, sql: str):
        """Estrae le definizioni delle tabelle"""
        # Pattern per CREATE TABLE
        table_pattern = r'CREATE\s+TABLE\s+(\w+)\s*\((.*?)\);'
        
        for match in re.finditer(table_pattern, sql, re.IGNORECASE | re.DOTALL):
            table_name = match.group(1).upper()
            table_content = match.group(2)
            
            logger.debug(f"Analizzando tabella: {table_name}")
            
            # Estrai colonne e constraints
            columns = self._parse_table_content(table_name, table_content)
            
            self.tables[table_name] = {
                'name': table_name,
                'columns': columns,
                'primary_key': self._find_primary_key(columns, table_content),
                'foreign_keys': []
            }
    
    def _parse_table_content(self, table_name: str, content: str) -> List[Dict[str, Any]]:
        """Parsa il contenuto di una CREATE TABLE"""
        columns = []
        lines = content.split(',')
        
        for line in lines:
            line = line.strip()
            
            # Skip constraints di tabella
            if any(keyword in line.upper() for keyword in ['FOREIGN KEY', 'PRIMARY KEY', 'CHECK', 'UNIQUE']):
                # Gestisci foreign key
                if 'FOREIGN KEY' in line.upper():
                    self._parse_foreign_key(table_name, line)
                continue
            
            # Parsa colonna
            column = self._parse_column(line)
            if column:
                columns.append(column)
        
        return columns
    
    def _parse_column(self, line: str) -> Dict[str, Any]:
        """Parsa una definizione di colonna"""
        # Pattern base: nome tipo [constraints]
        pattern = r'^(\w+)\s+(\w+(?:\([^)]+\))?)\s*(.*?)$'
        match = re.match(pattern, line.strip())
        
        if not match:
            return None
        
        column_name = match.group(1)
        data_type = match.group(2)
        constraints = match.group(3) if match.group(3) else ""
        
        # Parsa il tipo di dato
        type_info = self._parse_data_type(data_type)
        
        # Parsa i constraints
        column_def = {
            'name': column_name,
            'db_name': column_name.lower(),
            'type': type_info['base_type'],
            'sql_type': data_type.upper(),
            'length': type_info.get('length'),
            'precision': type_info.get('precision'),
            'scale': type_info.get('scale'),
            'nullable': 'NOT NULL' not in constraints.upper(),
            'unique': 'UNIQUE' in constraints.upper(),
            'primary_key': 'PRIMARY KEY' in constraints.upper(),
            'default': self._extract_default(constraints),
            'check': self._extract_check(constraints)
        }
        
        # Mappa su tipo Java
        column_def['java_type'] = self._map_sql_to_java_type(column_def)
        column_def['java_name'] = self._to_camel_case(column_name)
        
        return column_def
    
    def _parse_data_type(self, data_type: str) -> Dict[str, Any]:
        """Parsa un tipo di dato SQL"""
        type_upper = data_type.upper()
        result = {'base_type': type_upper}
        
        # Estrai dimensioni se presenti
        if '(' in type_upper:
            base = type_upper[:type_upper.index('(')]
            params = type_upper[type_upper.index('(')+1:type_upper.index(')')]
            
            result['base_type'] = base
            
            if ',' in params:
                # DECIMAL(15,2)
                parts = params.split(',')
                result['precision'] = int(parts[0].strip())
                result['scale'] = int(parts[1].strip())
            else:
                # VARCHAR(50), CHAR(8)
                result['length'] = int(params.strip())
        
        return result
    
    def _map_sql_to_java_type(self, column: Dict[str, Any]) -> str:
        """Mappa un tipo SQL su tipo Java"""
        sql_type = column['type']
        
        mapping = {
            'CHAR': 'String',
            'VARCHAR': 'String',
            'TEXT': 'String',
            'INT': 'Integer',
            'INTEGER': 'Integer',
            'BIGINT': 'Long',
            'SERIAL': 'Long',
            'DECIMAL': 'BigDecimal',
            'NUMERIC': 'BigDecimal',
            'FLOAT': 'Double',
            'DOUBLE': 'Double',
            'REAL': 'Float',
            'BOOLEAN': 'Boolean',
            'DATE': 'LocalDate',
            'TIME': 'LocalTime',
            'TIMESTAMP': 'LocalDateTime',
            'BYTEA': 'byte[]',
            'BLOB': 'byte[]'
        }
        
        # Gestisci SERIAL (auto-increment)
        if sql_type == 'SERIAL':
            column['auto_increment'] = True
        
        return mapping.get(sql_type, 'String')
    
    def _extract_default(self, constraints: str) -> Any:
        """Estrae il valore DEFAULT dai constraints"""
        pattern = r'DEFAULT\s+([^\s]+)'
        match = re.search(pattern, constraints, re.IGNORECASE)
        
        if match:
            default_value = match.group(1)
            # Rimuovi quotes se presenti
            if default_value.startswith("'") and default_value.endswith("'"):
                return default_value[1:-1]
            # Gestisci funzioni
            if default_value.upper() in ['CURRENT_DATE', 'CURRENT_TIMESTAMP', 'NOW()']:
                return default_value.upper()
            return default_value
        
        return None
    
    def _extract_check(self, constraints: str) -> str:
        """Estrae il constraint CHECK"""
        pattern = r'CHECK\s*\((.*?)\)'
        match = re.search(pattern, constraints, re.IGNORECASE)
        
        if match:
            return match.group(1)
        
        return None
    
    def _parse_foreign_key(self, table_name: str, line: str):
        """Parsa una definizione di FOREIGN KEY"""
        pattern = r'FOREIGN\s+KEY\s*\((\w+)\)\s*REFERENCES\s+(\w+)\s*\((\w+)\)'
        match = re.search(pattern, line, re.IGNORECASE)
        
        if match:
            fk = {
                'table': table_name,
                'column': match.group(1),
                'referenced_table': match.group(2).upper(),
                'referenced_column': match.group(3)
            }
            
            if table_name not in self.foreign_keys:
                self.foreign_keys[table_name] = []
            
            self.foreign_keys[table_name].append(fk)
            
            # Aggiungi anche alla definizione della tabella
            if table_name in self.tables:
                self.tables[table_name]['foreign_keys'].append(fk)
    
    def _find_primary_key(self, columns: List[Dict], content: str) -> List[str]:
        """Trova la primary key"""
        pk_columns = []
        
        # Cerca nelle definizioni delle colonne
        for col in columns:
            if col.get('primary_key'):
                pk_columns.append(col['name'])
        
        # Cerca constraint PRIMARY KEY a livello tabella
        if not pk_columns:
            pattern = r'PRIMARY\s+KEY\s*\(([^)]+)\)'
            match = re.search(pattern, content, re.IGNORECASE)
            if match:
                pk_cols = match.group(1).split(',')
                pk_columns = [col.strip() for col in pk_cols]
        
        return pk_columns
    
    def _extract_indexes(self, sql: str):
        """Estrae le definizioni degli indici"""
        pattern = r'CREATE\s+(?:UNIQUE\s+)?INDEX\s+(\w+)\s+ON\s+(\w+)\s*\(([^)]+)\)'
        
        for match in re.finditer(pattern, sql, re.IGNORECASE):
            index_name = match.group(1)
            table_name = match.group(2).upper()
            columns = [col.strip() for col in match.group(3).split(',')]
            
            if table_name not in self.indexes:
                self.indexes[table_name] = []
            
            self.indexes[table_name].append({
                'name': index_name,
                'columns': columns,
                'unique': 'UNIQUE' in match.group(0).upper()
            })
    
    def _to_camel_case(self, snake_str: str) -> str:
        """Converte snake_case in camelCase"""
        components = snake_str.lower().split('_')
        return components[0] + ''.join(x.title() for x in components[1:])
    
    def get_table_info(self, table_name: str) -> Dict[str, Any]:
        """Ottiene le informazioni di una specifica tabella"""
        return self.tables.get(table_name.upper(), None)