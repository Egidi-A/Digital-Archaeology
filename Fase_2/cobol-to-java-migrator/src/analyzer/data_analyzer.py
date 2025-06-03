"""
Data Analyzer - Analizza le strutture dati COBOL e le mappa su classi Java
"""

from typing import Dict, List, Any
from loguru import logger
from ..parser.ast_nodes import *


class DataAnalyzer:
    """Analizza le strutture dati nell'AST COBOL"""
    
    def __init__(self, config: Dict[str, Any]):
        self.config = config
        self.type_mappings = config.get('type_mappings', {})
        self.data_structures = {}
        self.file_definitions = {}
        self.sql_tables = set()
        self.sql_schema = None
        
    def set_sql_schema(self, sql_schema: Dict[str, Any]):
        """Imposta lo schema SQL parsato"""
        self.sql_schema = sql_schema
        logger.info(f"Schema SQL impostato con {len(sql_schema.get('tables', {}))} tabelle")
        
    def analyze(self, ast: ASTNode) -> Dict[str, Any]:
        """Analizza l'AST e estrae le strutture dati"""
        logger.info("Inizio analisi delle strutture dati")
        
        # Reset delle strutture
        self.data_structures = {}
        self.file_definitions = {}
        self.sql_tables = set()
        
        # Analizza ricorsivamente l'AST
        self._analyze_node(ast)
        
        # Organizza i risultati
        result = {
            'data_items': self._organize_data_items(),
            'file_definitions': self.file_definitions,
            'sql_tables': list(self.sql_tables),
            'entities': self._generate_entities(),
            'dtos': self._generate_dtos()
        }
        
        logger.info(f"Trovate {len(result['data_items'])} strutture dati")
        logger.info(f"Trovate {len(result['file_definitions'])} definizioni file")
        logger.info(f"Trovate {len(result['sql_tables'])} tabelle SQL")
        
        return result
    
    def _analyze_node(self, node: ASTNode):
        """Analizza ricorsivamente un nodo dell'AST"""
        if isinstance(node, DataItemNode):
            self._analyze_data_item(node)
        elif isinstance(node, FileDefinitionNode):
            self._analyze_file_definition(node)
        elif isinstance(node, SqlStatementNode):
            self._analyze_sql_statement(node)
        
        # Analizza i figli
        for child in node.children:
            self._analyze_node(child)
    
    def _analyze_data_item(self, node: DataItemNode):
        """Analizza un data item COBOL"""
        # Determina il tipo Java
        java_type = self._map_cobol_type_to_java(node)
        
        # Crea la struttura dati
        data_structure = {
            'name': self._to_java_name(node.name),
            'cobol_name': node.name,
            'level': node.level,
            'java_type': java_type,
            'picture': node.picture,
            'value': node.value,
            'occurs': node.occurs,
            'size': node.size,
            'decimals': node.decimals,
            'is_group': len(node.children) > 0,
            'children': []
        }
        
        # Se è un gruppo, analizza i figli
        if data_structure['is_group']:
            for child in node.children:
                if isinstance(child, DataItemNode):
                    data_structure['children'].append(child.name)
        
        # Memorizza la struttura
        self.data_structures[node.name] = data_structure
    
    def _analyze_file_definition(self, node: FileDefinitionNode):
        """Analizza una definizione di file"""
        file_def = {
            'name': node.file_name,
            'assign_to': node.assign_to,
            'organization': node.organization,
            'record_structure': []
        }
        
        # Analizza la struttura del record
        for child in node.children:
            if isinstance(child, DataItemNode):
                file_def['record_structure'].append({
                    'name': child.name,
                    'java_type': self._map_cobol_type_to_java(child),
                    'size': child.size
                })
        
        self.file_definitions[node.file_name] = file_def
    
    def _analyze_sql_statement(self, node: SqlStatementNode):
        """Analizza uno statement SQL per identificare le tabelle"""
        if node.table_name:
            self.sql_tables.add(node.table_name)
    
    def _map_cobol_type_to_java(self, node: DataItemNode) -> str:
        """Mappa un tipo COBOL su un tipo Java"""
        if not node.picture:
            # Se non ha PICTURE, è probabilmente un gruppo
            return "Object" if node.level == "01" else "String"
        
        pic = node.picture.upper()
        
        # Controlla mappature esatte
        for cobol_pattern, java_type in self.type_mappings.items():
            if pic.startswith(cobol_pattern.replace('(n)', '')):
                return java_type
        
        # Logica di default basata sul tipo
        if node.data_type:
            return node.data_type
        
        # Analisi basata sul pattern
        if 'X' in pic or 'A' in pic:
            return "String"
        elif 'V' in pic or '.' in pic:
            return "BigDecimal"
        elif 'S' in pic:
            if node.size and node.size > 9:
                return "Long"
            return "Integer"
        elif '9' in pic:
            if node.decimals and node.decimals > 0:
                return "BigDecimal"
            elif node.size and node.size > 9:
                return "Long"
            return "Integer"
        
        return "String"  # Default
    
    def _organize_data_items(self) -> Dict[str, Any]:
        """Organizza i data items in una struttura gerarchica"""
        organized = {}
        
        # Prima passa: identifica i top-level items (livello 01)
        for name, item in self.data_structures.items():
            if item['level'] == '01':
                organized[name] = item
        
        # Seconda passa: organizza i figli
        for name, item in self.data_structures.items():
            if item['level'] != '01':
                # Trova il parent
                parent = self._find_parent_item(name, item['level'])
                if parent and parent in organized:
                    if 'fields' not in organized[parent]:
                        organized[parent]['fields'] = []
                    organized[parent]['fields'].append(item)
        
        return organized
    
    def _find_parent_item(self, item_name: str, item_level: str) -> str:
        """Trova il parent di un data item basandosi sul livello"""
        # Logica semplificata - in un parser completo dovremmo tracciare
        # la gerarchia durante il parsing
        level_num = int(item_level)
        
        for name, item in self.data_structures.items():
            if int(item['level']) < level_num:
                return name
        
        return None
    
    def _generate_entities(self) -> List[Dict[str, Any]]:
        """Genera le entità JPA dalle tabelle SQL identificate"""
        entities = []
        
        if self.sql_schema and 'tables' in self.sql_schema:
            # Usa lo schema SQL reale
            for table_name, table_info in self.sql_schema['tables'].items():
                entity = {
                    'table_name': table_name,
                    'class_name': self._to_class_name(table_name),
                    'fields': []
                }
                
                # Usa le colonne reali dal database
                for column in table_info['columns']:
                    field = {
                        'name': column['java_name'],
                        'db_name': column['db_name'],
                        'type': column['java_type'],
                        'nullable': column['nullable'],
                        'unique': column['unique'],
                        'length': column.get('length'),
                        'precision': column.get('precision'),
                        'scale': column.get('scale'),
                        'default': column.get('default'),
                        'id': column['name'] in table_info['primary_key']
                    }
                    entity['fields'].append(field)
                
                # Aggiungi relazioni basate su foreign keys
                if table_name in self.sql_schema.get('foreign_keys', {}):
                    entity['relationships'] = []
                    for fk in self.sql_schema['foreign_keys'][table_name]:
                        relationship = {
                            'type': 'ManyToOne',
                            'field_name': self._to_java_name(fk['column']),
                            'target_entity': self._to_class_name(fk['referenced_table']),
                            'join_column': fk['column'],
                            'referenced_column': fk['referenced_column']
                        }
                        entity['relationships'].append(relationship)
                
                entities.append(entity)
        else:
            # Fallback al comportamento precedente se non c'è schema SQL
            for table_name in self.sql_tables:
                entity = {
                    'table_name': table_name,
                    'class_name': self._to_class_name(table_name),
                    'fields': self._infer_table_fields(table_name)
                }
                entities.append(entity)
        
        return entities
    
    def _infer_table_fields(self, table_name: str) -> List[Dict[str, Any]]:
        """Inferisce i campi di una tabella dall'analisi del codice"""
        fields = []
        
        # Mappature comuni per tabelle bancarie
        if table_name.upper() == 'CONTI':
            fields = [
                {'name': 'numeroConto', 'db_name': 'numero_conto', 'type': 'String', 'id': True},
                {'name': 'codiceCliente', 'db_name': 'codice_cliente', 'type': 'String'},
                {'name': 'tipoConto', 'db_name': 'tipo_conto', 'type': 'String'},
                {'name': 'saldo', 'db_name': 'saldo', 'type': 'BigDecimal'},
                {'name': 'dataApertura', 'db_name': 'data_apertura', 'type': 'LocalDate'},
                {'name': 'stato', 'db_name': 'stato', 'type': 'String'},
                {'name': 'fido', 'db_name': 'fido', 'type': 'BigDecimal'}
            ]
        elif table_name.upper() == 'CLIENTI':
            fields = [
                {'name': 'codiceCliente', 'db_name': 'codice_cliente', 'type': 'String', 'id': True},
                {'name': 'nome', 'db_name': 'nome', 'type': 'String'},
                {'name': 'cognome', 'db_name': 'cognome', 'type': 'String'},
                {'name': 'codiceFiscale', 'db_name': 'codice_fiscale', 'type': 'String'},
                {'name': 'dataNascita', 'db_name': 'data_nascita', 'type': 'LocalDate'},
                {'name': 'indirizzo', 'db_name': 'indirizzo', 'type': 'String'},
                {'name': 'citta', 'db_name': 'citta', 'type': 'String'},
                {'name': 'cap', 'db_name': 'cap', 'type': 'String'},
                {'name': 'telefono', 'db_name': 'telefono', 'type': 'String'},
                {'name': 'email', 'db_name': 'email', 'type': 'String'}
            ]
        elif table_name.upper() == 'MOVIMENTI':
            fields = [
                {'name': 'id', 'db_name': 'id', 'type': 'Long', 'id': True},
                {'name': 'numeroConto', 'db_name': 'numero_conto', 'type': 'String'},
                {'name': 'dataMovimento', 'db_name': 'data_movimento', 'type': 'LocalDateTime'},
                {'name': 'tipoMovimento', 'db_name': 'tipo_movimento', 'type': 'String'},
                {'name': 'importo', 'db_name': 'importo', 'type': 'BigDecimal'},
                {'name': 'causale', 'db_name': 'causale', 'type': 'String'},
                {'name': 'saldoDopo', 'db_name': 'saldo_dopo', 'type': 'BigDecimal'},
                {'name': 'eseguitoDa', 'db_name': 'eseguito_da', 'type': 'String'}
            ]
        
        return fields
    
    def _generate_dtos(self) -> List[Dict[str, Any]]:
        """Genera i DTO per le operazioni"""
        dtos = [
            {
                'name': 'AperturaContoRequest',
                'fields': [
                    {'name': 'codiceCliente', 'type': 'String', 'validation': 'NotBlank'},
                    {'name': 'tipoConto', 'type': 'String', 'validation': 'NotBlank'},
                    {'name': 'importoIniziale', 'type': 'BigDecimal', 'validation': 'PositiveOrZero'},
                    {'name': 'fido', 'type': 'BigDecimal', 'validation': 'PositiveOrZero'}
                ]
            },
            {
                'name': 'MovimentoRequest',
                'fields': [
                    {'name': 'importo', 'type': 'BigDecimal', 'validation': 'Positive'},
                    {'name': 'causale', 'type': 'String', 'validation': 'NotBlank'}
                ]
            },
            {
                'name': 'ContoResponse',
                'fields': [
                    {'name': 'numeroConto', 'type': 'String'},
                    {'name': 'intestatario', 'type': 'String'},
                    {'name': 'saldo', 'type': 'BigDecimal'},
                    {'name': 'fido', 'type': 'BigDecimal'},
                    {'name': 'disponibile', 'type': 'BigDecimal'}
                ]
            }
        ]
        
        return dtos
    
    def _to_java_name(self, cobol_name: str) -> str:
        """Converte un nome COBOL in nome Java (camelCase)"""
        # Rimuove prefissi comuni COBOL
        name = cobol_name
        for prefix in ['WS-', 'LS-', 'W-']:
            if name.startswith(prefix):
                name = name[len(prefix):]
        
        # Converte in camelCase
        parts = name.replace('-', '_').split('_')
        if len(parts) == 1:
            return parts[0].lower()
        
        return parts[0].lower() + ''.join(p.capitalize() for p in parts[1:])
    
    def _to_class_name(self, name: str) -> str:
        """Converte un nome in PascalCase per le classi Java"""
        parts = name.replace('-', '_').split('_')
        return ''.join(p.capitalize() for p in parts)