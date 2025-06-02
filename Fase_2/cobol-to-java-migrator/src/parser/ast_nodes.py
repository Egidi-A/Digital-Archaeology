"""
AST (Abstract Syntax Tree) nodes for COBOL parsing
"""

from dataclasses import dataclass, field
from typing import List, Optional, Dict, Any
from enum import Enum


class NodeType(Enum):
    """Tipi di nodi nell'AST"""
    PROGRAM = "PROGRAM"
    DIVISION = "DIVISION"
    SECTION = "SECTION"
    PARAGRAPH = "PARAGRAPH"
    DATA_ITEM = "DATA_ITEM"
    STATEMENT = "STATEMENT"
    SQL_STATEMENT = "SQL_STATEMENT"
    FILE_DEFINITION = "FILE_DEFINITION"
    PROCEDURE = "PROCEDURE"


@dataclass
class ASTNode:
    """Nodo base dell'AST"""
    node_type: NodeType
    name: str
    line_number: int = 0
    children: List['ASTNode'] = field(default_factory=list)
    attributes: Dict[str, Any] = field(default_factory=dict)
    
    def add_child(self, child: 'ASTNode'):
        """Aggiunge un nodo figlio"""
        self.children.append(child)
    
    def get_attribute(self, key: str, default=None):
        """Ottiene un attributo del nodo"""
        return self.attributes.get(key, default)
    
    def set_attribute(self, key: str, value: Any):
        """Imposta un attributo del nodo"""
        self.attributes[key] = value


@dataclass
class ProgramNode(ASTNode):
    """Nodo per PROGRAM-ID"""
    def __init__(self, program_id: str, line_number: int = 0):
        super().__init__(
            node_type=NodeType.PROGRAM,
            name=program_id,
            line_number=line_number
        )
        self.program_id = program_id
        self.author = None
        self.date_written = None


@dataclass
class DivisionNode(ASTNode):
    """Nodo per le divisioni COBOL"""
    def __init__(self, division_name: str, line_number: int = 0):
        super().__init__(
            node_type=NodeType.DIVISION,
            name=division_name,
            line_number=line_number
        )
        self.division_name = division_name


@dataclass
class DataItemNode(ASTNode):
    """Nodo per gli item di dati (DATA DIVISION)"""
    def __init__(self, level: str, name: str, line_number: int = 0):
        super().__init__(
            node_type=NodeType.DATA_ITEM,
            name=name,
            line_number=line_number
        )
        self.level = level
        self.picture = None
        self.value = None
        self.occurs = None
        self.redefines = None
        self.data_type = None  # Tipo derivato da PIC
        self.size = None
        self.decimals = None
        self.signed = False


@dataclass
class SqlStatementNode(ASTNode):
    """Nodo per statement SQL embedded"""
    def __init__(self, sql_type: str, line_number: int = 0):
        super().__init__(
            node_type=NodeType.SQL_STATEMENT,
            name=f"SQL_{sql_type}",
            line_number=line_number
        )
        self.sql_type = sql_type  # SELECT, INSERT, UPDATE, DELETE, etc.
        self.sql_text = ""
        self.host_variables = []  # Variabili COBOL usate nell'SQL
        self.into_variables = []  # Variabili per INTO clause
        self.table_name = None
        self.is_cursor = False
        self.cursor_name = None


@dataclass
class ProcedureNode(ASTNode):
    """Nodo per procedure/paragrafi"""
    def __init__(self, name: str, line_number: int = 0):
        super().__init__(
            node_type=NodeType.PROCEDURE,
            name=name,
            line_number=line_number
        )
        self.performs = []  # Lista di procedure chiamate
        self.called_by = []  # Lista di procedure che chiamano questa
        self.statements = []  # Statement contenuti


@dataclass
class StatementNode(ASTNode):
    """Nodo per statement COBOL generico"""
    def __init__(self, statement_type: str, line_number: int = 0):
        super().__init__(
            node_type=NodeType.STATEMENT,
            name=statement_type,
            line_number=line_number
        )
        self.statement_type = statement_type  # MOVE, DISPLAY, ACCEPT, etc.
        self.operands = []
        self.conditions = []


@dataclass
class FileDefinitionNode(ASTNode):
    """Nodo per definizioni file"""
    def __init__(self, file_name: str, line_number: int = 0):
        super().__init__(
            node_type=NodeType.FILE_DEFINITION,
            name=file_name,
            line_number=line_number
        )
        self.file_name = file_name
        self.organization = None
        self.access_mode = None
        self.record_name = None
        self.assign_to = None