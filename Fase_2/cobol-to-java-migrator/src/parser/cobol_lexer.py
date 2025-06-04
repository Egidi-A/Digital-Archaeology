"""
COBOL Lexer - Tokenizzatore per codice COBOL
Supporta COBOL-85 con estensioni SQL embedded
"""

import ply.lex as lex
from loguru import logger


class CobolLexer:
    """Lexer per il linguaggio COBOL"""
    
    # Stati per gestire SQL embedded
    states = (
        ('sql', 'exclusive'),
    )
    
    # Token riservati COBOL
    reserved = {
        # Divisions
        'IDENTIFICATION': 'IDENTIFICATION',
        'ENVIRONMENT': 'ENVIRONMENT',
        'DATA': 'DATA',
        'PROCEDURE': 'PROCEDURE',
        'DIVISION': 'DIVISION',
        
        # Sections
        'PROGRAM-ID': 'PROGRAM_ID',
        'AUTHOR': 'AUTHOR',
        'DATE-WRITTEN': 'DATE_WRITTEN',
        'FILE-CONTROL': 'FILE_CONTROL',
        'INPUT-OUTPUT': 'INPUT_OUTPUT',
        'SECTION': 'SECTION',
        'WORKING-STORAGE': 'WORKING_STORAGE',
        'FILE': 'FILE',
        'FD': 'FD',
        
        # Statements
        'PERFORM': 'PERFORM',
        'MOVE': 'MOVE',
        'DISPLAY': 'DISPLAY',
        'ACCEPT': 'ACCEPT',
        'OPEN': 'OPEN',
        'CLOSE': 'CLOSE',
        'READ': 'READ',
        'WRITE': 'WRITE',
        'IF': 'IF',
        'ELSE': 'ELSE',
        'END-IF': 'END_IF',
        'EVALUATE': 'EVALUATE',
        'WHEN': 'WHEN',
        'END-EVALUATE': 'END_EVALUATE',
        'COMPUTE': 'COMPUTE',
        'ADD': 'ADD',
        'SUBTRACT': 'SUBTRACT',
        'MULTIPLY': 'MULTIPLY',
        'DIVIDE': 'DIVIDE',
        'STOP': 'STOP',
        'RUN': 'RUN',
        'EXIT': 'EXIT',
        'PARAGRAPH': 'PARAGRAPH',
        'UNTIL': 'UNTIL',
        'VARYING': 'VARYING',
        'FROM': 'FROM',
        'BY': 'BY',
        'GIVING': 'GIVING',
        'RETURNING': 'RETURNING',
        'CALL': 'CALL',
        'CONTINUE': 'CONTINUE',
        'THRU': 'THRU',
        'THROUGH': 'THROUGH',
        'THEN': 'THEN',
        'END': 'END',
        'END-READ': 'END_READ',
        'AT': 'AT',
        'OF': 'OF',
        'IN': 'IN',
        'ROUNDED': 'ROUNDED',
        
        # Data types
        'PIC': 'PIC',
        'PICTURE': 'PICTURE',
        'VALUE': 'VALUE',
        'VALUES': 'VALUES',
        'OCCURS': 'OCCURS',
        'TIMES': 'TIMES',
        'REDEFINES': 'REDEFINES',
        'COMP': 'COMP',
        'COMPUTATIONAL': 'COMPUTATIONAL',
        'BINARY': 'BINARY',
        'PACKED-DECIMAL': 'PACKED_DECIMAL',
        'ZERO': 'ZERO',
        'ZEROS': 'ZEROS',
        'ZEROES': 'ZEROES',
        'SPACE': 'SPACE',
        'SPACES': 'SPACES',
        'ALL': 'ALL',
        'HIGH-VALUE': 'HIGH_VALUE',
        'HIGH-VALUES': 'HIGH_VALUES',
        'LOW-VALUE': 'LOW_VALUE',
        'LOW-VALUES': 'LOW_VALUES',
        'QUOTE': 'QUOTE',
        'QUOTES': 'QUOTES',
        
        # SQL base tokens
        'EXEC': 'EXEC',
        'SQL': 'SQL',
        'END-EXEC': 'END_EXEC',
        
        # File operations
        'ASSIGN': 'ASSIGN',
        'TO': 'TO',
        'ORGANIZATION': 'ORGANIZATION',
        'IS': 'IS',
        'ARE': 'ARE',
        'LINE': 'LINE',
        'SEQUENTIAL': 'SEQUENTIAL',
        'RANDOM': 'RANDOM',
        'DYNAMIC': 'DYNAMIC',
        'ACCESS': 'ACCESS',
        'MODE': 'MODE',
        'RECORD': 'RECORD',
        'KEY': 'KEY',
        'STATUS': 'STATUS',
        'INPUT': 'INPUT',
        'OUTPUT': 'OUTPUT',
        'I-O': 'I_O',
        'EXTEND': 'EXTEND',
        
        # Logical operators
        'AND': 'AND',
        'OR': 'OR',
        'NOT': 'NOT',
        'TRUE': 'TRUE',
        'FALSE': 'FALSE',
        
        # Comparison
        'EQUAL': 'EQUAL',
        'EQUALS': 'EQUALS',
        'GREATER': 'GREATER',
        'LESS': 'LESS',
        'THAN': 'THAN',
        
        # Others
        'WITH': 'WITH',
        'NO': 'NO',
        'ADVANCING': 'ADVANCING',
        'DELIMITED': 'DELIMITED',
        'SIZE': 'SIZE',
        'STRING': 'STRING',
        'UNSTRING': 'UNSTRING',
        'INSPECT': 'INSPECT',
        'REPLACING': 'REPLACING',
        'OTHER': 'OTHER',
        'FILLER': 'FILLER',
        'FUNCTION': 'FUNCTION',
        'CURRENT-DATE': 'CURRENT_DATE',
        'LENGTH': 'LENGTH',
        'LPAD': 'LPAD',
        'CAST': 'CAST',
        'AS': 'AS',
        'MAX': 'MAX',
        'MIN': 'MIN',
        'COUNT': 'COUNT',
        'SUM': 'SUM',
        'AVG': 'AVG',
        'COALESCE': 'COALESCE',
        'SUBSTR': 'SUBSTR',
        'INTEGER': 'INTEGER',
        'VARCHAR': 'VARCHAR',
        'CURRENT_DATE': 'CURRENT_DATE_FUNC',
        'CURRENT_TIMESTAMP': 'CURRENT_TIMESTAMP',
        'NOW': 'NOW',
        'LIKE': 'LIKE',
        'EXISTS': 'EXISTS',
        'BETWEEN': 'BETWEEN',
        'NULL': 'NULL',
        'JOIN': 'JOIN',
        'INNER': 'INNER',
        'LEFT': 'LEFT', 
        'RIGHT': 'RIGHT',
        'FULL': 'FULL',
        'ON': 'ON',
        'ASC': 'ASC',
        'DESC': 'DESC',
        'CURRENT': 'CURRENT',
        'SQLCODE': 'SQLCODE',
        'SQLCA': 'SQLCA',
    }
    
    # Token per lo stato SQL
    sql_reserved = {
        'SELECT': 'SELECT',
        'INSERT': 'INSERT',
        'UPDATE': 'UPDATE',
        'DELETE': 'DELETE',
        'INTO': 'INTO',
        'VALUES': 'VALUES',
        'WHERE': 'WHERE',
        'SET': 'SET',
        'DECLARE': 'DECLARE',
        'CURSOR': 'CURSOR',
        'FOR': 'FOR',
        'FETCH': 'FETCH',
        'CONNECT': 'CONNECT',
        'DISCONNECT': 'DISCONNECT',
        'COMMIT': 'COMMIT',
        'ROLLBACK': 'ROLLBACK',
        'SQLCODE': 'SQLCODE',
        'INCLUDE': 'INCLUDE',
        'SQLCA': 'SQLCA',
        'USER': 'USER',
        'USING': 'USING',
        'ORDER': 'ORDER',
        'BY': 'BY',
        'DESC': 'DESC',
        'ASC': 'ASC',
        'FROM': 'FROM',
        'JOIN': 'JOIN',
        'ON': 'ON',
        'AND': 'AND',
        'OR': 'OR',
        'NOT': 'NOT',
        'NULL': 'NULL',
        'IS': 'IS',
        'LIKE': 'LIKE',
        'IN': 'IN',
        'EXISTS': 'EXISTS',
        'BETWEEN': 'BETWEEN',
        'CURRENT_DATE': 'CURRENT_DATE',
        'CURRENT_TIMESTAMP': 'CURRENT_TIMESTAMP',
        'ALL': 'ALL',
    }

    # Lista di tutti i token
    tokens = [
        'NUMBER',
        'IDENTIFIER',
        'STRING_LITERAL',
        'PICTURE_STRING',
        'LEVEL_NUMBER',
        'PERIOD',
        'COMMA',
        'SEMICOLON',
        'COLON',
        'LPAREN',
        'RPAREN',
        'PLUS',
        'MINUS',
        'MULTIPLY_OP',
        'DIVIDE_OP',
        'EQUALS',
        'GREATER_OP',
        'LESS_OP',
        'NOT_EQUAL',
        'GREATER_EQUAL',
        'LESS_EQUAL',
        'COMMENT',
        'SQL_CODE',
        'HOST_VARIABLE',
        'UNDERSCORE',
        'CONCATENATE',
    ] + [v for k, v in reserved.items()] + [v for k, v in sql_reserved.items()]
    # + [v for k, v in reserved.items() if k == v]
    # + [v for k, v in reserved.items() if k != v]
    # + [v for k, v in sql_reserved.items() if k == v]
    # + [v for k, v in sql_reserved.items() if k != v]
    
    # Token semplici (stato INITIAL)
    t_PERIOD = r'\.'
    t_COMMA = r','
    t_SEMICOLON = r';'
    t_COLON = r':'
    t_LPAREN = r'\('
    t_RPAREN = r'\)'
    t_PLUS = r'\+'
    t_MINUS = r'-'
    t_MULTIPLY_OP = r'\*'
    t_DIVIDE_OP = r'/'
    t_EQUALS = r'='
    t_GREATER_OP = r'>'
    t_LESS_OP = r'<'
    t_NOT_EQUAL = r'<>'
    t_GREATER_EQUAL = r'>='
    t_LESS_EQUAL = r'<='
    t_UNDERSCORE = r'_'
    t_CONCATENATE = r'\|\|'
    
    # Token semplici (stato SQL)
    t_sql_PERIOD = r'\.'
    t_sql_COMMA = r','
    t_sql_SEMICOLON = r';'
    t_sql_COLON = r':'
    t_sql_LPAREN = r'\('
    t_sql_RPAREN = r'\)'
    t_sql_PLUS = r'\+'
    t_sql_MINUS = r'-'
    t_sql_MULTIPLY_OP = r'\*'
    t_sql_DIVIDE_OP = r'/'
    t_sql_EQUALS = r'='
    t_sql_GREATER_OP = r'>'
    t_sql_LESS_OP = r'<'
    t_sql_NOT_EQUAL = r'<>'
    t_sql_GREATER_EQUAL = r'>='
    t_sql_LESS_EQUAL = r'<='
    t_sql_UNDERSCORE = r'_'
    t_sql_CONCATENATE = r'\|\|'
    
    # Ignora spazi e tab
    t_ignore = ' \t'
    t_sql_ignore = ' \t'
    
    def __init__(self):
        self.lexer = None
    
    def t_COMMENT(self, t):
        r'\*>.*'
        pass  # Ignora i commenti
        
    def t_COMMENT_OLD(self, t):
        r'^\s{6}\*.*$'
        pass  # Ignora i commenti vecchio stile (colonna 7)
    
    def t_EXEC_SQL(self, t):
        r'EXEC\s+SQL'
        t.lexer.begin('sql')
        t.type = 'EXEC'
        return t
    
    def t_sql_END_EXEC(self, t):
        r'END-EXEC'
        t.lexer.begin('INITIAL')
        t.type = 'END_EXEC'
        return t
    
    def t_HOST_VARIABLE(self, t):
        r':[\w-]+'
        t.value = t.value[1:]  # Rimuove il :
        return t
    
    def t_sql_HOST_VARIABLE(self, t):
        r':[\w-]+'
        t.value = t.value[1:]  # Rimuove il :
        return t
    
    def t_STRING_LITERAL(self, t):
        r'"[^"]*"|\'[^\']*\''
        t.value = t.value[1:-1]  # Rimuove le virgolette
        return t
    
    def t_sql_STRING_LITERAL(self, t):
        r'"[^"]*"|\'[^\']*\''
        t.value = t.value[1:-1]  # Rimuove le virgolette
        return t
    
    def t_PICTURE_STRING(self, t):
        r'(PIC|PICTURE)\s+(IS\s+)?[XAZS9V](\([0-9]+\))?[XAZS9V\-\+\$\*\,\.]*(\([0-9]+\))?'
        # Estrae solo la parte della picture clause
        parts = t.value.split()
        if 'IS' in parts:
            idx = parts.index('IS') + 1
        else:
            idx = 1
        if idx < len(parts):
            t.value = parts[idx]
        return t
    
    def t_LEVEL_NUMBER(self, t):
        r'(0[1-9]|[1-4][0-9]|77|78|88)\s+'
        t.value = t.value.strip()
        return t
    
    def t_NUMBER(self, t):
        r'\d+(\.\d+)?'
        if '.' in t.value:
            t.value = float(t.value)
        else:
            t.value = int(t.value)
        return t
    
    def t_sql_NUMBER(self, t):
        r'\d+(\.\d+)?'
        if '.' in t.value:
            t.value = float(t.value)
        else:
            t.value = int(t.value)
        return t
    
    def t_IDENTIFIER(self, t):
        r'[A-Za-z][A-Za-z0-9\-_]*(?:\([0-9]+(?::[0-9]+)?\))?'
        # Normalizza identificatori sostituendo underscore con trattini per COBOL
        normalized = t.value.upper()
        
        # Controlla se è una parola riservata
        if normalized in self.reserved:
            t.type = self.reserved[normalized]
        else:
            # Mantieni underscore per variabili come WS_SCELTA
            t.type = 'IDENTIFIER'
            
        return t
    
    def t_sql_IDENTIFIER(self, t):
        r'[A-Za-z_][A-Za-z0-9_]*'
        # In SQL, gli identificatori possono avere underscore
        upper_value = t.value.upper()
        
        # Controlla se è una parola riservata SQL
        if upper_value in self.sql_reserved:
            t.type = self.sql_reserved[upper_value]
        else:
            t.type = 'IDENTIFIER'
            
        return t
    
    # Speciale per SQL_CODE
    def t_sql_SQL_CODE(self, t):
        r'[^;]+'
        # Accumula tutto il codice SQL fino al prossimo END-EXEC
        # Questo è gestito dal parser
        return t
    
    def t_newline(self, t):
        r'\n+'
        t.lexer.lineno += len(t.value)
    
    def t_sql_newline(self, t):
        r'\n+'
        t.lexer.lineno += len(t.value)
    
    def t_error(self, t):
        logger.warning(f"Carattere illegale '{t.value[0]}' alla linea {t.lineno}")
        t.lexer.skip(1)
    
    def t_sql_error(self, t):
        logger.warning(f"Carattere illegale in SQL '{t.value[0]}' alla linea {t.lineno}")
        t.lexer.skip(1)
    
    def build(self, **kwargs):
        """Costruisce il lexer"""
        self.lexer = lex.lex(module=self, **kwargs)
        return self.lexer
    
    def tokenize(self, data):
        """Tokenizza il codice COBOL"""
        # Preprocessa prima di tokenizzare
        preprocessed = self._preprocess_input(data)
        
        self.lexer.input(preprocessed)
        tokens = []
        while True:
            tok = self.lexer.token()
            if not tok:
                break
            tokens.append(tok)
        return tokens
    
    def _preprocess_input(self, data):
        """Preprocessa l'input per gestire casi speciali"""
        lines = data.split('\n')
        processed_lines = []
        
        for line in lines:
            # Gestisci underscore in identificatori COBOL
            # Gli underscore sono validi in COBOL moderno
            processed_line = line
            
            # Non modificare underscore in stringhe o commenti
            if not (line.strip().startswith('*') or '*>' in line):
                # Mantieni gli underscore negli identificatori
                pass
            
            processed_lines.append(processed_line)
        
        return '\n'.join(processed_lines)
    
    def test(self, data):
        """Test del lexer con output dei token"""
        self.lexer.input(data)
        for tok in iter(self.lexer.token, None):
            print(f"{tok.type:15} {tok.value:30} [Linea {tok.lineno}]")