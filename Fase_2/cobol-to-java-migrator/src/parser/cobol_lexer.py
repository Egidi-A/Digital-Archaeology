"""
COBOL Lexer - Tokenizzatore per codice COBOL
Supporta COBOL-85 con estensioni SQL embedded
"""

import ply.lex as lex
from loguru import logger


class CobolLexer:
    """Lexer per il linguaggio COBOL"""
    
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
        
        # Data types
        'PIC': 'PIC',
        'PICTURE': 'PICTURE',
        'VALUE': 'VALUE',
        'OCCURS': 'OCCURS',
        'TIMES': 'TIMES',
        'REDEFINES': 'REDEFINES',
        'COMP': 'COMP',
        'COMPUTATIONAL': 'COMPUTATIONAL',
        'BINARY': 'BINARY',
        'PACKED-DECIMAL': 'PACKED_DECIMAL',
        'ZERO': 'ZERO',
        'ZEROS': 'ZEROS',
        'SPACE': 'SPACE',
        'SPACES': 'SPACES',
        'ALL': 'ALL',
        
        # SQL
        'EXEC': 'EXEC',
        'SQL': 'SQL',
        'END-EXEC': 'END_EXEC',
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
        'DESC': 'DESC',
        
        
        # File operations
        'ASSIGN': 'ASSIGN',
        'TO': 'TO',
        'ORGANIZATION': 'ORGANIZATION',
        'IS': 'IS',
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
        'COLON': 'COLON',
        'LPAREN': 'LPAREN',
        'RPAREN': 'RPAREN',
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
    ] + list(reserved.values())
    
    # Token semplici
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
    
    # Ignora spazi e tab
    t_ignore = ' \t'
    
    def __init__(self):
        self.lexer = None
        self.in_sql_block = False
        self.sql_buffer = []
        
    def t_COMMENT(self, t):
        r'\*>.*'
        pass  # Ignora i commenti
        
    def t_COMMENT_OLD(self, t):
        r'^\s{6}\*.*$'
        pass  # Ignora i commenti vecchio stile (colonna 7)
    
    def t_EXEC_SQL(self, t):
        r'EXEC\s+SQL'
        self.in_sql_block = True
        t.type = 'EXEC'
        return t
    
    def t_END_EXEC(self, t):
        r'END-EXEC'
        if self.in_sql_block:
            self.in_sql_block = False
            # Processa il buffer SQL accumulato
            if self.sql_buffer:
                sql_token = lex.LexToken()
                sql_token.type = 'SQL_CODE'
                sql_token.value = ' '.join(self.sql_buffer)
                sql_token.lineno = t.lineno
                sql_token.lexpos = t.lexpos
                self.sql_buffer = []
        t.type = 'END_EXEC'
        return t
    
    def t_HOST_VARIABLE(self, t):
        r':[\w-]+'
        t.value = t.value[1:]  # Rimuove il :
        return t
    
    def t_STRING_LITERAL(self, t):
        r'"[^"]*"|\'[^\']*\''
        t.value = t.value[1:-1]  # Rimuove le virgolette
        return t
    
    def t_PICTURE_STRING(self, t):
        r'(PIC|PICTURE)\s+IS\s+[XAZS9V\(\)\-\+\$\*\,\.]+|(PIC|PICTURE)\s+[XAZS9V\(\)\-\+\$\*\,\.]+'
        # Estrae solo la parte della picture clause
        parts = t.value.split()
        if 'IS' in parts:
            t.value = parts[parts.index('IS') + 1]
        else:
            t.value = parts[1]
        return t
    
    def t_LEVEL_NUMBER(self, t):
        r'^\s*(0[1-9]|[1-4][0-9]|77|78|88)\s+'
        t.value = t.value.strip()
        return t
    
    def t_NUMBER(self, t):
        r'\d+(\.\d+)?'
        if '.' in t.value:
            t.value = float(t.value)
        else:
            t.value = int(t.value)
        return t
    
    def t_IDENTIFIER(self, t):
        r'[A-Za-z][A-Za-z0-9\-]*(?:\([0-9]+:[0-9]+\))?'
        # Converte in maiuscolo per confronto
        upper_value = t.value.upper().replace('_', '-')
        
        # Se siamo in un blocco SQL, accumula il codice
        if self.in_sql_block and upper_value not in ['END-EXEC']:
            self.sql_buffer.append(t.value)
            return None
        
        # Controlla se è una parola riservata
        t.type = self.reserved.get(upper_value, 'IDENTIFIER')
        return t
    
    def t_newline(self, t):
        r'\n+'
        t.lexer.lineno += len(t.value)
    
    def t_error(self, t):
        logger.warning(f"Carattere illegale '{t.value[0]}' alla linea {t.lineno}")
        t.lexer.skip(1)
    
    def build(self, **kwargs):
        """Costruisce il lexer"""
        self.lexer = lex.lex(module=self, **kwargs)
        return self.lexer
    
    def tokenize(self, data):
        """Tokenizza il codice COBOL"""
        self.lexer.input(data)
        tokens = []
        while True:
            tok = self.lexer.token()
            if not tok:
                break
            tokens.append(tok)
        return tokens
    
    def test(self, data):
        """Test del lexer con output dei token"""
        self.lexer.input(data)
        for tok in iter(self.lexer.token, None):
            print(f"{tok.type:15} {tok.value:30} [Linea {tok.lineno}]")