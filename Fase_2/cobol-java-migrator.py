#!/usr/bin/env python3
"""
COBOL to Java Migration Tool
Transpiler per la conversione automatica di programmi COBOL in Java
Author: Annalisa Egidi
Version: 1.0
"""

import os
import re
import yaml
from dataclasses import dataclass
from typing import List, Dict, Optional, Tuple
from enum import Enum

# ==================== Data Structures ====================

class TokenType(Enum):
    """Token types per il lexer COBOL"""
    DIVISION = "DIVISION"
    SECTION = "SECTION"
    PARAGRAPH = "PARAGRAPH"
    KEYWORD = "KEYWORD"
    IDENTIFIER = "IDENTIFIER"
    NUMBER = "NUMBER"
    STRING = "STRING"
    PICTURE = "PICTURE"
    OPERATOR = "OPERATOR"
    DOT = "DOT"
    EXEC_SQL = "EXEC_SQL"
    END_EXEC = "END_EXEC"

@dataclass
class Token:
    """Rappresenta un token COBOL"""
    type: TokenType
    value: str
    line: int
    column: int

@dataclass
class DataItem:
    """Rappresenta un data item COBOL"""
    level: int
    name: str
    picture: Optional[str]
    value: Optional[str]
    occurs: Optional[int]
    children: List['DataItem']

@dataclass
class SQLStatement:
    """Rappresenta uno statement SQL embedded"""
    type: str  # SELECT, INSERT, UPDATE, DELETE, DECLARE CURSOR
    sql: str
    host_variables: List[str]
    cursor_name: Optional[str] = None

# ==================== Lexer ====================

class COBOLLexer:
    """Lexer per tokenizzare codice COBOL"""
    
    def __init__(self):
        self.keywords = {
            'IDENTIFICATION', 'ENVIRONMENT', 'DATA', 'PROCEDURE',
            'DIVISION', 'SECTION', 'PROGRAM-ID', 'AUTHOR',
            'WORKING-STORAGE', 'FILE', 'FD', 'PERFORM', 'MOVE',
            'DISPLAY', 'ACCEPT', 'IF', 'ELSE', 'END-IF',
            'EVALUATE', 'WHEN', 'END-EVALUATE', 'EXEC', 'SQL',
            'END-EXEC', 'COMPUTE', 'ADD', 'SUBTRACT', 'MULTIPLY',
            'DIVIDE', 'OPEN', 'CLOSE', 'READ', 'WRITE'
        }
        
        self.patterns = [
            (r'\*>.*$', None),  # Comments
            (r'EXEC\s+SQL', TokenType.EXEC_SQL),
            (r'END-EXEC', TokenType.END_EXEC),
            (r'PIC(TURE)?\s+[X9A-Z\(\)\-\.]+', TokenType.PICTURE),
            (r'\d+', TokenType.NUMBER),
            (r'"[^"]*"', TokenType.STRING),
            (r"'[^']*'", TokenType.STRING),
            (r'[A-Z][A-Z0-9\-]*', TokenType.IDENTIFIER),
            (r'\.', TokenType.DOT),
            (r'[=\+\-\*/\(\)]', TokenType.OPERATOR),
        ]
    
    def tokenize(self, code: str) -> List[Token]:
        """Tokenizza il codice COBOL"""
        tokens = []
        lines = code.split('\n')
        
        for line_num, line in enumerate(lines, 1):
            # Skip comment lines
            if line.strip().startswith('*'):
                continue
                
            # Process line starting from column 8 (COBOL standard)
            if len(line) > 7:
                line_content = line[7:]
            else:
                continue
            
            col = 8
            while col < len(line):
                # Skip whitespace
                if line[col-1].isspace():
                    col += 1
                    continue
                
                # Try to match patterns
                matched = False
                for pattern, token_type in self.patterns:
                    regex = re.compile(pattern, re.IGNORECASE)
                    match = regex.match(line[col-1:])
                    
                    if match:
                        value = match.group(0)
                        
                        # Check if it's a keyword
                        if token_type == TokenType.IDENTIFIER and value.upper() in self.keywords:
                            if value.upper() in ['IDENTIFICATION', 'ENVIRONMENT', 'DATA', 'PROCEDURE']:
                                token_type = TokenType.DIVISION
                            elif value.upper() in ['WORKING-STORAGE', 'FILE', 'INPUT-OUTPUT']:
                                token_type = TokenType.SECTION
                            else:
                                token_type = TokenType.KEYWORD
                        
                        if token_type:  # Skip None (comments)
                            tokens.append(Token(token_type, value, line_num, col))
                        
                        col += len(value)
                        matched = True
                        break
                
                if not matched:
                    col += 1
        
        return tokens

# ==================== Parser ====================

class COBOLParser:
    """Parser per costruire AST da token COBOL"""
    
    def __init__(self, tokens: List[Token]):
        self.tokens = tokens
        self.current = 0
        self.data_items = {}
        self.sql_statements = []
        self.procedures = {}
    
    def parse(self) -> Dict:
        """Parse principale che restituisce la struttura del programma"""
        ast = {
            'program_id': None,
            'author': None,
            'environment_division': {
                'configuration': {},
                'files': []
            },
            'data_division': {},
            'procedure_division': {},
            'sql_statements': []
        }
        
        while not self.is_at_end():
            if self.match(TokenType.DIVISION):
                division_name = self.previous().value.upper()
                
                if division_name == 'IDENTIFICATION':
                    self.parse_identification_division(ast)
                elif division_name == 'ENVIRONMENT':
                    self.parse_environment_division(ast)
                elif division_name == 'DATA':
                    self.parse_data_division(ast)
                elif division_name == 'PROCEDURE':
                    self.parse_procedure_division(ast)
            else:
                self.advance()
        
        ast['sql_statements'] = self.sql_statements
        return ast
    
    def parse_identification_division(self, ast: Dict):
        """Parse IDENTIFICATION DIVISION"""
        while not self.check_division():
            if self.match_keyword('PROGRAM-ID'):
                self.advance()  # Skip dot
                ast['program_id'] = self.advance().value
            elif self.match_keyword('AUTHOR'):
                self.advance()  # Skip dot
                author_parts = []
                while not self.check(TokenType.DOT):
                    author_parts.append(self.advance().value)
                ast['author'] = ' '.join(author_parts)
            else:
                self.advance()
    
    def parse_environment_division(self, ast: Dict):
        """Parse ENVIRONMENT DIVISION"""
        current_section = None
        current_file = None
        
        while not self.check_division():
            if self.match(TokenType.SECTION):
                section_name = self.previous().value.upper()
                if 'CONFIGURATION' in section_name:
                    current_section = 'configuration'
                elif 'INPUT-OUTPUT' in section_name:
                    current_section = 'input_output'
            elif current_section == 'configuration':
                if self.match_keyword('SOURCE-COMPUTER'):
                    self.advance()  # Skip dot
                    ast['environment_division']['configuration']['source_computer'] = self.advance().value
                elif self.match_keyword('OBJECT-COMPUTER'):
                    self.advance()  # Skip dot
                    ast['environment_division']['configuration']['object_computer'] = self.advance().value
                elif self.match_keyword('SPECIAL-NAMES'):
                    self.parse_special_names(ast)
                else:
                    self.advance()
            elif current_section == 'input_output':
                if self.match_keyword('FILE-CONTROL'):
                    continue
                elif self.match_keyword('SELECT'):
                    current_file = self.parse_file_select()
                    ast['environment_division']['files'].append(current_file)
                else:
                    self.advance()
            else:
                self.advance()
    
    def parse_special_names(self, ast: Dict):
        """Parse SPECIAL-NAMES paragraph"""
        if 'special_names' not in ast['environment_division']['configuration']:
            ast['environment_division']['configuration']['special_names'] = {}
        
        while not self.check(TokenType.DOT) and not self.check_division() and not self.check(TokenType.SECTION):
            if self.match_keyword('DECIMAL-POINT'):
                self.match_keyword('IS')
                if self.match_keyword('COMMA'):
                    ast['environment_division']['configuration']['special_names']['decimal_point'] = 'COMMA'
            elif self.match_keyword('CURRENCY'):
                self.match_keyword('SIGN')
                self.match_keyword('IS')
                currency = self.advance().value
                ast['environment_division']['configuration']['special_names']['currency_sign'] = currency
            else:
                self.advance()
    
    def parse_file_select(self) -> Dict:
        """Parse SELECT statement for file"""
        file_def = {
            'name': self.advance().value,
            'assign': None,
            'organization': 'SEQUENTIAL',  # default
            'access': 'SEQUENTIAL',
            'key': None,
            'alternate_keys': [],
            'status': None
        }
        
        while not self.match_keyword('SELECT') and not self.check_division() and not self.check(TokenType.DOT):
            if self.match_keyword('ASSIGN'):
                self.match_keyword('TO')
                file_def['assign'] = self.advance().value.strip('"')
            elif self.match_keyword('ORGANIZATION'):
                self.match_keyword('IS')
                org = []
                while not self.check_keyword_ahead(['ACCESS', 'RECORD', 'FILE', 'SELECT']):
                    org.append(self.advance().value)
                file_def['organization'] = ' '.join(org)
            elif self.match_keyword('ACCESS'):
                self.match_keyword('MODE')
                self.match_keyword('IS')
                file_def['access'] = self.advance().value
            elif self.match_keyword('RECORD'):
                self.match_keyword('KEY')
                self.match_keyword('IS')
                file_def['key'] = self.advance().value
            elif self.match_keyword('ALTERNATE'):
                self.match_keyword('RECORD')
                self.match_keyword('KEY')
                self.match_keyword('IS')
                file_def['alternate_keys'].append(self.advance().value)
            elif self.match_keyword('FILE'):
                self.match_keyword('STATUS')
                self.match_keyword('IS')
                file_def['status'] = self.advance().value
            else:
                self.advance()
        
        return file_def
    
    def check_keyword_ahead(self, keywords: List[str]) -> bool:
        """Check if any of the keywords appear ahead"""
        if self.is_at_end():
            return False
        token = self.peek()
        return token and token.type == TokenType.KEYWORD and token.value.upper() in keywords
    
    def parse_data_division(self, ast: Dict):
        """Parse DATA DIVISION"""
        current_section = None
        
        while not self.check_division():
            if self.match(TokenType.SECTION):
                section_name = self.previous().value.upper()
                if 'WORKING-STORAGE' in section_name:
                    current_section = 'working_storage'
                    ast['data_division'][current_section] = []
            elif current_section and self.check_number():
                # Parse data item
                level = int(self.advance().value)
                name = self.advance().value
                
                data_item = DataItem(
                    level=level,
                    name=name,
                    picture=None,
                    value=None,
                    occurs=None,
                    children=[]
                )
                
                # Parse PICTURE clause
                while not self.check(TokenType.DOT):
                    if self.match(TokenType.PICTURE):
                        data_item.picture = self.previous().value
                    elif self.match_keyword('VALUE'):
                        data_item.value = self.advance().value
                    elif self.match_keyword('OCCURS'):
                        data_item.occurs = int(self.advance().value)
                    else:
                        self.advance()
                
                ast['data_division'][current_section].append(data_item)
                self.data_items[name] = data_item
            else:
                self.advance()
    
    def parse_procedure_division(self, ast: Dict):
        """Parse PROCEDURE DIVISION"""
        current_paragraph = None
        
        while not self.is_at_end():
            if self.match(TokenType.EXEC_SQL):
                sql = self.parse_sql_block()
                self.sql_statements.append(sql)
                if current_paragraph:
                    ast['procedure_division'][current_paragraph]['sql'].append(sql)
            elif self.check(TokenType.IDENTIFIER) and self.peek_next() and self.peek_next().type == TokenType.DOT:
                # New paragraph
                current_paragraph = self.advance().value
                self.advance()  # Skip dot
                ast['procedure_division'][current_paragraph] = {
                    'statements': [],
                    'sql': []
                }
            elif current_paragraph:
                # Parse statements
                statement = self.parse_statement()
                if statement:
                    ast['procedure_division'][current_paragraph]['statements'].append(statement)
            else:
                self.advance()
        
        return ast
    
    def parse_sql_block(self) -> SQLStatement:
        """Parse EXEC SQL ... END-EXEC block"""
        sql_tokens = []
        host_vars = []
        
        while not self.match(TokenType.END_EXEC):
            token = self.advance()
            sql_tokens.append(token.value)
            
            # Detect host variables (starting with :)
            if token.value.startswith(':'):
                host_vars.append(token.value[1:])
        
        sql_text = ' '.join(sql_tokens)
        
        # Determine SQL type
        sql_type = 'UNKNOWN'
        if 'SELECT' in sql_text.upper():
            sql_type = 'SELECT'
        elif 'INSERT' in sql_text.upper():
            sql_type = 'INSERT'
        elif 'UPDATE' in sql_text.upper():
            sql_type = 'UPDATE'
        elif 'DELETE' in sql_text.upper():
            sql_type = 'DELETE'
        elif 'DECLARE' in sql_text.upper() and 'CURSOR' in sql_text.upper():
            sql_type = 'CURSOR'
        
        return SQLStatement(
            type=sql_type,
            sql=sql_text,
            host_variables=host_vars
        )
    
    def parse_statement(self) -> Optional[Dict]:
        """Parse a single COBOL statement"""
        if self.match_keyword('PERFORM'):
            return self.parse_perform()
        elif self.match_keyword('MOVE'):
            return self.parse_move()
        elif self.match_keyword('DISPLAY'):
            return self.parse_display()
        elif self.match_keyword('IF'):
            return self.parse_if()
        elif self.match_keyword('EVALUATE'):
            return self.parse_evaluate()
        else:
            self.advance()
            return None
    
    # Helper methods
    def match(self, *types: TokenType) -> bool:
        for t in types:
            if self.check(t):
                self.advance()
                return True
        return False
    
    def match_keyword(self, keyword: str) -> bool:
        if self.check(TokenType.KEYWORD) and self.peek().value.upper() == keyword.upper():
            self.advance()
            return True
        return False
    
    def check(self, type: TokenType) -> bool:
        if self.is_at_end():
            return False
        return self.peek().type == type
    
    def check_division(self) -> bool:
        return self.check(TokenType.DIVISION)
    
    def check_number(self) -> bool:
        return self.check(TokenType.NUMBER)
    
    def advance(self) -> Token:
        if not self.is_at_end():
            self.current += 1
        return self.previous()
    
    def is_at_end(self) -> bool:
        return self.current >= len(self.tokens)
    
    def peek(self) -> Optional[Token]:
        if self.is_at_end():
            return None
        return self.tokens[self.current]
    
    def peek_next(self) -> Optional[Token]:
        if self.current + 1 >= len(self.tokens):
            return None
        return self.tokens[self.current + 1]
    
    def previous(self) -> Token:
        return self.tokens[self.current - 1]
    
    def parse_perform(self) -> Dict:
        """Parse PERFORM statement"""
        target = self.advance().value
        return {'type': 'PERFORM', 'target': target}
    
    def parse_move(self) -> Dict:
        """Parse MOVE statement"""
        source = self.advance().value
        self.match_keyword('TO')
        target = self.advance().value
        return {'type': 'MOVE', 'source': source, 'target': target}
    
    def parse_display(self) -> Dict:
        """Parse DISPLAY statement"""
        values = []
        while not self.check(TokenType.DOT):
            values.append(self.advance().value)
        return {'type': 'DISPLAY', 'values': values}
    
    def parse_if(self) -> Dict:
        """Parse IF statement"""
        condition = []
        while not self.match_keyword('THEN') and not self.check(TokenType.DOT):
            condition.append(self.advance().value)
        
        then_branch = []
        else_branch = []
        current_branch = then_branch
        
        while not self.match_keyword('END-IF'):
            if self.match_keyword('ELSE'):
                current_branch = else_branch
            else:
                stmt = self.parse_statement()
                if stmt:
                    current_branch.append(stmt)
        
        return {
            'type': 'IF',
            'condition': ' '.join(condition),
            'then': then_branch,
            'else': else_branch
        }
    
    def parse_evaluate(self) -> Dict:
        """Parse EVALUATE statement"""
        subject = self.advance().value
        cases = []
        
        while not self.match_keyword('END-EVALUATE'):
            if self.match_keyword('WHEN'):
                value = self.advance().value
                statements = []
                
                while not self.check_keyword('WHEN') and not self.check_keyword('END-EVALUATE'):
                    stmt = self.parse_statement()
                    if stmt:
                        statements.append(stmt)
                
                cases.append({'value': value, 'statements': statements})
        
        return {
            'type': 'EVALUATE',
            'subject': subject,
            'cases': cases
        }
    
    def check_keyword(self, keyword: str) -> bool:
        return self.check(TokenType.KEYWORD) and self.peek() and self.peek().value.upper() == keyword.upper()

# ==================== Type Mapper ====================

class TypeMapper:
    """Mappa i tipi COBOL in tipi Java"""
    
    @staticmethod
    def map_picture_to_java(picture: str) -> Tuple[str, Optional[int]]:
        """Converte PICTURE clause in tipo Java e lunghezza"""
        picture = picture.upper().replace('PICTURE', '').replace('PIC', '').strip()
        
        # Numeric types
        if '9' in picture:
            if 'V' in picture or '.' in picture:
                return 'BigDecimal', None
            else:
                length = picture.count('9') + picture.count('(')
                if length <= 4:
                    return 'short', length
                elif length <= 9:
                    return 'int', length
                else:
                    return 'long', length
        
        # String types
        elif 'X' in picture or 'A' in picture:
            length = 0
            if '(' in picture:
                # Extract length from X(n) format
                match = re.search(r'\((\d+)\)', picture)
                if match:
                    length = int(match.group(1))
            else:
                length = picture.count('X') + picture.count('A')
            
            return 'String', length
        
        # Signed numeric
        elif 'S' in picture:
            return 'BigDecimal', None
        
        return 'String', None

# ==================== Java Generator ====================

class JavaGenerator:
    """Genera codice Java da AST COBOL"""
    
    def __init__(self, ast: Dict, config: Dict):
        self.ast = ast
        self.config = config
        self.imports = set()
        self.class_name = self._to_camel_case(ast['program_id'])
        
    def generate(self) -> str:
        """Genera il codice Java completo"""
        self._analyze_imports()
        
        java_code = f"""/**
 * Generated from COBOL program: {self.ast['program_id']}
 * Author: {self.ast.get('author', 'Unknown')}
 * Migration Date: {self._get_current_date()}
 */
"""
        
        # Package declaration
        java_code += f"package {self.config.get('package', 'com.miriade.cobol.migrated')};\n\n"
        
        # Imports
        for imp in sorted(self.imports):
            java_code += f"import {imp};\n"
        java_code += "\n"
        
        # Class declaration
        java_code += f"public class {self.class_name} {{\n\n"
        
        # Generate configuration from ENVIRONMENT DIVISION
        java_code += self._generate_environment_config()
        
        # Generate file handlers from ENVIRONMENT DIVISION
        java_code += self._generate_file_handlers()
        
        # Generate data members
        java_code += self._generate_data_members()
        
        # Generate constructor
        java_code += self._generate_constructor()
        
        # Generate methods
        java_code += self._generate_methods()
        
        # Generate main method
        java_code += self._generate_main()
        
        java_code += "}\n"
        
        return java_code
    
    def _generate_environment_config(self) -> str:
        """Genera configurazione da ENVIRONMENT DIVISION"""
        env_div = self.ast.get('environment_division', {})
        config = env_div.get('configuration', {})
        
        if not config:
            return ""
        
        code = "    // ENVIRONMENT DIVISION - Configuration\n"
        code += "    static {\n"
        
        # Special names
        special_names = config.get('special_names', {})
        if special_names.get('decimal_point') == 'COMMA':
            code += "        // DECIMAL-POINT IS COMMA\n"
            code += "        Locale.setDefault(Locale.ITALY);\n"
            code += "        DecimalFormatSymbols symbols = new DecimalFormatSymbols();\n"
            code += "        symbols.setDecimalSeparator(',');\n"
            code += "        symbols.setGroupingSeparator('.');\n"
            self.imports.add("java.text.DecimalFormatSymbols")
            self.imports.add("java.util.Locale")
        
        if 'currency_sign' in special_names:
            code += f"        // CURRENCY SIGN IS {special_names['currency_sign']}\n"
            code += f"        Currency currency = Currency.getInstance(\"{special_names['currency_sign']}\");\n"
            self.imports.add("java.util.Currency")
        
        code += "    }\n\n"
        return code
    
    def _generate_file_handlers(self) -> str:
        """Genera handler per i file da ENVIRONMENT DIVISION"""
        env_div = self.ast.get('environment_division', {})
        files = env_div.get('files', [])
        
        if not files:
            return ""
        
        code = "    // ENVIRONMENT DIVISION - File Handlers\n"
        
        for file_def in files:
            file_name = self._to_java_variable(file_def['name'])
            class_name = self._to_camel_case(file_def['name']) + "Handler"
            
            # Generate file handler class
            code += f"    private {class_name} {file_name};\n"
            
            # Add to imports based on organization
            if file_def['organization'] == 'LINE SEQUENTIAL':
                self.imports.add("java.io.BufferedReader")
                self.imports.add("java.io.BufferedWriter")
            elif file_def['organization'] == 'INDEXED':
                self.imports.add("java.io.RandomAccessFile")
                self.imports.add("java.util.Map")
                self.imports.add("java.util.HashMap")
        
        code += "\n"
        
        # Generate file handler classes
        for file_def in files:
            code += self._generate_file_handler_class(file_def)
        
        return code
    
    def _generate_file_handler_class(self, file_def: Dict) -> str:
        """Genera classe handler per un file specifico"""
        class_name = self._to_camel_case(file_def['name']) + "Handler"
        
        code = f"    // File handler for {file_def['name']}\n"
        code += f"    class {class_name} {{\n"
        
        if file_def['organization'] == 'LINE SEQUENTIAL':
            code += "        private BufferedReader reader;\n"
            code += "        private BufferedWriter writer;\n"
            code += f"        private String fileName = \"{file_def['assign']}\";\n"
            
            if file_def.get('status'):
                status_var = self._to_java_variable(file_def['status'])
                code += f"        private String fileStatus = \"00\"; // Links to {status_var}\n"
            
            code += """
        public void openInput() throws IOException {
            try {
                reader = new BufferedReader(new FileReader(fileName));
                if (fileStatus != null) fileStatus = "00";
            } catch (IOException e) {
                if (fileStatus != null) fileStatus = "30";
                throw e;
            }
        }
        
        public void openOutput() throws IOException {
            try {
                writer = new BufferedWriter(new FileWriter(fileName));
                if (fileStatus != null) fileStatus = "00";
            } catch (IOException e) {
                if (fileStatus != null) fileStatus = "30";
                throw e;
            }
        }
        
        public String readLine() throws IOException {
            return reader.readLine();
        }
        
        public void writeLine(String line) throws IOException {
            writer.write(line);
            writer.newLine();
        }
        
        public void close() throws IOException {
            if (reader != null) reader.close();
            if (writer != null) writer.close();
        }
"""
        elif file_def['organization'] == 'INDEXED':
            code += "        private RandomAccessFile file;\n"
            code += f"        private String fileName = \"{file_def['assign']}\";\n"
            code += f"        private Map<String, Long> primaryIndex = new HashMap<>();\n"
            
            if file_def.get('alternate_keys'):
                for key in file_def['alternate_keys']:
                    key_var = self._to_java_variable(key)
                    code += f"        private Map<String, Long> {key_var}Index = new HashMap<>();\n"
            
            code += """
        public void open(String mode) throws IOException {
            String accessMode = mode.equals("INPUT") ? "r" : "rw";
            file = new RandomAccessFile(fileName, accessMode);
            buildIndexes();
        }
        
        private void buildIndexes() throws IOException {
            // Build primary and alternate indexes
            // Implementation depends on record structure
        }
"""
        
        code += "    }\n\n"
        return code
    
    def _analyze_imports(self):
        """Analizza quali import sono necessari"""
        self.imports.add("java.math.BigDecimal")
        self.imports.add("java.util.Scanner")
        
        # Check for SQL usage
        if self.ast.get('sql_statements'):
            self.imports.add("java.sql.*")
            self.imports.add("javax.sql.DataSource")
            
        # Check for file operations
        if self._has_file_operations():
            self.imports.add("java.io.*")
            self.imports.add("java.nio.file.*")
    
    def _generate_data_members(self) -> str:
        """Genera le variabili membro della classe"""
        code = "    // Data members from WORKING-STORAGE SECTION\n"
        
        working_storage = self.ast['data_division'].get('working_storage', [])
        
        for item in working_storage:
            if item.level == 1:
                code += self._generate_data_structure(item)
            else:
                java_type, length = TypeMapper.map_picture_to_java(item.picture) if item.picture else ('String', None)
                
                # Initialize with default value
                default_value = self._get_default_value(java_type, item.value)
                code += f"    private static {java_type} {self._to_java_variable(item.name)} = {default_value};\n"
        
        code += "\n"
        
        # Add SQL connection if needed
        if self.ast.get('sql_statements'):
            code += "    // Database connection\n"
            code += "    private static Connection connection;\n"
            code += "    private static final String DB_URL = \"jdbc:postgresql://localhost/\";\n"
            code += "    private static final String DB_USER = \"postgres\";\n"
            code += "    private static final String DB_PASSWORD = \"password\";\n\n"
        
        return code
    
    def _generate_data_structure(self, item: DataItem) -> str:
        """Genera una struttura dati (01 level) come classe interna"""
        class_name = self._to_camel_case(item.name)
        code = f"\n    // Data structure: {item.name}\n"
        code += f"    static class {class_name} {{\n"
        
        # Generate fields for children
        for child in item.children:
            java_type, length = TypeMapper.map_picture_to_java(child.picture) if child.picture else ('String', None)
            field_name = self._to_java_variable(child.name)
            code += f"        private {java_type} {field_name};\n"
        
        # Generate getters and setters
        code += "\n"
        for child in item.children:
            java_type, length = TypeMapper.map_picture_to_java(child.picture) if child.picture else ('String', None)
            field_name = self._to_java_variable(child.name)
            method_name = self._to_camel_case(child.name)
            
            # Getter
            code += f"        public {java_type} get{method_name}() {{ return {field_name}; }}\n"
            # Setter
            code += f"        public void set{method_name}({java_type} {field_name}) {{ this.{field_name} = {field_name}; }}\n"
        
        code += "    }\n"
        
        # Create instance
        instance_name = self._to_java_variable(item.name)
        code += f"    private static {class_name} {instance_name} = new {class_name}();\n"
        
        return code
    
    def _generate_constructor(self) -> str:
        """Genera il costruttore"""
        return f"""    // Constructor
    public {self.class_name}() {{
        // Initialize components
    }}
    
"""
    
    def _generate_methods(self) -> str:
        """Genera i metodi dalle procedure COBOL"""
        code = ""
        
        # Generate database connection method if needed
        if self.ast.get('sql_statements'):
            code += self._generate_db_methods()
        
        # Generate methods for each paragraph
        for paragraph_name, paragraph_data in self.ast['procedure_division'].items():
            method_name = self._to_java_method_name(paragraph_name)
            code += f"    private static void {method_name}() {{\n"
            
            # Generate statements
            for statement in paragraph_data.get('statements', []):
                code += self._generate_statement(statement, indent=8)
            
            # Generate SQL statements
            for sql in paragraph_data.get('sql', []):
                code += self._generate_sql_statement(sql, indent=8)
            
            code += "    }\n\n"
        
        return code
    
    def _generate_statement(self, statement: Dict, indent: int) -> str:
        """Genera codice Java per uno statement COBOL"""
        ind = ' ' * indent
        
        if statement['type'] == 'PERFORM':
            method_name = self._to_java_method_name(statement['target'])
            return f"{ind}{method_name}();\n"
        
        elif statement['type'] == 'MOVE':
            source = self._to_java_variable(statement['source'])
            target = self._to_java_variable(statement['target'])
            return f"{ind}{target} = {source};\n"
        
        elif statement['type'] == 'DISPLAY':
            values = ' + '.join([f'"{v}"' if not self._is_variable(v) else self._to_java_variable(v) 
                               for v in statement['values']])
            return f"{ind}System.out.println({values});\n"
        
        elif statement['type'] == 'IF':
            code = f"{ind}if ({statement['condition']}) {{\n"
            for stmt in statement['then']:
                code += self._generate_statement(stmt, indent + 4)
            if statement['else']:
                code += f"{ind}}} else {{\n"
                for stmt in statement['else']:
                    code += self._generate_statement(stmt, indent + 4)
            code += f"{ind}}}\n"
            return code
        
        elif statement['type'] == 'EVALUATE':
            code = f"{ind}switch ({self._to_java_variable(statement['subject'])}) {{\n"
            for case in statement['cases']:
                code += f"{ind}    case {case['value']}:\n"
                for stmt in case['statements']:
                    code += self._generate_statement(stmt, indent + 8)
                code += f"{ind}        break;\n"
            code += f"{ind}}}\n"
            return code
        
        return f"{ind}// TODO: Implement {statement['type']}\n"
    
    def _generate_sql_statement(self, sql: SQLStatement, indent: int) -> str:
        """Genera codice Java per statement SQL"""
        ind = ' ' * indent
        code = f"\n{ind}// SQL: {sql.type}\n"
        
        if sql.type == 'SELECT':
            code += f"{ind}try {{\n"
            code += f"{ind}    String sql = {self._format_sql_string(sql.sql)};\n"
            code += f"{ind}    PreparedStatement pstmt = connection.prepareStatement(sql);\n"
            
            # Set parameters
            for i, var in enumerate(sql.host_variables, 1):
                code += f"{ind}    pstmt.setObject({i}, {self._to_java_variable(var)});\n"
            
            code += f"{ind}    ResultSet rs = pstmt.executeQuery();\n"
            code += f"{ind}    if (rs.next()) {{\n"
            code += f"{ind}        // TODO: Map result to variables\n"
            code += f"{ind}    }}\n"
            code += f"{ind}    rs.close();\n"
            code += f"{ind}    pstmt.close();\n"
            code += f"{ind}}} catch (SQLException e) {{\n"
            code += f"{ind}    e.printStackTrace();\n"
            code += f"{ind}}}\n"
        
        elif sql.type in ['INSERT', 'UPDATE', 'DELETE']:
            code += f"{ind}try {{\n"
            code += f"{ind}    String sql = {self._format_sql_string(sql.sql)};\n"
            code += f"{ind}    PreparedStatement pstmt = connection.prepareStatement(sql);\n"
            
            # Set parameters
            for i, var in enumerate(sql.host_variables, 1):
                code += f"{ind}    pstmt.setObject({i}, {self._to_java_variable(var)});\n"
            
            code += f"{ind}    int rowsAffected = pstmt.executeUpdate();\n"
            code += f"{ind}    pstmt.close();\n"
            code += f"{ind}}} catch (SQLException e) {{\n"
            code += f"{ind}    e.printStackTrace();\n"
            code += f"{ind}}}\n"
        
        return code
    
    def _generate_db_methods(self) -> str:
        """Genera metodi per gestione database"""
        return """    private static void connectDatabase() {
        try {
            connection = DriverManager.getConnection(DB_URL, DB_USER, DB_PASSWORD);
            System.out.println("Connessione al database stabilita");
        } catch (SQLException e) {
            System.err.println("Errore connessione database: " + e.getMessage());
            System.exit(1);
        }
    }
    
    private static void disconnectDatabase() {
        try {
            if (connection != null && !connection.isClosed()) {
                connection.close();
                System.out.println("Disconnesso dal database");
            }
        } catch (SQLException e) {
            e.printStackTrace();
        }
    }
    
"""
    
    def _generate_main(self) -> str:
        """Genera il metodo main"""
        return """    public static void main(String[] args) {
        // Connect to database if needed
        if (connection == null) {
            connectDatabase();
        }
        
        // Execute main logic
        mainLogic();
        
        // Disconnect from database
        disconnectDatabase();
    }
    
"""
    
    # Helper methods
    def _to_camel_case(self, name: str) -> str:
        """Converte COBOL-NAME in CamelCase"""
        parts = name.replace('-', '_').split('_')
        return ''.join(word.capitalize() for word in parts)
    
    def _to_java_variable(self, name: str) -> str:
        """Converte COBOL-NAME in javaVariableName"""
        parts = name.replace('-', '_').split('_')
        if len(parts) == 1:
            return parts[0].lower()
        return parts[0].lower() + ''.join(word.capitalize() for word in parts[1:])
    
    def _to_java_method_name(self, name: str) -> str:
        """Converte PARAGRAPH-NAME in methodName"""
        return self._to_java_variable(name)
    
    def _is_variable(self, value: str) -> bool:
        """Verifica se un valore è una variabile"""
        return value.upper() in [item.name for items in self.ast['data_division'].values() 
                                for item in items if hasattr(item, 'name')]
    
    def _get_default_value(self, java_type: str, cobol_value: str) -> str:
        """Ottiene il valore di default per un tipo Java"""
        if cobol_value:
            if java_type == 'String':
                return f'"{cobol_value.strip("\'")}"'
            else:
                return cobol_value
        
        defaults = {
            'String': '""',
            'int': '0',
            'short': '0',
            'long': '0L',
            'BigDecimal': 'BigDecimal.ZERO',
            'boolean': 'false'
        }
        return defaults.get(java_type, 'null')
    
    def _format_sql_string(self, sql: str) -> str:
        """Formatta SQL per Java sostituendo host variables con ?"""
        # Replace :HOST-VAR with ?
        sql_formatted = re.sub(r':\w+', '?', sql)
        return f'"{sql_formatted}"'
    
    def _has_file_operations(self) -> bool:
        """Verifica se il programma usa operazioni su file"""
        file_keywords = ['OPEN', 'CLOSE', 'READ', 'WRITE', 'FILE']
        for proc in self.ast['procedure_division'].values():
            for stmt in proc.get('statements', []):
                if any(keyword in str(stmt) for keyword in file_keywords):
                    return True
        return False
    
    def _get_current_date(self) -> str:
        """Ottiene la data corrente"""
        from datetime import datetime
        return datetime.now().strftime("%Y-%m-%d")

# ==================== SQL Mapper ====================

class SQLMapper:
    """Gestisce la trasformazione di SQL embedded in pattern Java moderni"""
    
    def __init__(self):
        self.entity_classes = {}
        self.dao_classes = {}
    
    def analyze_tables(self, sql_statements: List[SQLStatement]) -> Dict[str, List[str]]:
        """Analizza le tabelle utilizzate negli statement SQL"""
        tables = {}
        
        for stmt in sql_statements:
            # Extract table names from SQL
            sql_upper = stmt.sql.upper()
            
            # Find FROM clause
            if 'FROM' in sql_upper:
                from_index = sql_upper.index('FROM')
                after_from = sql_upper[from_index + 4:].strip()
                
                # Extract table name (simplified - handles basic cases)
                table_match = re.match(r'(\w+)', after_from)
                if table_match:
                    table_name = table_match.group(1)
                    if table_name not in tables:
                        tables[table_name] = []
                    
                    # Extract columns from SELECT
                    if stmt.type == 'SELECT' and 'SELECT' in sql_upper:
                        select_index = sql_upper.index('SELECT')
                        select_part = sql_upper[select_index + 6:from_index].strip()
                        
                        # Simple column extraction (handles basic cases)
                        if select_part != '*':
                            columns = [col.strip() for col in select_part.split(',')]
                            tables[table_name].extend(columns)
        
        return tables
    
    def generate_entity_class(self, table_name: str, columns: List[str]) -> str:
        """Genera una classe Entity JPA per una tabella"""
        class_name = self._table_to_class_name(table_name)
        
        code = f"""
@Entity
@Table(name = "{table_name}")
public class {class_name} {{
    
"""
        # Generate fields (simplified - in real implementation would need type info)
        for column in columns:
            field_name = self._column_to_field_name(column)
            code += f"    @Column(name = \"{column}\")\n"
            code += f"    private String {field_name};\n\n"
        
        # Generate getters and setters
        for column in columns:
            field_name = self._column_to_field_name(column)
            method_name = self._to_camel_case(field_name)
            
            code += f"    public String get{method_name}() {{\n"
            code += f"        return {field_name};\n"
            code += f"    }}\n\n"
            
            code += f"    public void set{method_name}(String {field_name}) {{\n"
            code += f"        this.{field_name} = {field_name};\n"
            code += f"    }}\n\n"
        
        code += "}\n"
        
        return code
    
    def generate_dao_class(self, table_name: str) -> str:
        """Genera una classe DAO per una tabella"""
        entity_name = self._table_to_class_name(table_name)
        dao_name = f"{entity_name}DAO"
        
        code = f"""
@Repository
public class {dao_name} {{
    
    @Autowired
    private JdbcTemplate jdbcTemplate;
    
    public List<{entity_name}> findAll() {{
        String sql = "SELECT * FROM {table_name}";
        return jdbcTemplate.query(sql, new BeanPropertyRowMapper<>({entity_name}.class));
    }}
    
    public {entity_name} findById(Long id) {{
        String sql = "SELECT * FROM {table_name} WHERE id = ?";
        return jdbcTemplate.queryForObject(sql, new BeanPropertyRowMapper<>({entity_name}.class), id);
    }}
    
    public int save({entity_name} entity) {{
        // Implementation depends on table structure
        String sql = "INSERT INTO {table_name} (...) VALUES (...)";
        return jdbcTemplate.update(sql, /* parameters */);
    }}
    
    public int update({entity_name} entity) {{
        // Implementation depends on table structure
        String sql = "UPDATE {table_name} SET ... WHERE id = ?";
        return jdbcTemplate.update(sql, /* parameters */);
    }}
    
    public int deleteById(Long id) {{
        String sql = "DELETE FROM {table_name} WHERE id = ?";
        return jdbcTemplate.update(sql, id);
    }}
}}
"""
        return code
    
    def _table_to_class_name(self, table_name: str) -> str:
        """Converte TABLE_NAME in ClassName"""
        parts = table_name.lower().split('_')
        return ''.join(word.capitalize() for word in parts)
    
    def _column_to_field_name(self, column_name: str) -> str:
        """Converte COLUMN_NAME in fieldName"""
        parts = column_name.lower().split('_')
        if len(parts) == 1:
            return parts[0]
        return parts[0] + ''.join(word.capitalize() for word in parts[1:])
    
    def _to_camel_case(self, name: str) -> str:
        """Converte name in CamelCase"""
        return name[0].upper() + name[1:]

# ==================== Main Migration Tool ====================

class COBOLToJavaMigrator:
    """Tool principale per la migrazione COBOL to Java"""
    
    def __init__(self, config_file: str = "config/mapping_rules.yaml"):
        self.config = self._load_config(config_file)
        self.lexer = COBOLLexer()
        self.sql_mapper = SQLMapper()
        
    def _load_config(self, config_file: str) -> Dict:
        """Carica la configurazione"""
        # Default configuration if file doesn't exist
        default_config = {
            'package': 'com.miriade.cobol.migrated',
            'output_dir': 'output',
            'generate_entities': True,
            'generate_dao': True,
            'use_spring': True,
            'database': {
                'type': 'postgresql',
                'driver': 'org.postgresql.Driver'
            }
        }
        
        if os.path.exists(config_file):
            with open(config_file, 'r') as f:
                config = yaml.safe_load(f)
                default_config.update(config)
        
        return default_config
    
    def migrate_file(self, cobol_file: str) -> Dict[str, str]:
        """Migra un singolo file COBOL"""
        print(f"Processing {cobol_file}...")
        
        # Read COBOL source
        with open(cobol_file, 'r', encoding='utf-8') as f:
            cobol_code = f.read()
        
        # Tokenize
        tokens = self.lexer.tokenize(cobol_code)
        print(f"  - Tokenized: {len(tokens)} tokens")
        
        # Parse
        parser = COBOLParser(tokens)
        ast = parser.parse()
        print(f"  - Parsed: {ast['program_id']}")
        
        # Generate Java code
        generator = JavaGenerator(ast, self.config)
        java_code = generator.generate()
        
        # Generate additional classes if needed
        generated_files = {
            f"{generator.class_name}.java": java_code
        }
        
        # Generate entities and DAOs if SQL is used
        if ast.get('sql_statements') and self.config.get('generate_entities'):
            tables = self.sql_mapper.analyze_tables(ast['sql_statements'])
            
            for table_name, columns in tables.items():
                if columns:  # Only if we found columns
                    # Generate entity
                    entity_code = self.sql_mapper.generate_entity_class(table_name, list(set(columns)))
                    entity_name = self.sql_mapper._table_to_class_name(table_name)
                    generated_files[f"{entity_name}.java"] = entity_code
                    
                    # Generate DAO
                    if self.config.get('generate_dao'):
                        dao_code = self.sql_mapper.generate_dao_class(table_name)
                        generated_files[f"{entity_name}DAO.java"] = dao_code
        
        return generated_files
    
    def migrate_directory(self, input_dir: str, output_dir: str = None):
        """Migra tutti i file COBOL in una directory"""
        if output_dir is None:
            output_dir = self.config.get('output_dir', 'output')
        
        # Create output directory
        os.makedirs(output_dir, exist_ok=True)
        
        # Find all COBOL files
        cobol_files = []
        for root, dirs, files in os.walk(input_dir):
            for file in files:
                if file.endswith(('.cob', '.cbl', '.cobol', '.txt')):
                    cobol_files.append(os.path.join(root, file))
        
        print(f"Found {len(cobol_files)} COBOL files to migrate")
        
        # Process each file
        all_generated = {}
        for cobol_file in cobol_files:
            try:
                generated = self.migrate_file(cobol_file)
                all_generated.update(generated)
                
                # Write generated files
                for filename, content in generated.items():
                    output_path = os.path.join(output_dir, filename)
                    with open(output_path, 'w', encoding='utf-8') as f:
                        f.write(content)
                    print(f"  - Generated: {output_path}")
                    
            except Exception as e:
                print(f"  - Error processing {cobol_file}: {str(e)}")
        
        # Generate project structure files
        self._generate_project_files(output_dir)
        
        print(f"\nMigration complete! Generated {len(all_generated)} Java files")
        return all_generated
    
    def _generate_project_files(self, output_dir: str):
        """Genera file di progetto Maven/Gradle"""
        # Generate pom.xml
        pom_content = """<?xml version="1.0" encoding="UTF-8"?>
<project xmlns="http://maven.apache.org/POM/4.0.0"
         xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance"
         xsi:schemaLocation="http://maven.apache.org/POM/4.0.0 
         http://maven.apache.org/xsd/maven-4.0.0.xsd">
    <modelVersion>4.0.0</modelVersion>
    
    <groupId>com.miriade</groupId>
    <artifactId>cobol-migrated</artifactId>
    <version>1.0-SNAPSHOT</version>
    
    <properties>
        <maven.compiler.source>11</maven.compiler.source>
        <maven.compiler.target>11</maven.compiler.target>
        <project.build.sourceEncoding>UTF-8</project.build.sourceEncoding>
    </properties>
    
    <dependencies>
        <dependency>
            <groupId>org.postgresql</groupId>
            <artifactId>postgresql</artifactId>
            <version>42.5.1</version>
        </dependency>
        <dependency>
            <groupId>org.springframework.boot</groupId>
            <artifactId>spring-boot-starter-jdbc</artifactId>
            <version>2.7.5</version>
        </dependency>
        <dependency>
            <groupId>org.springframework.boot</groupId>
            <artifactId>spring-boot-starter-data-jpa</artifactId>
            <version>2.7.5</version>
        </dependency>
    </dependencies>
</project>
"""
        
        pom_path = os.path.join(output_dir, "pom.xml")
        with open(pom_path, 'w') as f:
            f.write(pom_content)
        print(f"  - Generated: {pom_path}")
        
        # Generate README
        readme_content = """# Migrated COBOL to Java Project

This project contains Java code automatically migrated from COBOL programs.

## Structure

- Main classes: Direct translations of COBOL programs
- Entity classes: JPA entities for database tables
- DAO classes: Data Access Objects for database operations

## Setup

1. Configure database connection in application.properties
2. Run `mvn clean install` to build
3. Execute main classes as needed

## Notes

- Review and test all generated code before production use
- Some manual adjustments may be required for complex COBOL constructs
- Database queries have been converted to JDBC/JPA patterns
"""
        
        readme_path = os.path.join(output_dir, "README.md")
        with open(readme_path, 'w') as f:
            f.write(readme_content)
        print(f"  - Generated: {readme_path}")

# ==================== CLI Interface ====================

def main():
    """Main entry point"""
    import argparse
    
    parser = argparse.ArgumentParser(description='COBOL to Java Migration Tool')
    parser.add_argument('input', help='Input COBOL file or directory')
    parser.add_argument('-o', '--output', help='Output directory', default='output')
    parser.add_argument('-c', '--config', help='Configuration file', default='config/mapping_rules.yaml')
    parser.add_argument('--no-entities', action='store_true', help='Skip entity generation')
    parser.add_argument('--no-dao', action='store_true', help='Skip DAO generation')
    
    args = parser.parse_args()
    
    # Create migrator
    migrator = COBOLToJavaMigrator(args.config)
    
    # Override config with CLI options
    if args.no_entities:
        migrator.config['generate_entities'] = False
    if args.no_dao:
        migrator.config['generate_dao'] = False
    
    # Perform migration
    if os.path.isdir(args.input):
        migrator.migrate_directory(args.input, args.output)
    else:
        generated = migrator.migrate_file(args.input)
        
        # Write output files
        os.makedirs(args.output, exist_ok=True)
        for filename, content in generated.items():
            output_path = os.path.join(args.output, filename)
            with open(output_path, 'w', encoding='utf-8') as f:
                f.write(content)
            print(f"Generated: {output_path}")

if __name__ == "__main__":
    main()