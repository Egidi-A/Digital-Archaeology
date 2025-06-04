"""
COBOL Parser - Costruisce l'AST dal codice COBOL tokenizzato
"""

import ply.yacc as yacc
from loguru import logger
from .cobol_lexer import CobolLexer
from .ast_nodes import *


class CobolParser:
    """Parser per il linguaggio COBOL"""
    
    # Definisci precedenza per risolvere ambiguità
    precedence = (
        ('left', 'OR'),
        ('left', 'AND'),
        ('left', 'EQUALS', 'NOT_EQUAL', 'GREATER_OP', 'LESS_OP', 'GREATER_EQUAL', 'LESS_EQUAL'),
        ('left', 'PLUS', 'MINUS'),
        ('left', 'MULTIPLY_OP', 'DIVIDE_OP'),
    )
    
    def __init__(self):
        self.lexer = CobolLexer()
        self.lexer.build()
        self.tokens = self.lexer.tokens
        self.parser = None
        self.current_division = None
        self.current_section = None
        self.data_items = {}  # Dizionario per tracciare data items
        self.procedures = {}  # Dizionario per tracciare procedure
        
    def p_program(self, p):
        """program : identification_division environment_division_opt data_division_opt procedure_division_opt
                  | identification_division data_division_opt procedure_division_opt
                  | identification_division procedure_division_opt"""
        program_node = p[1]  # identification_division returns ProgramNode
        
        # Aggiungi le divisioni trovate
        for i in range(2, len(p)):
            if p[i] and isinstance(p[i], ASTNode):
                program_node.add_child(p[i])
                
        p[0] = program_node
    
    def p_identification_division(self, p):
        """identification_division : IDENTIFICATION DIVISION PERIOD program_id_paragraph author_paragraph_opt date_written_paragraph_opt"""
        program_node = p[4]  # program_id_paragraph returns ProgramNode
        
        if p[5]:  # author
            program_node.author = p[5]
        if p[6]:  # date_written
            program_node.date_written = p[6]
            
        p[0] = program_node
    
    def p_program_id_paragraph(self, p):
        """program_id_paragraph : PROGRAM_ID PERIOD IDENTIFIER PERIOD
                               | PROGRAM_ID PERIOD IDENTIFIER MINUS IDENTIFIER PERIOD"""
        if len(p) == 5:
            program_id = p[3]
        else:
            program_id = f"{p[3]}-{p[5]}"
        
        p[0] = ProgramNode(program_id=program_id, line_number=p.lineno(1))
    
    def p_author_paragraph_opt(self, p):
        """author_paragraph_opt : AUTHOR PERIOD author_name PERIOD
                               | empty"""
        if len(p) > 2:
            p[0] = p[3]
        else:
            p[0] = None
    
    def p_author_name(self, p):
        """author_name : text_content"""
        p[0] = p[1]
    
    def p_text_content(self, p):
        """text_content : IDENTIFIER
                       | IDENTIFIER MINUS IDENTIFIER
                       | STRING_LITERAL
                       | text_content IDENTIFIER
                       | text_content MINUS
                       | text_content STRING_LITERAL"""
        if len(p) == 2:
            p[0] = str(p[1])
        elif len(p) == 4 and p[2] == '-':
            p[0] = f"{p[1]}-{p[3]}"
        else:
            p[0] = f"{p[1]} {p[2]}"
    
    def p_date_written_paragraph_opt(self, p):
        """date_written_paragraph_opt : DATE_WRITTEN PERIOD date_value PERIOD
                                     | empty"""
        if len(p) > 2:
            p[0] = p[3]
        else:
            p[0] = None
    
    def p_date_value(self, p):
        """date_value : NUMBER MINUS NUMBER MINUS NUMBER
                     | text_content"""
        if len(p) == 6:
            p[0] = f"{p[1]}-{p[3]}-{p[5]}"
        else:
            p[0] = p[1]
    
    def p_environment_division_opt(self, p):
        """environment_division_opt : environment_division
                                   | empty"""
        p[0] = p[1]
    
    def p_environment_division(self, p):
        """environment_division : ENVIRONMENT DIVISION PERIOD input_output_section_opt"""
        div_node = DivisionNode("ENVIRONMENT", line_number=p.lineno(1))
        if p[4]:
            div_node.add_child(p[4])
        p[0] = div_node
    
    def p_input_output_section_opt(self, p):
        """input_output_section_opt : INPUT_OUTPUT SECTION PERIOD file_control_paragraph_opt
                                   | empty"""
        if len(p) > 2:
            section_node = ASTNode(NodeType.SECTION, "INPUT-OUTPUT", 
                                 line_number=p.lineno(1))
            if p[4]:
                section_node.add_child(p[4])
            p[0] = section_node
        else:
            p[0] = None
    
    def p_file_control_paragraph_opt(self, p):
        """file_control_paragraph_opt : FILE_CONTROL PERIOD file_control_entries
                                     | empty"""
        if len(p) > 2:
            p[0] = p[3]
        else:
            p[0] = None
    
    def p_file_control_entries(self, p):
        """file_control_entries : file_control_entry
                               | file_control_entries file_control_entry"""
        if len(p) == 2:
            p[0] = [p[1]]
        else:
            p[1].append(p[2])
            p[0] = p[1]
    
    def p_file_control_entry(self, p):
        """file_control_entry : SELECT IDENTIFIER ASSIGN TO STRING_LITERAL organization_clause_opt PERIOD
                             | SELECT IDENTIFIER ASSIGN TO IDENTIFIER organization_clause_opt PERIOD"""
        file_node = FileDefinitionNode(p[2], line_number=p.lineno(1))
        file_node.assign_to = p[5]
        if p[6]:
            file_node.organization = p[6]
        p[0] = file_node
    
    def p_organization_clause_opt(self, p):
        """organization_clause_opt : ORGANIZATION IS LINE SEQUENTIAL
                                  | ORGANIZATION IS SEQUENTIAL
                                  | ORGANIZATION IS RANDOM
                                  | ORGANIZATION IS DYNAMIC
                                  | empty"""
        if len(p) > 2:
            if len(p) == 5:
                p[0] = f"{p[3]} {p[4]}"
            else:
                p[0] = p[3]
        else:
            p[0] = "SEQUENTIAL"  # Default
    
    def p_data_division_opt(self, p):
        """data_division_opt : data_division
                            | empty"""
        p[0] = p[1]
    
    def p_data_division(self, p):
        """data_division : DATA DIVISION PERIOD file_section_opt working_storage_section_opt"""
        div_node = DivisionNode("DATA", line_number=p.lineno(1))
        if p[4]:
            div_node.add_child(p[4])
        if p[5]:
            div_node.add_child(p[5])
        p[0] = div_node
    
    def p_file_section_opt(self, p):
        """file_section_opt : FILE SECTION PERIOD file_descriptions
                           | empty"""
        if len(p) > 2:
            section_node = ASTNode(NodeType.SECTION, "FILE", 
                                 line_number=p.lineno(1))
            if p[4]:
                for fd in p[4]:
                    section_node.add_child(fd)
            p[0] = section_node
        else:
            p[0] = None
    
    def p_file_descriptions(self, p):
        """file_descriptions : file_description
                            | file_descriptions file_description"""
        if len(p) == 2:
            p[0] = [p[1]]
        else:
            p[1].append(p[2])
            p[0] = p[1]
    
    def p_file_description(self, p):
        """file_description : FD IDENTIFIER PERIOD data_description_entries"""
        fd_node = FileDefinitionNode(p[2], line_number=p.lineno(1))
        if p[4]:
            for entry in p[4]:
                fd_node.add_child(entry)
        p[0] = fd_node
    
    def p_working_storage_section_opt(self, p):
        """working_storage_section_opt : WORKING_STORAGE SECTION PERIOD data_description_entries
                                      | WORKING_STORAGE SECTION PERIOD
                                      | empty"""
        if len(p) > 2:
            section_node = ASTNode(NodeType.SECTION, "WORKING-STORAGE", 
                                 line_number=p.lineno(1))
            if len(p) > 4 and p[4]:
                for entry in p[4]:
                    section_node.add_child(entry)
                    # Registra data items per riferimenti futuri
                    if isinstance(entry, DataItemNode):
                        self.data_items[entry.name] = entry
            p[0] = section_node
        else:
            p[0] = None
    
    def p_data_description_entries(self, p):
        """data_description_entries : data_description_entry
                                   | data_description_entries data_description_entry
                                   | empty"""
        if len(p) == 2:
            if p[1]:
                p[0] = [p[1]]
            else:
                p[0] = []
        else:
            if p[2]:
                p[1].append(p[2])
            p[0] = p[1]
    
    def p_data_description_entry(self, p):
        """data_description_entry : level_number data_name_or_filler picture_clause_opt value_clause_opt occurs_clause_opt PERIOD
                                 | exec_sql_include
                                 | exec_sql_declare_cursor_section"""
        if len(p) == 2:
            p[0] = p[1]  # exec_sql statements
        else:
            level = p[1]
            name = p[2] if p[2] else "FILLER"
            
            data_node = DataItemNode(level, name, line_number=p.lineno(1))
            
            if p[3]:  # picture_clause
                data_node.picture = p[3]
                data_node.data_type, data_node.size, data_node.decimals = \
                    self._analyze_picture(p[3])
            
            if p[4]:  # value_clause
                data_node.value = p[4]
            
            if p[5]:  # occurs_clause
                data_node.occurs = p[5]
            
            p[0] = data_node
    
    def p_level_number(self, p):
        """level_number : LEVEL_NUMBER"""
        p[0] = str(p[1])
    
    def p_data_name_or_filler(self, p):
        """data_name_or_filler : IDENTIFIER
                              | FILLER
                              | empty"""
        p[0] = p[1] if p[1] else "FILLER"
    
    def p_picture_clause_opt(self, p):
        """picture_clause_opt : PICTURE IS picture_string
                             | PIC IS picture_string
                             | PICTURE picture_string
                             | PIC picture_string
                             | empty"""
        if len(p) > 2:
            if len(p) == 4:
                p[0] = p[3]
            else:
                p[0] = p[2]
        else:
            p[0] = None
    
    def p_picture_string(self, p):
        """picture_string : PICTURE_STRING
                         | IDENTIFIER"""
        # Gestisci sia PICTURE_STRING che IDENTIFIER per flessibilità
        p[0] = str(p[1])
    
    def p_value_clause_opt(self, p):
        """value_clause_opt : VALUE IS literal
                           | VALUE literal
                           | empty"""
        if len(p) > 2:
            if len(p) == 4:
                p[0] = p[3]
            else:
                p[0] = p[2]
        else:
            p[0] = None
    
    def p_literal(self, p):
        """literal : STRING_LITERAL
                  | NUMBER
                  | ZERO
                  | ZEROS
                  | ZEROES
                  | SPACE
                  | SPACES
                  | HIGH_VALUE
                  | HIGH_VALUES
                  | LOW_VALUE
                  | LOW_VALUES
                  | QUOTE
                  | QUOTES
                  | ALL STRING_LITERAL
                  | ALL literal"""
        if len(p) == 2:
            p[0] = p[1]
        else:
            p[0] = f"ALL {p[2]}"
    
    def p_occurs_clause_opt(self, p):
        """occurs_clause_opt : OCCURS NUMBER TIMES
                            | OCCURS NUMBER
                            | empty"""
        if len(p) > 2:
            p[0] = int(p[2])
        else:
            p[0] = None
    
    def p_exec_sql_include(self, p):
        """exec_sql_include : EXEC SQL INCLUDE IDENTIFIER END_EXEC PERIOD
                           | EXEC SQL INCLUDE IDENTIFIER END_EXEC"""
        sql_node = SqlStatementNode("INCLUDE", line_number=p.lineno(1))
        sql_node.sql_text = f"INCLUDE {p[4]}"
        p[0] = sql_node
    
    def p_exec_sql_declare_cursor_section(self, p):
        """exec_sql_declare_cursor_section : EXEC SQL sql_declare_cursor END_EXEC PERIOD
                                          | EXEC SQL sql_declare_cursor END_EXEC"""
        p[0] = p[3]
    
    def p_procedure_division_opt(self, p):
        """procedure_division_opt : procedure_division
                                 | empty"""
        p[0] = p[1]
    
    def p_procedure_division(self, p):
        """procedure_division : PROCEDURE DIVISION PERIOD procedure_content
                             | PROCEDURE DIVISION PERIOD"""
        div_node = DivisionNode("PROCEDURE", line_number=p.lineno(1))
        if len(p) > 4 and p[4]:
            for item in p[4]:
                div_node.add_child(item)
        p[0] = div_node
    
    def p_procedure_content(self, p):
        """procedure_content : paragraph_or_statement
                            | procedure_content paragraph_or_statement"""
        if len(p) == 2:
            if p[1]:
                p[0] = [p[1]]
            else:
                p[0] = []
        else:
            if p[2]:
                p[1].append(p[2])
            p[0] = p[1]
    
    def p_paragraph_or_statement(self, p):
        """paragraph_or_statement : paragraph
                                 | statement
                                 | empty"""
        p[0] = p[1]
    
    def p_paragraph(self, p):
        """paragraph : paragraph_name PERIOD statements_opt"""
        para_node = ProcedureNode(p[1], line_number=p.lineno(1))
        if p[3]:
            para_node.statements = p[3]
            for stmt in p[3]:
                para_node.add_child(stmt)
        self.procedures[p[1]] = para_node
        p[0] = para_node
    
    def p_paragraph_name(self, p):
        """paragraph_name : IDENTIFIER
                         | IDENTIFIER MINUS IDENTIFIER"""
        if len(p) == 2:
            p[0] = p[1]
        else:
            p[0] = f"{p[1]}-{p[3]}"
    
    def p_statements_opt(self, p):
        """statements_opt : statements
                         | empty"""
        p[0] = p[1] if p[1] else []
    
    def p_statements(self, p):
        """statements : statement
                     | statements statement"""
        if len(p) == 2:
            if p[1]:
                p[0] = [p[1]]
            else:
                p[0] = []
        else:
            if p[2]:
                p[1].append(p[2])
            p[0] = p[1]
    
    def p_statement(self, p):
        """statement : move_statement
                    | display_statement
                    | accept_statement
                    | perform_statement
                    | if_statement
                    | evaluate_statement
                    | exec_sql_statement
                    | stop_statement
                    | compute_statement
                    | add_statement
                    | subtract_statement
                    | multiply_statement
                    | divide_statement
                    | open_statement
                    | close_statement
                    | read_statement
                    | write_statement
                    | string_statement
                    | exit_statement
                    | continue_statement
                    | call_statement
                    | empty"""
        p[0] = p[1]
    
    def p_move_statement(self, p):
        """move_statement : MOVE identifier_or_literal TO identifier_list PERIOD"""
        stmt = StatementNode("MOVE", line_number=p.lineno(1))
        stmt.set_attribute("source", p[2])
        stmt.set_attribute("targets", p[4])
        p[0] = stmt
    
    def p_identifier_or_literal(self, p):
        """identifier_or_literal : qualified_identifier
                                | literal
                                | arithmetic_expression"""
        p[0] = p[1]
    
    def p_qualified_identifier(self, p):
        """qualified_identifier : IDENTIFIER
                               | IDENTIFIER OF IDENTIFIER
                               | IDENTIFIER IN IDENTIFIER"""
        if len(p) == 2:
            p[0] = p[1]
        else:
            p[0] = f"{p[1]} {p[2]} {p[3]}"
    
    def p_identifier_list(self, p):
        """identifier_list : qualified_identifier
                          | identifier_list qualified_identifier
                          | identifier_list COMMA qualified_identifier"""
        if len(p) == 2:
            p[0] = [p[1]]
        elif len(p) == 3:
            p[1].append(p[2])
            p[0] = p[1]
        else:
            p[1].append(p[3])
            p[0] = p[1]
    
    def p_display_statement(self, p):
        """display_statement : DISPLAY display_items display_options PERIOD"""
        stmt = StatementNode("DISPLAY", line_number=p.lineno(1))
        stmt.set_attribute("items", p[2])
        stmt.set_attribute("options", p[3])
        p[0] = stmt
    
    def p_display_items(self, p):
        """display_items : display_item
                        | display_items display_item"""
        if len(p) == 2:
            p[0] = [p[1]]
        else:
            p[1].append(p[2])
            p[0] = p[1]
    
    def p_display_item(self, p):
        """display_item : STRING_LITERAL
                       | qualified_identifier
                       | literal"""
        p[0] = p[1]
    
    def p_display_options(self, p):
        """display_options : WITH NO ADVANCING
                          | empty"""
        if len(p) > 2:
            p[0] = "NO ADVANCING"
        else:
            p[0] = None
    
    def p_accept_statement(self, p):
        """accept_statement : ACCEPT qualified_identifier PERIOD
                           | ACCEPT qualified_identifier FROM IDENTIFIER PERIOD"""
        stmt = StatementNode("ACCEPT", line_number=p.lineno(1))
        stmt.set_attribute("target", p[2])
        if len(p) > 4:
            stmt.set_attribute("from", p[4])
        p[0] = stmt
    
    def p_perform_statement(self, p):
        """perform_statement : PERFORM paragraph_name PERIOD
                            | PERFORM paragraph_name THROUGH paragraph_name PERIOD
                            | PERFORM paragraph_name THRU paragraph_name PERIOD
                            | PERFORM paragraph_name UNTIL condition PERIOD
                            | PERFORM paragraph_name VARYING IDENTIFIER FROM identifier_or_literal BY identifier_or_literal UNTIL condition PERIOD"""
        stmt = StatementNode("PERFORM", line_number=p.lineno(1))
        stmt.set_attribute("procedure", p[2])
        
        if len(p) > 4:
            if p[3] in ['THROUGH', 'THRU']:
                stmt.set_attribute("through", p[4])
            elif p[3] == 'UNTIL':
                stmt.set_attribute("until", p[4])
            elif p[3] == 'VARYING':
                stmt.set_attribute("varying", {
                    "variable": p[4],
                    "from": p[6],
                    "by": p[8],
                    "until": p[10]
                })
        
        # Traccia le chiamate tra procedure
        if p[2] in self.procedures:
            self.procedures[p[2]].called_by.append(self.current_section)
        
        p[0] = stmt
    
    def p_condition(self, p):
        """condition : simple_condition
                    | condition OR simple_condition
                    | condition AND simple_condition
                    | NOT condition
                    | LPAREN condition RPAREN"""
        if len(p) == 2:
            p[0] = p[1]
        elif len(p) == 3:
            p[0] = f"NOT {p[2]}"
        elif len(p) == 4:
            if p[1] == '(':
                p[0] = f"({p[2]})"
            else:
                p[0] = f"{p[1]} {p[2]} {p[3]}"
        else:
            p[0] = p[1]
    
    def p_simple_condition(self, p):
        """simple_condition : identifier_or_literal comparison_op identifier_or_literal
                           | identifier_or_literal IS NOT comparison_op identifier_or_literal
                           | identifier_or_literal"""
        if len(p) == 2:
            p[0] = p[1]
        elif len(p) == 4:
            p[0] = f"{p[1]} {p[2]} {p[3]}"
        else:
            p[0] = f"{p[1]} NOT {p[4]} {p[5]}"
    
    def p_comparison_op(self, p):
        """comparison_op : EQUALS
                        | EQUAL
                        | GREATER THAN
                        | LESS THAN
                        | GREATER_OP
                        | LESS_OP
                        | NOT_EQUAL
                        | GREATER_EQUAL
                        | LESS_EQUAL"""
        if len(p) == 2:
            p[0] = p[1]
        else:
            p[0] = f"{p[1]} {p[2]}"
    
    def p_if_statement(self, p):
        """if_statement : IF condition if_body END_IF PERIOD
                       | IF condition if_body ELSE else_body END_IF PERIOD"""
        stmt = StatementNode("IF", line_number=p.lineno(1))
        stmt.set_attribute("condition", p[2])
        stmt.set_attribute("then_statements", p[3])
        
        if len(p) == 8:  # Con ELSE
            stmt.set_attribute("else_statements", p[5])
        
        p[0] = stmt
    
    def p_if_body(self, p):
        """if_body : statements
                  | THEN statements"""
        if len(p) == 2:
            p[0] = p[1]
        else:
            p[0] = p[2]
    
    def p_else_body(self, p):
        """else_body : statements"""
        p[0] = p[1]
    
    def p_evaluate_statement(self, p):
        """evaluate_statement : EVALUATE identifier_or_literal when_clauses END_EVALUATE PERIOD"""
        stmt = StatementNode("EVALUATE", line_number=p.lineno(1))
        stmt.set_attribute("variable", p[2])
        stmt.set_attribute("when_clauses", p[3])
        p[0] = stmt
    
    def p_when_clauses(self, p):
        """when_clauses : when_clause
                       | when_clauses when_clause"""
        if len(p) == 2:
            p[0] = [p[1]]
        else:
            p[1].append(p[2])
            p[0] = p[1]
    
    def p_when_clause(self, p):
        """when_clause : WHEN when_value statements
                      | WHEN OTHER statements"""
        p[0] = {
            "value": p[2],
            "statements": p[3]
        }
    
    def p_when_value(self, p):
        """when_value : literal
                     | identifier_or_literal
                     | when_value THRU literal
                     | when_value THROUGH literal"""
        if len(p) == 2:
            p[0] = p[1]
        else:
            p[0] = f"{p[1]} THRU {p[3]}"
    
    def p_compute_statement(self, p):
        """compute_statement : COMPUTE qualified_identifier EQUALS arithmetic_expression PERIOD
                            | COMPUTE qualified_identifier ROUNDED EQUALS arithmetic_expression PERIOD"""
        stmt = StatementNode("COMPUTE", line_number=p.lineno(1))
        stmt.set_attribute("target", p[2])
        
        if len(p) == 6:
            stmt.set_attribute("expression", p[4])
        else:
            stmt.set_attribute("expression", p[5])
            stmt.set_attribute("rounded", True)
        
        p[0] = stmt
    
    def p_arithmetic_expression(self, p):
        """arithmetic_expression : arithmetic_term
                                | arithmetic_expression PLUS arithmetic_term
                                | arithmetic_expression MINUS arithmetic_term"""
        if len(p) == 2:
            p[0] = p[1]
        else:
            p[0] = f"{p[1]} {p[2]} {p[3]}"
    
    def p_arithmetic_term(self, p):
        """arithmetic_term : arithmetic_factor
                          | arithmetic_term MULTIPLY_OP arithmetic_factor
                          | arithmetic_term DIVIDE_OP arithmetic_factor"""
        if len(p) == 2:
            p[0] = p[1]
        else:
            p[0] = f"{p[1]} {p[2]} {p[3]}"
    
    def p_arithmetic_factor(self, p):
        """arithmetic_factor : qualified_identifier
                            | NUMBER
                            | LPAREN arithmetic_expression RPAREN
                            | FUNCTION function_name LPAREN function_args_opt RPAREN"""
        if len(p) == 2:
            p[0] = str(p[1])
        elif len(p) == 4:
            p[0] = f"({p[2]})"
        else:
            args = p[4] if p[4] else ""
            p[0] = f"FUNCTION {p[2]}({args})"
    
    def p_function_name(self, p):
        """function_name : IDENTIFIER
                        | CURRENT_DATE
                        | LENGTH
                        | MAX
                        | MIN"""
        p[0] = p[1]
    
    def p_function_args_opt(self, p):
        """function_args_opt : function_args
                            | empty"""
        p[0] = p[1]
    
    def p_function_args(self, p):
        """function_args : identifier_or_literal
                        | function_args COMMA identifier_or_literal"""
        if len(p) == 2:
            p[0] = str(p[1])
        else:
            p[0] = f"{p[1]}, {p[3]}"
    
    def p_add_statement(self, p):
        """add_statement : ADD identifier_or_literal TO qualified_identifier PERIOD
                        | ADD identifier_or_literal TO qualified_identifier GIVING qualified_identifier PERIOD"""
        stmt = StatementNode("ADD", line_number=p.lineno(1))
        stmt.set_attribute("source", p[2])
        stmt.set_attribute("target", p[4])
        
        if len(p) > 6:
            stmt.set_attribute("giving", p[6])
        
        p[0] = stmt
    
    def p_subtract_statement(self, p):
        """subtract_statement : SUBTRACT identifier_or_literal FROM qualified_identifier PERIOD
                             | SUBTRACT identifier_or_literal FROM qualified_identifier GIVING qualified_identifier PERIOD"""
        stmt = StatementNode("SUBTRACT", line_number=p.lineno(1))
        stmt.set_attribute("source", p[2])
        stmt.set_attribute("target", p[4])
        
        if len(p) > 6:
            stmt.set_attribute("giving", p[6])
        
        p[0] = stmt
    
    def p_multiply_statement(self, p):
        """multiply_statement : MULTIPLY identifier_or_literal BY qualified_identifier PERIOD
                             | MULTIPLY identifier_or_literal BY qualified_identifier GIVING qualified_identifier PERIOD"""
        stmt = StatementNode("MULTIPLY", line_number=p.lineno(1))
        stmt.set_attribute("source", p[2])
        stmt.set_attribute("target", p[4])
        
        if len(p) > 6:
            stmt.set_attribute("giving", p[6])
        
        p[0] = stmt
    
    def p_divide_statement(self, p):
        """divide_statement : DIVIDE identifier_or_literal INTO qualified_identifier PERIOD
                           | DIVIDE identifier_or_literal BY qualified_identifier GIVING qualified_identifier PERIOD"""
        stmt = StatementNode("DIVIDE", line_number=p.lineno(1))
        stmt.set_attribute("source", p[2])
        
        if p[3] == 'INTO':
            stmt.set_attribute("target", p[4])
        else:
            stmt.set_attribute("target", p[4])
            stmt.set_attribute("giving", p[6])
        
        p[0] = stmt
    
    def p_string_statement(self, p):
        """string_statement : STRING string_items DELIMITED BY string_delimiter INTO qualified_identifier PERIOD"""
        stmt = StatementNode("STRING", line_number=p.lineno(1))
        stmt.set_attribute("items", p[2])
        stmt.set_attribute("delimiter", p[5])
        stmt.set_attribute("target", p[7])
        p[0] = stmt
    
    def p_string_items(self, p):
        """string_items : string_item
                       | string_items string_item"""
        if len(p) == 2:
            p[0] = [p[1]]
        else:
            p[1].append(p[2])
            p[0] = p[1]
    
    def p_string_item(self, p):
        """string_item : qualified_identifier
                      | STRING_LITERAL
                      | qualified_identifier LPAREN NUMBER COLON NUMBER RPAREN
                      | qualified_identifier LPAREN NUMBER COLON RPAREN"""
        if len(p) == 2:
            p[0] = p[1]
        elif len(p) == 7:
            p[0] = f"{p[1]}({p[3]}:{p[5]})"
        else:
            p[0] = f"{p[1]}({p[3]}:)"
    
    def p_string_delimiter(self, p):
        """string_delimiter : SIZE
                           | STRING_LITERAL
                           | qualified_identifier"""
        p[0] = p[1]
    
    def p_exit_statement(self, p):
        """exit_statement : EXIT PARAGRAPH PERIOD
                         | EXIT PERIOD"""
        stmt = StatementNode("EXIT", line_number=p.lineno(1))
        if len(p) == 4:
            stmt.set_attribute("type", "PARAGRAPH")
        p[0] = stmt
    
    def p_continue_statement(self, p):
        """continue_statement : CONTINUE PERIOD"""
        stmt = StatementNode("CONTINUE", line_number=p.lineno(1))
        p[0] = stmt
    
    def p_call_statement(self, p):
        """call_statement : CALL STRING_LITERAL PERIOD
                         | CALL STRING_LITERAL USING identifier_list PERIOD
                         | CALL IDENTIFIER PERIOD
                         | CALL IDENTIFIER USING identifier_list PERIOD"""
        stmt = StatementNode("CALL", line_number=p.lineno(1))
        stmt.set_attribute("program", p[2])
        
        if len(p) > 4 and p[3] == 'USING':
            stmt.set_attribute("using", p[4])
        
        p[0] = stmt
    
    def p_stop_statement(self, p):
        """stop_statement : STOP RUN PERIOD"""
        stmt = StatementNode("STOP_RUN", line_number=p.lineno(1))
        p[0] = stmt
    
    def p_open_statement(self, p):
        """open_statement : OPEN open_mode qualified_identifier PERIOD
                         | OPEN open_mode file_list PERIOD"""
        stmt = StatementNode("OPEN", line_number=p.lineno(1))
        stmt.set_attribute("mode", p[2])
        stmt.set_attribute("files", p[3] if isinstance(p[3], list) else [p[3]])
        p[0] = stmt
    
    def p_open_mode(self, p):
        """open_mode : INPUT
                    | OUTPUT
                    | I_O
                    | EXTEND"""
        p[0] = p[1]
    
    def p_file_list(self, p):
        """file_list : qualified_identifier
                    | file_list qualified_identifier
                    | file_list COMMA qualified_identifier"""
        if len(p) == 2:
            p[0] = [p[1]]
        elif len(p) == 3:
            p[1].append(p[2])
            p[0] = p[1]
        else:
            p[1].append(p[3])
            p[0] = p[1]
    
    def p_close_statement(self, p):
        """close_statement : CLOSE qualified_identifier PERIOD
                          | CLOSE file_list PERIOD"""
        stmt = StatementNode("CLOSE", line_number=p.lineno(1))
        stmt.set_attribute("files", p[2] if isinstance(p[2], list) else [p[2]])
        p[0] = stmt
    
    def p_read_statement(self, p):
        """read_statement : READ qualified_identifier PERIOD
                         | READ qualified_identifier INTO qualified_identifier PERIOD
                         | READ qualified_identifier AT END statements END_READ PERIOD"""
        stmt = StatementNode("READ", line_number=p.lineno(1))
        stmt.set_attribute("file", p[2])
        
        if len(p) > 4:
            if p[3] == 'INTO':
                stmt.set_attribute("into", p[4])
            elif p[3] == 'AT':
                stmt.set_attribute("at_end", p[5])
        
        p[0] = stmt
    
    def p_write_statement(self, p):
        """write_statement : WRITE qualified_identifier PERIOD
                          | WRITE qualified_identifier FROM qualified_identifier PERIOD"""
        stmt = StatementNode("WRITE", line_number=p.lineno(1))
        stmt.set_attribute("record", p[2])
        
        if len(p) > 4:
            stmt.set_attribute("from", p[4])
        
        p[0] = stmt
    
    # SQL Statement handling
    def p_exec_sql_statement(self, p):
        """exec_sql_statement : EXEC SQL sql_statements END_EXEC PERIOD
                             | EXEC SQL sql_statements END_EXEC"""
        p[0] = p[3]  # sql_statements già restituisce SqlStatementNode
    
    def p_sql_statements(self, p):
        """sql_statements : sql_statement
                         | connect_statement
                         | disconnect_statement"""
        p[0] = p[1]
    
    def p_sql_statement(self, p):
        """sql_statement : sql_select
                        | sql_insert
                        | sql_update
                        | sql_delete
                        | sql_declare_cursor
                        | sql_open_cursor
                        | sql_fetch_cursor
                        | sql_close_cursor
                        | sql_commit
                        | sql_rollback"""
        p[0] = p[1]
    
    def p_sql_select(self, p):
        """sql_select : SELECT select_list INTO host_variable_list FROM table_reference where_clause_opt order_by_opt"""
        sql_node = SqlStatementNode("SELECT", line_number=p.lineno(1))
        sql_node.sql_text = f"SELECT {p[2]} INTO {', '.join(p[4])} FROM {p[6]}"
        sql_node.into_variables = p[4]
        sql_node.table_name = p[6]
        
        if p[7]:
            sql_node.sql_text += f" WHERE {p[7]}"
        if p[8]:
            sql_node.sql_text += f" {p[8]}"
        
        p[0] = sql_node
    
    def p_select_list(self, p):
        """select_list : select_item
                      | select_list COMMA select_item"""
        if len(p) == 2:
            p[0] = str(p[1])
        else:
            p[0] = f"{p[1]}, {p[3]}"
    
    def p_select_item(self, p):
        """select_item : IDENTIFIER
                      | IDENTIFIER PERIOD IDENTIFIER
                      | MULTIPLY_OP
                      | sql_function"""
        if len(p) == 2:
            p[0] = p[1]
        elif len(p) == 4:
            p[0] = f"{p[1]}.{p[3]}"
        else:
            p[0] = p[1]
    
    def p_sql_function(self, p):
        """sql_function : COUNT LPAREN MULTIPLY_OP RPAREN
                       | COUNT LPAREN IDENTIFIER RPAREN
                       | MAX LPAREN IDENTIFIER RPAREN
                       | MIN LPAREN IDENTIFIER RPAREN
                       | SUM LPAREN IDENTIFIER RPAREN
                       | AVG LPAREN IDENTIFIER RPAREN
                       | COALESCE LPAREN sql_expression_list RPAREN
                       | CAST LPAREN sql_expression AS sql_data_type RPAREN
                       | SUBSTR LPAREN IDENTIFIER COMMA NUMBER RPAREN
                       | SUBSTR LPAREN IDENTIFIER COMMA NUMBER COMMA NUMBER RPAREN
                       | LPAD LPAREN sql_expression_list RPAREN
                       | IDENTIFIER CONCATENATE sql_expression
                       | STRING_LITERAL CONCATENATE sql_expression"""
        
        if p[1] in ['COUNT', 'MAX', 'MIN', 'SUM', 'AVG']:
            p[0] = f"{p[1]}({p[3]})"
        elif p[1] == 'COALESCE':
            p[0] = f"COALESCE({p[3]})"
        elif p[1] == 'CAST':
            p[0] = f"CAST({p[3]} AS {p[5]})"
        elif p[1] == 'SUBSTR':
            if len(p) == 7:
                p[0] = f"SUBSTR({p[3]}, {p[5]})"
            else:
                p[0] = f"SUBSTR({p[3]}, {p[5]}, {p[7]})"
        elif p[1] == 'LPAD':
            p[0] = f"LPAD({p[3]})"
        elif p[2] == '||':
            # Concatenazione
            p[0] = f"{p[1]} || {p[3]}"
        else:
            # Default
            p[0] = str(p[1])
    
    def p_sql_data_type(self, p):
        """sql_data_type : INTEGER
                        | VARCHAR
                        | IDENTIFIER"""
        p[0] = p[1]
    
    def p_sql_expression(self, p):
        """sql_expression : sql_term
                         | sql_expression PLUS sql_term
                         | sql_expression MINUS sql_term
                         | sql_expression CONCATENATE sql_term"""
        if len(p) == 2:
            p[0] = str(p[1])
        else:
            p[0] = f"{p[1]} {p[2]} {p[3]}"
    
    def p_sql_term(self, p):
        """sql_term : sql_factor
                   | sql_term MULTIPLY_OP sql_factor
                   | sql_term DIVIDE_OP sql_factor"""
        if len(p) == 2:
            p[0] = p[1]
        else:
            p[0] = f"{p[1]} {p[2]} {p[3]}"
    
    def p_sql_factor(self, p):
        """sql_factor : IDENTIFIER
                     | IDENTIFIER PERIOD IDENTIFIER
                     | NUMBER
                     | STRING_LITERAL
                     | HOST_VARIABLE
                     | sql_function
                     | LPAREN sql_expression RPAREN"""
        if len(p) == 2:
            p[0] = str(p[1])
        elif len(p) == 4 and p[1] != '(':
            p[0] = f"{p[1]}.{p[3]}"
        else:
            p[0] = f"({p[2]})"
    
    def p_sql_expression_list(self, p):
        """sql_expression_list : sql_expression
                              | sql_expression_list COMMA sql_expression"""
        if len(p) == 2:
            p[0] = str(p[1])
        else:
            p[0] = f"{p[1]}, {p[3]}"
    
    def p_table_reference(self, p):
        """table_reference : IDENTIFIER
                          | IDENTIFIER IDENTIFIER"""
        if len(p) == 2:
            p[0] = p[1]
        else:
            p[0] = f"{p[1]} {p[2]}"
    
    def p_host_variable_list(self, p):
        """host_variable_list : HOST_VARIABLE
                             | host_variable_list COMMA HOST_VARIABLE"""
        if len(p) == 2:
            p[0] = [p[1]]
        else:
            p[1].append(p[3])
            p[0] = p[1]
    
    def p_where_clause_opt(self, p):
        """where_clause_opt : WHERE sql_condition
                           | empty"""
        p[0] = p[2] if len(p) > 2 else None
    
    def p_sql_condition(self, p):
        """sql_condition : sql_simple_condition
                        | sql_condition AND sql_simple_condition
                        | sql_condition OR sql_simple_condition"""
        if len(p) == 2:
            p[0] = p[1]
        else:
            p[0] = f"{p[1]} {p[2]} {p[3]}"
    
    def p_sql_simple_condition(self, p):
        """sql_simple_condition : sql_expression sql_comparison_op sql_expression
                               | sql_expression IS NULL
                               | sql_expression IS NOT NULL
                               | sql_expression LIKE STRING_LITERAL
                               | sql_expression IN LPAREN sql_expression_list RPAREN
                               | EXISTS LPAREN sql_select RPAREN"""
        if len(p) == 4:
            if p[2] == 'IS':
                p[0] = f"{p[1]} IS {p[3]}"
            else:
                p[0] = f"{p[1]} {p[2]} {p[3]}"
        elif len(p) == 5:
            p[0] = f"{p[1]} IS NOT {p[4]}"
        elif p[2] == 'IN':
            p[0] = f"{p[1]} IN ({p[4]})"
        elif p[1] == 'EXISTS':
            p[0] = f"EXISTS ({p[3]})"
    
    def p_sql_comparison_op(self, p):
        """sql_comparison_op : EQUALS
                            | GREATER_OP
                            | LESS_OP
                            | NOT_EQUAL
                            | GREATER_EQUAL
                            | LESS_EQUAL"""
        p[0] = p[1]
    
    def p_order_by_opt(self, p):
        """order_by_opt : ORDER BY order_item_list
                       | empty"""
        if len(p) > 2:
            p[0] = f"ORDER BY {p[3]}"
        else:
            p[0] = None
    
    def p_order_item_list(self, p):
        """order_item_list : order_item
                          | order_item_list COMMA order_item"""
        if len(p) == 2:
            p[0] = p[1]
        else:
            p[0] = f"{p[1]}, {p[3]}"
    
    def p_order_item(self, p):
        """order_item : IDENTIFIER
                     | IDENTIFIER ASC
                     | IDENTIFIER DESC"""
        if len(p) == 2:
            p[0] = p[1]
        else:
            p[0] = f"{p[1]} {p[2]}"
    
    def p_sql_insert(self, p):
        """sql_insert : INSERT INTO IDENTIFIER LPAREN column_list RPAREN VALUES LPAREN value_list RPAREN
                     | INSERT INTO IDENTIFIER VALUES LPAREN value_list RPAREN"""
        sql_node = SqlStatementNode("INSERT", line_number=p.lineno(1))
        sql_node.table_name = p[3]
        
        if len(p) == 11:
            sql_node.sql_text = f"INSERT INTO {p[3]} ({p[5]}) VALUES ({p[9]})"
        else:
            sql_node.sql_text = f"INSERT INTO {p[3]} VALUES ({p[6]})"
        
        p[0] = sql_node
    
    def p_column_list(self, p):
        """column_list : IDENTIFIER
                      | column_list COMMA IDENTIFIER"""
        if len(p) == 2:
            p[0] = p[1]
        else:
            p[0] = f"{p[1]}, {p[3]}"
    
    def p_value_list(self, p):
        """value_list : value_item
                     | value_list COMMA value_item"""
        if len(p) == 2:
            p[0] = str(p[1])
        else:
            p[0] = f"{p[1]}, {p[3]}"
    
    def p_value_item(self, p):
        """value_item : HOST_VARIABLE
                     | STRING_LITERAL
                     | NUMBER
                     | NULL
                     | CURRENT_DATE
                     | CURRENT_TIMESTAMP
                     | sql_function"""
        p[0] = p[1]
    
    def p_sql_update(self, p):
        """sql_update : UPDATE IDENTIFIER SET set_clause_list where_clause_opt"""
        sql_node = SqlStatementNode("UPDATE", line_number=p.lineno(1))
        sql_node.table_name = p[2]
        sql_node.sql_text = f"UPDATE {p[2]} SET {p[4]}"
        
        if p[5]:
            sql_node.sql_text += f" WHERE {p[5]}"
        
        p[0] = sql_node
    
    def p_set_clause_list(self, p):
        """set_clause_list : set_clause
                          | set_clause_list COMMA set_clause"""
        if len(p) == 2:
            p[0] = p[1]
        else:
            p[0] = f"{p[1]}, {p[3]}"
    
    def p_set_clause(self, p):
        """set_clause : IDENTIFIER EQUALS value_item
                     | IDENTIFIER EQUALS sql_expression"""
        p[0] = f"{p[1]} = {p[3]}"
    
    def p_sql_delete(self, p):
        """sql_delete : DELETE FROM IDENTIFIER where_clause_opt"""
        sql_node = SqlStatementNode("DELETE", line_number=p.lineno(1))
        sql_node.table_name = p[3]
        sql_node.sql_text = f"DELETE FROM {p[3]}"
        
        if p[4]:
            sql_node.sql_text += f" WHERE {p[4]}"
        
        p[0] = sql_node
    
    def p_sql_declare_cursor(self, p):
        """sql_declare_cursor : DECLARE IDENTIFIER CURSOR FOR select_statement"""
        sql_node = SqlStatementNode("DECLARE_CURSOR", line_number=p.lineno(1))
        sql_node.cursor_name = p[2]
        sql_node.is_cursor = True
        sql_node.sql_text = f"DECLARE {p[2]} CURSOR FOR {p[5]}"
        p[0] = sql_node
    
    def p_select_statement(self, p):
        """select_statement : SELECT select_list FROM table_reference where_clause_opt order_by_opt
                           | SELECT select_list FROM table_reference join_clause where_clause_opt order_by_opt"""
        stmt = f"SELECT {p[2]} FROM {p[4]}"
        
        if len(p) == 7:
            if p[5]:
                stmt += f" WHERE {p[5]}"
            if p[6]:
                stmt += f" {p[6]}"
        else:
            stmt += f" {p[5]}"
            if p[6]:
                stmt += f" WHERE {p[6]}"
            if p[7]:
                stmt += f" {p[7]}"
        
        p[0] = stmt
    
    def p_join_clause(self, p):
        """join_clause : join_type JOIN table_reference ON join_condition
                      | join_type JOIN table_reference ON join_condition join_clause"""
        if len(p) == 6:
            p[0] = f"{p[1]} JOIN {p[3]} ON {p[5]}"
        else:
            p[0] = f"{p[1]} JOIN {p[3]} ON {p[5]} {p[6]}"
    
    def p_join_type(self, p):
        """join_type : empty
                    | INNER
                    | LEFT
                    | RIGHT
                    | FULL"""
        p[0] = p[1] if p[1] else ""
    
    def p_join_condition(self, p):
        """join_condition : IDENTIFIER EQUALS IDENTIFIER
                         | IDENTIFIER PERIOD IDENTIFIER EQUALS IDENTIFIER PERIOD IDENTIFIER"""
        if len(p) == 4:
            p[0] = f"{p[1]} = {p[3]}"
        else:
            p[0] = f"{p[1]}.{p[3]} = {p[5]}.{p[7]}"
    
    def p_sql_open_cursor(self, p):
        """sql_open_cursor : OPEN IDENTIFIER"""
        sql_node = SqlStatementNode("OPEN_CURSOR", line_number=p.lineno(1))
        sql_node.cursor_name = p[2]
        sql_node.sql_text = f"OPEN {p[2]}"
        p[0] = sql_node
    
    def p_sql_fetch_cursor(self, p):
        """sql_fetch_cursor : FETCH IDENTIFIER INTO host_variable_list"""
        sql_node = SqlStatementNode("FETCH", line_number=p.lineno(1))
        sql_node.cursor_name = p[2]
        sql_node.into_variables = p[4]
        sql_node.sql_text = f"FETCH {p[2]} INTO {', '.join(p[4])}"
        p[0] = sql_node
    
    def p_sql_close_cursor(self, p):
        """sql_close_cursor : CLOSE IDENTIFIER"""
        sql_node = SqlStatementNode("CLOSE_CURSOR", line_number=p.lineno(1))
        sql_node.cursor_name = p[2]
        sql_node.sql_text = f"CLOSE {p[2]}"
        p[0] = sql_node
    
    def p_sql_commit(self, p):
        """sql_commit : COMMIT"""
        sql_node = SqlStatementNode("COMMIT", line_number=p.lineno(1))
        sql_node.sql_text = "COMMIT"
        p[0] = sql_node
    
    def p_sql_rollback(self, p):
        """sql_rollback : ROLLBACK"""
        sql_node = SqlStatementNode("ROLLBACK", line_number=p.lineno(1))
        sql_node.sql_text = "ROLLBACK"
        p[0] = sql_node
    
    def p_connect_statement(self, p):
        """connect_statement : CONNECT TO STRING_LITERAL
                            | CONNECT TO STRING_LITERAL USER STRING_LITERAL
                            | CONNECT TO STRING_LITERAL USER STRING_LITERAL USING STRING_LITERAL"""
        sql_node = SqlStatementNode("CONNECT", line_number=p.lineno(1))
        
        if len(p) == 4:
            sql_node.sql_text = f"CONNECT TO {p[3]}"
        elif len(p) == 6:
            sql_node.sql_text = f"CONNECT TO {p[3]} USER {p[5]}"
        else:
            sql_node.sql_text = f"CONNECT TO {p[3]} USER {p[5]} USING {p[7]}"
        
        p[0] = sql_node
    
    def p_disconnect_statement(self, p):
        """disconnect_statement : DISCONNECT ALL
                               | DISCONNECT CURRENT
                               | DISCONNECT"""
        sql_node = SqlStatementNode("DISCONNECT", line_number=p.lineno(1))
        
        if len(p) == 3:
            sql_node.sql_text = f"DISCONNECT {p[2]}"
        else:
            sql_node.sql_text = "DISCONNECT"
        
        p[0] = sql_node
    
    def p_empty(self, p):
        """empty :"""
        pass
    
    def p_error(self, p):
        if p:
            logger.error(f"Errore di sintassi alla linea {p.lineno}: token '{p.value}' di tipo '{p.type}'")
            logger.error(f"Context: {self._get_error_context(p)}")
        else:
            logger.error("Errore di sintassi: fine del file inaspettata")
    
    def _get_error_context(self, p):
        """Ottiene il contesto dell'errore per debugging"""
        if not p:
            return "EOF"
        
        # Mostra i token vicini
        context = []
        lexer = p.lexer
        
        # Prova a ottenere i prossimi token
        for i in range(5):
            tok = lexer.token()
            if not tok:
                break
            context.append(f"{tok.type}:{tok.value}")
        
        return " ".join(context)
    
    def _analyze_picture(self, pic_string):
        """Analizza una PICTURE clause e determina il tipo Java"""
        if not pic_string:
            return "String", 1, 0
            
        pic = pic_string.upper()
        
        # Determina se è numerico o alfanumerico
        if 'X' in pic or 'A' in pic:
            # Stringa
            size = self._calculate_pic_size(pic)
            return "String", size, 0
        elif '9' in pic:
            # Numerico
            size = self._calculate_pic_size(pic)
            decimals = 0
            
            if 'V' in pic:
                # Ha decimali
                parts = pic.split('V')
                if len(parts) > 1:
                    decimals = self._calculate_pic_size(parts[1])
                return "BigDecimal", size, decimals
            elif 'S' in pic:
                # Con segno
                if size <= 9:
                    return "int", size, 0
                else:
                    return "long", size, 0
            else:
                # Senza segno
                if size <= 9:
                    return "int", size, 0
                else:
                    return "long", size, 0
        
        return "String", 1, 0
    
    def _calculate_pic_size(self, pic_string):
        """Calcola la dimensione da una PICTURE string"""
        import re
        
        # Gestisce formati come 9(10) o X(50)
        match = re.search(r'[9XAS]\((\d+)\)', pic_string)
        if match:
            return int(match.group(1))
        
        # Conta i caratteri singoli
        count = 0
        for char in pic_string:
            if char in '9XAS':
                count += 1
        
        return count if count > 0 else 1
    
    def build(self, **kwargs):
        """Costruisce il parser"""
        self.parser = yacc.yacc(module=self, **kwargs)
        return self.parser
    
    def parse(self, data, debug=False):
        """Parsa il codice COBOL e restituisce l'AST"""
        try:
            # Preprocessa il codice COBOL
            processed_data = self._preprocess_cobol(data)
            
            if debug:
                logger.debug("Codice preprocessato:")
                for i, line in enumerate(processed_data.split('\n'), 1):
                    logger.debug(f"{i:4d}: {line}")
            
            # Resetta lo stato
            self.data_items = {}
            self.procedures = {}
            self.current_division = None
            self.current_section = None
            
            # Parsa
            result = self.parser.parse(processed_data, lexer=self.lexer.lexer, 
                                     debug=debug)
            
            if result:
                logger.info("Parsing completato con successo")
                if debug:
                    self._print_ast(result)
            else:
                logger.error("Parsing fallito: nessun risultato")
                
            return result
            
        except Exception as e:
            logger.error(f"Errore durante il parsing: {str(e)}")
            import traceback
            logger.debug(traceback.format_exc())
            raise
    
    def _preprocess_cobol(self, data):
        """Preprocessa il codice COBOL per gestire il formato fisso"""
        lines = data.split('\n')
        processed_lines = []
        in_exec_sql = False
        continuation_line = ""
        
        for line_num, line in enumerate(lines, 1):
            # Ignora righe vuote
            if not line.strip():
                continue
            
            # Gestisci formato fisso COBOL
            if len(line) > 6:
                # Controlla indicatore in colonna 7
                if line[6] == '*':
                    # Commento
                    continue
                elif line[6] == '-':
                    # Continuazione
                    if continuation_line:
                        continuation_line = continuation_line.rstrip() + ' ' + line[7:].lstrip()
                        continue
                
                # Linea normale
                if continuation_line:
                    processed_lines.append(continuation_line)
                    continuation_line = ""
                
                # Prendi dalla colonna 7 alla 72
                if len(line) > 72:
                    line = line[6:72]
                else:
                    line = line[6:]
            
            # Gestisci EXEC SQL in modo speciale
            if 'EXEC SQL' in line:
                in_exec_sql = True
            
            if in_exec_sql:
                # In SQL, preserva underscore e formattazione
                processed_lines.append(line.rstrip())
                if 'END-EXEC' in line:
                    in_exec_sql = False
            else:
                # Per COBOL normale, processa normalmente
                processed_lines.append(line.rstrip())
        
        # Aggiungi l'ultima linea di continuazione se presente
        if continuation_line:
            processed_lines.append(continuation_line)
        
        return '\n'.join(processed_lines)
    
    def _print_ast(self, node, indent=0):
        """Stampa l'AST per debugging"""
        if not node:
            return
        
        prefix = "  " * indent
        
        if isinstance(node, ASTNode):
            logger.debug(f"{prefix}{node.node_type.value}: {node.name}")
            
            # Stampa attributi speciali
            if isinstance(node, DataItemNode):
                if node.picture:
                    logger.debug(f"{prefix}  PIC: {node.picture}")
                if node.value:
                    logger.debug(f"{prefix}  VALUE: {node.value}")
            elif isinstance(node, SqlStatementNode):
                logger.debug(f"{prefix}  SQL: {node.sql_text}")
            elif isinstance(node, StatementNode):
                for key, value in node.attributes.items():
                    logger.debug(f"{prefix}  {key}: {value}")
            
            # Stampa figli
            for child in node.children:
                self._print_ast(child, indent + 1)
        else:
            logger.debug(f"{prefix}{type(node).__name__}: {node}")