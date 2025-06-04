"""
COBOL Parser - Costruisce l'AST dal codice COBOL tokenizzato
"""

import ply.yacc as yacc
from loguru import logger
from .cobol_lexer import CobolLexer
from .ast_nodes import *


class CobolParser:
    """Parser per il linguaggio COBOL"""
    
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
        """program : identification_division environment_division_opt data_division_opt procedure_division"""
        program_node = p[1]  # identification_division returns ProgramNode
        
        # Aggiungi le altre divisioni come figli
        if p[2]:  # environment_division
            program_node.add_child(p[2])
        if p[3]:  # data_division
            program_node.add_child(p[3])
        if p[4]:  # procedure_division
            program_node.add_child(p[4])
            
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
        """program_id_paragraph : PROGRAM_ID PERIOD IDENTIFIER PERIOD"""
        p[0] = ProgramNode(program_id=p[3], line_number=p.lineno(1))
    
    def p_author_paragraph_opt(self, p):
        """author_paragraph_opt : AUTHOR PERIOD author_name PERIOD
                               | empty"""
        if len(p) > 2:
            p[0] = p[3]
        else:
            p[0] = None
    
    def p_author_name(self, p):
        """author_name : IDENTIFIER
                      | IDENTIFIER MINUS IDENTIFIER"""
        if len(p) == 2:
            p[0] = p[1]
        else:
            p[0] = f"{p[1]}-{p[3]}"
    
    def p_date_written_paragraph_opt(self, p):
        """date_written_paragraph_opt : DATE_WRITTEN PERIOD date_value PERIOD
                                     | empty"""
        if len(p) > 2:
            p[0] = p[3]
        else:
            p[0] = None
    
    def p_date_value(self, p):
        """date_value : NUMBER MINUS NUMBER MINUS NUMBER
                     | IDENTIFIER"""
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
        """file_control_entry : SELECT IDENTIFIER ASSIGN TO STRING_LITERAL organization_clause_opt PERIOD"""

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
        for entry in p[4]:
            fd_node.add_child(entry)
        p[0] = fd_node
    
    def p_working_storage_section_opt(self, p):
        """working_storage_section_opt : WORKING_STORAGE SECTION PERIOD data_description_entries
                               | empty"""
        if len(p) > 2:
            section_node = ASTNode(NodeType.SECTION, "WORKING-STORAGE", 
                                 line_number=p.lineno(1))
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
                                   | data_description_entries data_description_entry"""
        if len(p) == 2:
            p[0] = [p[1]]
        else:
            p[1].append(p[2])
            p[0] = p[1]
    
    def p_data_description_entry(self, p):
        """data_description_entry : level_number data_name_or_filler picture_clause_opt value_clause_opt occurs_clause_opt PERIOD
                           | exec_sql_include"""

        if len(p) == 2:
            p[0] = p[1]  # exec_sql_include
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
        """level_number : LEVEL_NUMBER
                       | NUMBER"""
        p[0] = str(p[1])
    
    def p_data_name_or_filler(self, p):
        """data_name_or_filler : IDENTIFIER
                              | FILLER
                              | empty"""
        p[0] = p[1] if p[1] else "FILLER"
    
    def p_picture_clause_opt(self, p):
        """picture_clause_opt : PICTURE IS PICTURE_STRING
                             | PIC IS PICTURE_STRING
                             | PICTURE PICTURE_STRING
                             | PIC PICTURE_STRING
                             | empty"""
        if len(p) > 2:
            if len(p) == 4:
                p[0] = p[3]
            else:
                p[0] = p[2]
        else:
            p[0] = None
    
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
                  | SPACE
                  | SPACES
                  | ALL STRING_LITERAL"""
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
        """exec_sql_include : EXEC SQL INCLUDE SQLCA END_EXEC PERIOD"""
        sql_node = SqlStatementNode("INCLUDE", line_number=p.lineno(1))
        sql_node.sql_text = "INCLUDE SQLCA"
        p[0] = sql_node
    
    def p_procedure_division(self, p):
        """procedure_division : PROCEDURE DIVISION PERIOD procedure_content"""
        div_node = DivisionNode("PROCEDURE", line_number=p.lineno(1))
        for item in p[4]:
            div_node.add_child(item)
        p[0] = div_node
    
    def p_procedure_content(self, p):
        """procedure_content : paragraph_or_statement
                            | procedure_content paragraph_or_statement"""
        if len(p) == 2:
            p[0] = [p[1]]
        else:
            p[1].append(p[2])
            p[0] = p[1]
    
    def p_paragraph_or_statement(self, p):
        """paragraph_or_statement : paragraph
                                 | statement"""
        p[0] = p[1]
    
    def p_paragraph(self, p):
        """paragraph : IDENTIFIER PERIOD statements_opt"""
        para_node = ProcedureNode(p[1], line_number=p.lineno(1))
        if p[3]:
            para_node.statements = p[3]
        self.procedures[p[1]] = para_node
        p[0] = para_node
    
    def p_statements_opt(self, p):
        """statements_opt : statements
                         | empty"""
        p[0] = p[1] if p[1] else []
    
    def p_statements(self, p):
        """statements : statement
                     | statements statement"""
        if len(p) == 2:
            p[0] = [p[1]]
        else:
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
                    | open_statement
                    | close_statement
                    | write_statement
                    | string_statement
                    | exit_statement"""
        p[0] = p[1]

    def p_string_items(self, p):
        """string_items : string_item
                        | string_items string_item"""
        if len(p) == 2:
            p[0] = [p[1]]
        else:
            p[1].append(p[2])
            p[0] = p[1]

    def p_string_item(self, p):
        """string_item : IDENTIFIER
                    | STRING_LITERAL
                    | IDENTIFIER LPAREN NUMBER COLON NUMBER RPAREN"""
        if len(p) == 2:
            p[0] = p[1]
        else:
            p[0] = f"{p[1]}({p[3]}:{p[5]})"

    def p_string_statement(self, p):
        """string_statement : STRING string_items DELIMITED BY string_delimiter INTO IDENTIFIER PERIOD"""
        stmt = StatementNode("STRING", line_number=p.lineno(1))
        stmt.set_attribute("items", p[2])
        stmt.set_attribute("delimiter", p[5])
        stmt.set_attribute("target", p[7])
        p[0] = stmt

    def p_string_delimiter(self, p):
        """string_delimiter : SIZE
                        | STRING_LITERAL"""
        p[0] = p[1]

    def p_exit_statement(self, p):
        """exit_statement : EXIT PARAGRAPH PERIOD
                        | EXIT PERIOD"""
        stmt = StatementNode("EXIT", line_number=p.lineno(1))
        if len(p) == 4:
            stmt.set_attribute("type", "PARAGRAPH")
        p[0] = stmt

    def p_move_statement(self, p):
        """move_statement : MOVE identifier_or_literal TO identifier_list PERIOD"""
        stmt = StatementNode("MOVE", line_number=p.lineno(1))
        stmt.set_attribute("source", p[2])
        stmt.set_attribute("targets", p[4])
        p[0] = stmt
    
    def p_identifier_or_literal(self, p):
        """identifier_or_literal : IDENTIFIER
                                | literal"""
        p[0] = p[1]
    
    def p_identifier_list(self, p):
        """identifier_list : IDENTIFIER
                          | identifier_list IDENTIFIER"""
        if len(p) == 2:
            p[0] = [p[1]]
        else:
            p[1].append(p[2])
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
                       | IDENTIFIER"""
        p[0] = p[1]
    
    def p_display_options(self, p):
        """display_options : WITH NO ADVANCING
                          | empty"""
        if len(p) > 2:
            p[0] = "NO ADVANCING"
        else:
            p[0] = None
    
    def p_accept_statement(self, p):
        """accept_statement : ACCEPT IDENTIFIER PERIOD"""
        stmt = StatementNode("ACCEPT", line_number=p.lineno(1))
        stmt.set_attribute("target", p[2])
        p[0] = stmt
    
    def p_perform_statement(self, p):
        """perform_statement : PERFORM IDENTIFIER PERIOD
                            | PERFORM IDENTIFIER UNTIL condition PERIOD"""
        stmt = StatementNode("PERFORM", line_number=p.lineno(1))
        stmt.set_attribute("procedure", p[2])
        
        if len(p) > 4:
            stmt.set_attribute("until", p[4])
        
        # Traccia le chiamate tra procedure
        if p[2] in self.procedures:
            self.procedures[p[2]].called_by.append(self.current_section)
        
        p[0] = stmt
    
    def p_condition(self, p):
        """condition : simple_condition
                    | condition OR simple_condition
                    | condition AND simple_condition"""
        if len(p) == 2:
            p[0] = p[1]
        else:
            p[0] = f"{p[1]} {p[2]} {p[3]}"

    def p_simple_condition(self, p):
        """simple_condition : IDENTIFIER EQUALS literal
                        | IDENTIFIER NOT EQUALS literal
                        | IDENTIFIER GREATER THAN literal
                        | IDENTIFIER LESS THAN literal"""
        if len(p) == 4:
            p[0] = f"{p[1]} = {p[3]}"
        elif len(p) == 5 and p[2] == "NOT":
            p[0] = f"{p[1]} <> {p[4]}"
        else:
            op = ">" if p[2] == "GREATER" else "<"
            p[0] = f"{p[1]} {op} {p[4]}"
    
    def p_if_statement(self, p):
        """if_statement : IF condition statements END_IF PERIOD
                       | IF condition statements ELSE statements END_IF PERIOD"""
        stmt = StatementNode("IF", line_number=p.lineno(1))
        stmt.set_attribute("condition", p[2])
        stmt.set_attribute("then_statements", p[3])
        
        if len(p) == 8:  # Con ELSE
            stmt.set_attribute("else_statements", p[5])
        
        p[0] = stmt
    
    def p_evaluate_statement(self, p):
        """evaluate_statement : EVALUATE IDENTIFIER when_clauses END_EVALUATE PERIOD"""
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
        """when_clause : WHEN literal statements
                      | WHEN OTHER statements"""
        p[0] = {
            "value": p[2],
            "statements": p[3]
        }
    
    def p_exec_sql_statement(self, p):
        """exec_sql_statement : EXEC SQL sql_statement END_EXEC PERIOD"""
        p[0] = p[3]  # sql_statement già restituisce SqlStatementNode
    
    def p_sql_statement(self, p):
        """sql_statement : sql_select
                        | sql_insert
                        | sql_update
                        | sql_delete
                        | sql_connect
                        | sql_declare_cursor
                        | sql_open_cursor
                        | sql_fetch_cursor
                        | sql_close_cursor"""
        p[0] = p[1]
    
    def p_sql_select(self, p):
        """sql_select : SELECT select_list INTO host_variable_list FROM IDENTIFIER where_clause_opt"""
        sql_node = SqlStatementNode("SELECT", line_number=p.lineno(1))
        sql_node.sql_text = f"SELECT {p[2]} FROM {p[6]}"
        sql_node.into_variables = p[4]
        sql_node.table_name = p[6]
        
        if p[7]:
            sql_node.sql_text += f" WHERE {p[7]}"
        
        p[0] = sql_node
    
    def p_select_list(self, p):
        """select_list : IDENTIFIER
                      | select_list COMMA IDENTIFIER"""
        if len(p) == 2:
            p[0] = p[1]
        else:
            p[0] = f"{p[1]}, {p[3]}"
    
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
        """sql_condition : IDENTIFIER EQUALS HOST_VARIABLE
                        | IDENTIFIER EQUALS STRING_LITERAL"""
        p[0] = f"{p[1]} = {p[3]}"
    
    def p_sql_insert(self, p):
        """sql_insert : INSERT INTO IDENTIFIER LPAREN column_list RPAREN VALUES LPAREN value_list RPAREN"""
        sql_node = SqlStatementNode("INSERT", line_number=p.lineno(1))
        sql_node.table_name = p[3]
        sql_node.sql_text = f"INSERT INTO {p[3]} ({p[5]}) VALUES ({p[9]})"
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
                     | IDENTIFIER"""
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
        """set_clause : IDENTIFIER EQUALS value_item"""
        p[0] = f"{p[1]} = {p[3]}"
    
    def p_sql_delete(self, p):
        """sql_delete : DELETE FROM IDENTIFIER where_clause_opt"""
        sql_node = SqlStatementNode("DELETE", line_number=p.lineno(1))
        sql_node.table_name = p[3]
        sql_node.sql_text = f"DELETE FROM {p[3]}"
        
        if p[4]:
            sql_node.sql_text += f" WHERE {p[4]}"
        
        p[0] = sql_node
    
    def p_sql_connect(self, p):
        """sql_connect : CONNECT TO STRING_LITERAL USER STRING_LITERAL USING STRING_LITERAL"""
        sql_node = SqlStatementNode("CONNECT", line_number=p.lineno(1))
        sql_node.sql_text = f"CONNECT TO {p[3]} USER {p[5]} USING {p[7]}"
        p[0] = sql_node
    
    def p_sql_declare_cursor(self, p):
        """sql_declare_cursor : DECLARE IDENTIFIER CURSOR FOR select_statement"""
        sql_node = SqlStatementNode("DECLARE_CURSOR", line_number=p.lineno(1))
        sql_node.cursor_name = p[2]
        sql_node.is_cursor = True
        sql_node.sql_text = f"DECLARE {p[2]} CURSOR FOR {p[5]}"
        p[0] = sql_node
    
    def p_select_statement(self, p):
        """select_statement : SELECT select_list FROM IDENTIFIER where_clause_opt order_by_opt"""
        stmt = f"SELECT {p[2]} FROM {p[4]}"
        if p[5]:
            stmt += f" WHERE {p[5]}"
        if p[6]:
            stmt += f" {p[6]}"
        p[0] = stmt
    
    def p_order_by_opt(self, p):
        """order_by_opt : ORDER BY IDENTIFIER
                       | ORDER BY IDENTIFIER DESC
                       | empty"""
        if len(p) > 2:
            if len(p) == 4:
                p[0] = f"ORDER BY {p[3]}"
            else:
                p[0] = f"ORDER BY {p[3]} DESC"
        else:
            p[0] = None
    
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
    
    def p_stop_statement(self, p):
        """stop_statement : STOP RUN PERIOD"""
        stmt = StatementNode("STOP_RUN", line_number=p.lineno(1))
        p[0] = stmt
    
    def p_compute_statement(self, p):
        """compute_statement : COMPUTE IDENTIFIER EQUALS arithmetic_expression PERIOD"""
        stmt = StatementNode("COMPUTE", line_number=p.lineno(1))
        stmt.set_attribute("target", p[2])
        stmt.set_attribute("expression", p[4])
        p[0] = stmt
    
    def p_arithmetic_expression(self, p):
        """arithmetic_expression : IDENTIFIER
                                | NUMBER
                                | arithmetic_expression PLUS arithmetic_expression
                                | arithmetic_expression MINUS arithmetic_expression
                                | arithmetic_expression MULTIPLY_OP arithmetic_expression
                                | arithmetic_expression DIVIDE_OP arithmetic_expression"""
        if len(p) == 2:
            p[0] = str(p[1])
        else:
            p[0] = f"{p[1]} {p[2]} {p[3]}"
    
    def p_open_statement(self, p):
        """open_statement : OPEN open_mode IDENTIFIER PERIOD"""
        stmt = StatementNode("OPEN", line_number=p.lineno(1))
        stmt.set_attribute("mode", p[2])
        stmt.set_attribute("file", p[3])
        p[0] = stmt
    
    def p_open_mode(self, p):
        """open_mode : INPUT
                    | OUTPUT
                    | I_O
                    | EXTEND"""
        p[0] = p[1]
    
    def p_close_statement(self, p):
        """close_statement : CLOSE IDENTIFIER PERIOD"""
        stmt = StatementNode("CLOSE", line_number=p.lineno(1))
        stmt.set_attribute("file", p[2])
        p[0] = stmt
    
    def p_write_statement(self, p):
        """write_statement : WRITE IDENTIFIER PERIOD
                          | WRITE IDENTIFIER FROM IDENTIFIER PERIOD"""
        stmt = StatementNode("WRITE", line_number=p.lineno(1))
        stmt.set_attribute("record", p[2])
        
        if len(p) > 4:
            stmt.set_attribute("from", p[4])
        
        p[0] = stmt
    
    def p_empty(self, p):
        """empty :"""
        pass
    
    def p_error(self, p):
        if p:
            logger.error(f"Errore di sintassi alla linea {p.lineno}: token '{p.value}'")
        else:
            logger.error("Errore di sintassi: fine del file inaspettata")
    
    def _analyze_picture(self, pic_string):
        """Analizza una PICTURE clause e determina il tipo Java"""
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
            # Preprocessa il codice COBOL (rimuove colonne 1-6, gestisce continuazioni)
            processed_data = self._preprocess_cobol(data)
            
            # Resetta lo stato
            self.data_items = {}
            self.procedures = {}
            
            # Parsa
            result = self.parser.parse(processed_data, lexer=self.lexer.lexer, 
                                     debug=debug)
            
            if result:
                logger.info("Parsing completato con successo")
            else:
                logger.error("Parsing fallito: nessun risultato")
                
            return result
            
        except Exception as e:
            logger.error(f"Errore durante il parsing: {str(e)}")
            raise
    
    def _preprocess_cobol(self, data):
        """Preprocessa il codice COBOL per gestire il formato fisso"""
        lines = data.split('\n')
        processed_lines = []
        
        for line in lines:
            # Ignora righe vuote
            if not line.strip():
                continue
            
            # Ignora commenti (asterisco in colonna 7 o *> )
            if len(line) > 6 and line[6] == '*':
                continue
            if line.strip().startswith('*>'):
                continue
            
            # Rimuove le prime 6 colonne se presenti (numero di sequenza)
            if len(line) > 6:
                # Prende dalla colonna 7 in poi
                processed_line = line[6:]
            else:
                processed_line = line
            
            # Rimuove colonne 73-80 (identificazione)
            if len(processed_line) > 66:
                processed_line = processed_line[:66]
            
            processed_lines.append(processed_line.rstrip())
        
        return '\n'.join(processed_lines)
    
    def _preprocess_cobol_advanced(self, data):
        """Preprocessa il codice COBOL con gestione avanzata"""
        lines = data.split('\n')
        processed_lines = []
        in_string_statement = False
        
        for line in lines:
            # Gestione base esistente...
            processed_line = self._preprocess_cobol(line)
            
            # Gestione speciale per STRING statements multi-linea
            if 'STRING' in processed_line and not 'STRING_LITERAL' in processed_line:
                in_string_statement = True
            
            if in_string_statement:
                # Rimuovi spazi extra nelle string statements
                processed_line = ' '.join(processed_line.split())
                if processed_line.endswith('.'):
                    in_string_statement = False
            
            # Gestione underscore nelle query SQL (non sono errori)
            if 'EXEC SQL' in line or in_sql_block:
                # Gli underscore sono validi in SQL
                pass
            
            processed_lines.append(processed_line)
        
        return '\n'.join(processed_lines)