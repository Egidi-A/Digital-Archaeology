"""
Procedure Analyzer - Analizza le procedure COBOL e le mappa su metodi Java
"""

from typing import Dict, List, Any, Set
from loguru import logger
from ..parser.ast_nodes import *


class ProcedureAnalyzer:
    """Analizza le procedure nell'AST COBOL"""
    
    def __init__(self, config: Dict[str, Any]):
        self.config = config
        self.procedures = {}
        self.call_graph = {}
        self.main_procedure = None
        self.menu_procedures = []
        self.business_procedures = []
        self.utility_procedures = []
        
    def analyze(self, ast: ASTNode) -> Dict[str, Any]:
        """Analizza l'AST e estrae le procedure"""
        logger.info("Inizio analisi delle procedure")
        
        # Reset
        self.procedures = {}
        self.call_graph = {}
        self.main_procedure = None
        self.menu_procedures = []
        self.business_procedures = []
        self.utility_procedures = []
        
        # Trova la PROCEDURE DIVISION
        proc_div = self._find_procedure_division(ast)
        if not proc_div:
            logger.warning("PROCEDURE DIVISION non trovata")
            return {}
        
        # Analizza le procedure
        self._analyze_procedures(proc_div)
        
        # Costruisci il grafo delle chiamate
        self._build_call_graph()
        
        # Classifica le procedure
        self._classify_procedures()
        
        # Organizza i risultati
        result = {
            'procedures': self.procedures,
            'call_graph': self.call_graph,
            'main_procedure': self.main_procedure,
            'menu_procedures': self.menu_procedures,
            'business_procedures': self.business_procedures,
            'utility_procedures': self.utility_procedures,
            'services': self._generate_services()
        }
        
        logger.info(f"Trovate {len(self.procedures)} procedure")
        logger.info(f"Main procedure: {self.main_procedure}")
        
        return result
    
    def _find_procedure_division(self, node: ASTNode) -> ASTNode:
        """Trova la PROCEDURE DIVISION nell'AST"""
        if node.node_type == NodeType.DIVISION and node.name == "PROCEDURE":
            return node
        
        for child in node.children:
            result = self._find_procedure_division(child)
            if result:
                return result
        
        return None
    
    def _analyze_procedures(self, proc_div: ASTNode):
        """Analizza tutte le procedure nella PROCEDURE DIVISION"""
        for child in proc_div.children:
            if isinstance(child, ProcedureNode):
                self._analyze_single_procedure(child)
    
    def _analyze_single_procedure(self, proc_node: ProcedureNode):
        """Analizza una singola procedura"""
        proc_info = {
            'name': proc_node.name,
            'java_name': self._to_java_method_name(proc_node.name),
            'statements': [],
            'calls': [],
            'performs': [],
            'sql_operations': [],
            'file_operations': [],
            'input_operations': [],
            'output_operations': [],
            'business_logic': [],
            'parameters': [],
            'return_type': 'void',
            'is_main': proc_node.name.upper() in ['MAIN-LOGIC', 'MAIN', 'MAINLINE'],
            'is_menu': False,
            'is_business': False,
            'is_utility': False
        }
        
        # Analizza gli statement
        for stmt in proc_node.children:
            self._analyze_statement(stmt, proc_info)
        
        # Determina il tipo di procedura
        if 'MENU' in proc_node.name.upper() or 'DISPLAY' in proc_node.name.upper():
            proc_info['is_menu'] = True
        elif any(op in proc_node.name.upper() for op in ['APERTURA', 'DEPOSITO', 'PRELIEVO', 'SALDO']):
            proc_info['is_business'] = True
        else:
            proc_info['is_utility'] = True
        
        self.procedures[proc_node.name] = proc_info
    
    def _analyze_statement(self, stmt: ASTNode, proc_info: Dict[str, Any]):
        """Analizza uno statement e aggiorna le informazioni della procedura"""
        if isinstance(stmt, StatementNode):
            stmt_info = {
                'type': stmt.statement_type,
                'line': stmt.line_number
            }
            
            if stmt.statement_type == 'PERFORM':
                target = stmt.get_attribute('procedure')
                if target:
                    proc_info['performs'].append(target)
                    proc_info['calls'].append(target)
                    
            elif stmt.statement_type == 'DISPLAY':
                items = stmt.get_attribute('items', [])
                proc_info['output_operations'].append({
                    'type': 'display',
                    'items': items
                })
                
            elif stmt.statement_type == 'ACCEPT':
                target = stmt.get_attribute('target')
                proc_info['input_operations'].append({
                    'type': 'accept',
                    'target': target
                })
                
            elif stmt.statement_type in ['MOVE', 'COMPUTE', 'ADD', 'SUBTRACT']:
                proc_info['business_logic'].append(stmt_info)
                
            elif stmt.statement_type in ['OPEN', 'CLOSE', 'READ', 'WRITE']:
                proc_info['file_operations'].append({
                    'type': stmt.statement_type.lower(),
                    'file': stmt.get_attribute('file')
                })
                
            proc_info['statements'].append(stmt_info)
            
        elif isinstance(stmt, SqlStatementNode):
            sql_info = {
                'type': stmt.sql_type,
                'table': stmt.table_name,
                'sql': stmt.sql_text
            }
            proc_info['sql_operations'].append(sql_info)
            
        # Analizza ricorsivamente i figli
        for child in stmt.children:
            self._analyze_statement(child, proc_info)
    
    def _build_call_graph(self):
        """Costruisce il grafo delle chiamate tra procedure"""
        for proc_name, proc_info in self.procedures.items():
            self.call_graph[proc_name] = {
                'calls': proc_info['calls'],
                'called_by': []
            }
        
        # Calcola le dipendenze inverse
        for proc_name, proc_info in self.procedures.items():
            for called_proc in proc_info['calls']:
                if called_proc in self.call_graph:
                    self.call_graph[called_proc]['called_by'].append(proc_name)
    
    def _classify_procedures(self):
        """Classifica le procedure per tipo"""
        for proc_name, proc_info in self.procedures.items():
            if proc_info['is_main']:
                self.main_procedure = proc_name
            elif proc_info['is_menu']:
                self.menu_procedures.append(proc_name)
            elif proc_info['is_business']:
                self.business_procedures.append(proc_name)
            else:
                self.utility_procedures.append(proc_name)
    
    def _generate_services(self) -> List[Dict[str, Any]]:
        """Genera i servizi Java dalle procedure business"""
        services = []
        
        # Servizio principale per le operazioni bancarie
        bank_service = {
            'name': 'BankAccountService',
            'methods': []
        }
        
        # Mappa le procedure business su metodi del servizio
        procedure_to_method = {
            'APERTURA-CONTO': {
                'name': 'createAccount',
                'params': [
                    {'name': 'request', 'type': 'AperturaContoRequest'}
                ],
                'return': 'ContoResponse',
                'throws': ['BusinessException']
            },
            'DEPOSITO': {
                'name': 'deposit',
                'params': [
                    {'name': 'accountNumber', 'type': 'String'},
                    {'name': 'request', 'type': 'MovimentoRequest'}
                ],
                'return': 'ContoResponse',
                'throws': ['AccountNotFoundException', 'BusinessException']
            },
            'PRELIEVO': {
                'name': 'withdraw',
                'params': [
                    {'name': 'accountNumber', 'type': 'String'},
                    {'name': 'request', 'type': 'MovimentoRequest'}
                ],
                'return': 'ContoResponse',
                'throws': ['AccountNotFoundException', 'InsufficientFundsException', 'BusinessException']
            },
            'VISUALIZZA-SALDO': {
                'name': 'getBalance',
                'params': [
                    {'name': 'accountNumber', 'type': 'String'}
                ],
                'return': 'ContoResponse',
                'throws': ['AccountNotFoundException']
            },
            'ESTRATTO-CONTO': {
                'name': 'getStatement',
                'params': [
                    {'name': 'accountNumber', 'type': 'String'},
                    {'name': 'startDate', 'type': 'LocalDate', 'optional': True},
                    {'name': 'endDate', 'type': 'LocalDate', 'optional': True}
                ],
                'return': 'List<MovimentoResponse>',
                'throws': ['AccountNotFoundException']
            },
            'CHIUSURA-CONTO': {
                'name': 'closeAccount',
                'params': [
                    {'name': 'accountNumber', 'type': 'String'}
                ],
                'return': 'void',
                'throws': ['AccountNotFoundException', 'BusinessException']
            }
        }
        
        # Aggiungi i metodi trovati
        for proc_name in self.business_procedures:
            if proc_name in procedure_to_method:
                method = procedure_to_method[proc_name]
                method['original_procedure'] = proc_name
                bank_service['methods'].append(method)
        
        services.append(bank_service)
        
        # Servizio per le utility
        if self.utility_procedures:
            utility_service = {
                'name': 'UtilityService',
                'methods': []
            }
            
            for proc_name in self.utility_procedures:
                method = {
                    'name': self._to_java_method_name(proc_name),
                    'original_procedure': proc_name,
                    'params': [],
                    'return': 'void',
                    'throws': []
                }
                utility_service['methods'].append(method)
            
            services.append(utility_service)
        
        return services
    
    def _to_java_method_name(self, cobol_name: str) -> str:
        """Converte un nome COBOL in nome metodo Java (camelCase)"""
        # Rimuove numeri all'inizio
        name = cobol_name.lstrip('0123456789')
        
        # Converte in camelCase
        parts = name.replace('-', '_').split('_')
        if len(parts) == 1:
            return parts[0].lower()
        
        return parts[0].lower() + ''.join(p.capitalize() for p in parts[1:])