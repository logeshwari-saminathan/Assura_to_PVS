# ------------------------------------------------------------
# assuraDRC_to_pvsDRC.py
#
# 
# ------------------------------------------------------------
import ply.lex as lex
import string

# List of token names.   This is always required
tokens = (
    'NUMBER',
    'PLUS','MINUS','TIMES','DIVIDE','EQUALS',
    'LPAREN','RPAREN', 'MODULUS','POWER',  'GREATERTHAN', 'LESSTHAN','FLOAT','DRC','SEPNOTCH','ID','METAL','GEOMGETNON90',
    'ERRORINFO','RULMESSAGE' ,'UNDERSCORE' ,'LAYER','CHECK','NBURIED' ,'NWELL','ENC',
    )

# Regular expression rules for simple tokens

t_PLUS    = r'\+'
t_MINUS   = r'-'
t_TIMES   = r'\*'
t_DIVIDE  = r'/'
t_EQUALS  = r'='
t_LPAREN  = r'\('
t_RPAREN  = r'\)'
#t_METAL   = r'Metal'

#t_NAME    = r'[a-zA-Z_][a-zA-Z0-9_]*'
t_RULMESSAGE = r'".*"'
t_GREATERTHAN = r'\>'

#reserved words
reserved = {
   'errorLayer': 'ERRORINFO',
   'check ': 'CHECK',
   'enc': 'ENC',
}

# A regular expression rule with some action code
def t_METAL(t):    
    return t
t_METAL.__doc__=r'Metal\d' #can be an expression

    
def t_NWELL(t):
    r'nwell'
    t.value =['inLayer1']
    return t


def t_NBURIED(t):
    r'nburied'
    t.value =['inLayer2']
    return t

def t_LAYER(t):
    r'layer'
    t.value =['layer_def','layername']
    return t
    

def t_GEOMGETNON90(t):
    r'geomGetNon90'
    t.value = ['angle ', ' -ltgt 0 90']
    return t


def t_LESSTHAN(t):
    r'\<'
    t.value = '-lt'
    return t
    
def t_DRC(t):
    r'drc'
    t.value = ['exte']
    return t
    
def t_SEPNOTCH(t):
   r'sepNotch|sep'
   if t.value=='sMetal1epNotch':
       t.value = '-notch'
   else:
       t.value = ""
   return t
    
def t_ID(t):
   r'[a-zA-Z_][a-zA-Z_0-9]*'
   t.type = reserved.get(t.value,'ID')    # Check for reserved words
   return t

def t_NUMBER(t):
    r'\d*\.\d+|\d+'
    try:
        t.value = float(t.value)
    except ValueError:
        print("Number is too large %d", t.value)
        t.value = 0
    return t


def t_newline(t):
    r'\n+'
    t.lexer.lineno += len(t.value)
    

# A string containing ignored characters (spaces and tabs)
t_ignore  = '\t \n'

# Error handling rule
def t_error(t):
    print("Illegal character '%s'" % t.value[0])
    t.lexer.skip(1)

# Build the lexer
lexer = lex.lex()

# Test it out  (Input)
data = '''L998=drc(Metal1 sepNotch<0.06)

errorLayer(L998 "METAL1.SP.1.1: Metal1 to Metal1 spacing must be >= 0.06 um")'''
#Metal1_d=layer( 7 type(0) )
#L91383=drc(Nburied Nwell enc<0.2)'''




# Tokenize
#which_token = lexer.token()
#print ('which_token = ',which_token)
#print ("Comment:", which_token.type)
lexer.input(data)
while True: 
    tok = lexer.token()
    if not tok: 
        break      # No more input
    print(tok)


# Parsing rules  
precedence = (
('nonassoc', 'LESSTHAN', 'GREATERTHAN'),
('left', 'SEPNOTCH'),
('left','METAL'),
('left','LPAREN','RPAREN'),
('left','DRC'),
    )

# dictionary of names
names = {}


#[outLayer = ]drc( inLayer1 [inLayer2] check [modifiers] )
#def p_expression(t):print('\t',expr)
#   expression : ID? drc expression1
#   expression1: LPARAN Metal Metal? check RPARAN
#   check      : LESSTHAN | EQUALS? | GREATERTHAN? | EQUALS? 
                
    

                

# Parsing rules

expr = []
def print_expr():
    global expr
    for i in range(len(expr)):
        expr1 = names[expr[i]]
        if i ==0:
#            print(expr1)
            print('rule ',expr1.split(':')[0],'" {')
            print('\tcaption',expr1)
        else:
            print('\t',expr1)
             
    print('}')
    expr = []  

 #From : L998=drc(Metal1 sepNotch<0.06)       
# To : exte Metal1 Metal1 -lt 0.06 -output region -abut lt 90; 
def p_statement_assign(t):
    'expression :  ID EQUALS DRC LPAREN METAL SEPNOTCH LESSTHAN NUMBER RPAREN'
    names[t[1]] = " ".join([t[3][0],t[5],t[5], t[7],str(t[8]),'-output region -abut lt 90; ' ])
#    names[t[1]] = t[3]
    expr.append(t[1])
#    print(names[t[1]])
    

#errorLayer(L998 "METAL1.SP.1.1: Metal1 to spacing must be >= 0.06 um")
def p_statement_getRUL(t):
    'expression : ERRORINFO LPAREN ID RULMESSAGE RPAREN'
    names[t[1]] = t[4]
    expr.append(t[1])
    expr.reverse()
    print_expr()


#From :L51265=geomGetNon90(Metal1)
#To:angle inLayer -ltgt 0 90 outLayer;    
def p_statement_geonon90(t):
    'expression : ID EQUALS GEOMGETNON90 LPAREN METAL RPAREN'
    names[t[1]] = " ".join([t[3][0],t[5], t[3][1],t[1]])
    expr.append(t[1])
    
# From :Metal1_d=layer( 7 type(0) )
# To:layer_def layername arbitary_number;
def p_statement_layermap(t):
    'expression : ID EQUALS LAYER LPAREN NUMBER ID LPAREN NUMBER RPAREN RPAREN '
    names[t[1]] = " ".join([t[3][1],t[3][1],'arbitary_number;' ])
    expr.append(t[1])
    
#From : L91383=drc(Nburied Nwell enc<0.2)
#To   : enc inLayer2 inLayer1 -lt value -output region -abut lt 90;
def p_statement_enclose(t):
    'expression : ID EQUALS DRC LPAREN NBURIED NWELL ENC LESSTHAN NUMBER RPAREN '
    names[t[1]] = " ".join([t[7],t[5][0], t[6][0], t[8], t[9],'-output region -abut lt 90;'])
    expr.append(t[1])

    
import ply.yacc as yacc
parser = yacc.yacc()
lines = data.split('\n')
#infl = open("Input_rule_file")
#lines = list(infl)
for line in lines:
    if not(line == '\n' or line==''):
        result = parser.parse(line)

#def p_expression_name(t):
#    'expression : NAME'
#    try:
#        t[0] = names[t[1]]
#    except LookupError:
#        print("Undefined name '%s'" % t[1])
#        t[0] = 0
#        while True:

#def p_error(t):
#    print("Syntax error at '%s'" % t.value)



#while True: 
#    tok=parser.parse(names)
#    if not tok: 
#        break      # No more input
#    print(tok)
#while True:
#   try:
#       s = input('calc > ')
#   except EOFError:
#       break
#   if not s: continue
#   result = parser.parse(s)
#   print(result)
