# ------------------------------------------------------------
# assuraDRC_to_pvsDRC.py
#
# 
# ------------------------------------------------------------
import ply.lex as lex
import string
from collections import Counter
import os,sys

lines = []
expr = []
count = 0
DEBUG = 0
expr_sort = []
list_more = []

#Checks the number of times an element is repeated in the data
def num_times_var(var):
    global lines
    global count 
    count = 0
    for line in lines:
        if var in line:
            count = count +1
    return count

#If the element is repeated 3 or more than 3 times, 
#then return the next element as where the drc needs to 
# be added. 
#All the expressions before this i will be printed at a higher level
#before printing the drc.

def index_return(elements):
    global lines
    index=0
    i=0
#    for i in range(len(elements)):
    #If the element has repeated 3 or more than 3 times, then insert that line in the expression
    if num_times_var(elements[i])>= 3:
        index = i+1
    return index
                     
    
#    for element in lines:
#        if element >= 3:
#            i = i+1
#            insert_at = i
#    return insert_at
    
# List of token names.   This is always required
tokens = (
    'NUMBER',
    'PLUS','MINUS','TIMES','DIVIDE','EQUALS',
    'LPAREN','RPAREN', 'MODULUS','POWER',  'GREATERTHAN', 'LESSTHAN','FLOAT','DRC','SEPNOTCH','ID','METAL','GEOMGETNON90',
    'ERRORINFO','RULMESSAGE' ,'UNDERSCORE' ,'LAYER','CHECK','NBURIED' ,'NWELL','ENC','GEOMGETLENGTH','GEOMWIDTH','KEEP','GEOMGETEDGE','COINCIDENT',
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
#t_GREATERTHAN = r'\>'

#reserved words
reserved = {
   'errorLayer': 'ERRORINFO',
   'check ': 'CHECK',
   'enc': 'ENC',
}

# A regular expression rule with some action code

def t_COINCIDENT(t):
    r'coincident'
    t.value = '-coincident_only'
    return t

def t_GEOMGETEDGE(t):
    r'geomGetEdge'
    t.value = 'edge_boolean'
    return t
    
def t_KEEP(t):
    r'keep'
    t.value = ['-by','-underover']
    return t
t_KEEP.__doc__=r'keep' #can be an expression

def t_GEOMWIDTH(t):
    r'geomWidth'
    t.value = ['size','and']
    return t
t_GEOMWIDTH.__doc__=r'geomWidth' #can be an expression

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
    
def t_GREATERTHAN(t):
    r'\>'
    t.value = '-gt'
    return t   
    
def t_DRC(t):
    r'drc'
    t.value = ['exte']
    return t
    
def t_SEPNOTCH(t):
   r'sepNotch|sep'
   if t.value=='Metal1sepNotch':
       t.value = '-notch'
   else:
       t.value = ["",'exte']
   return t
   
def t_GEOMGETLENGTH(t):
    r'geomGetLength'
    t.value = 'edge_length'
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
data = '''L18723=geomWidth(Metal1 keep>0.18)
L52985=geomGetEdge(Metal1 coincident L18723)
L3396=drc(Metal1 L52985 0<sep<0.18 opposite edgeb)
L79024=geomGetLength(L3396 keep>0.56)
errorLayer(L79024 "METAL1.SP.1.2: Metal1 to Metal1 spacing must be >= 0.18 um")'''
#Metal1_d=layer( 7 type(0) )
#L91383=drc(Nburied Nwell enc<0.2)'''

#data = '''L18723=geomWidth(Metal1 keep>0.18)
#L52985=geomGetEdge(Metal1 coincident L18723)
#L52985=geomGetEdge(Metal1 coincident L18723)
#L3396=drc(Metal1 L52985 0<sep<0.18 opposite edgeb)
#L79024=geomGetLength(L3396 keep>0.56)
#errorLayer(L79024 "METAL1.SP.1.2: Metal1 to Metal1 spacing must be >= 0.18 um")'''



# Tokenize
#which_token = lexer.token()
#print ('which_token = ',which_token)
#print ("Comment:", which_token.type)
lexer.input(data)
while True: 
    tok = lexer.token()
    if not tok: 
        break      # No more input
    if DEBUG>2:
        print(tok)


# Parsing rules  
precedence = (
('nonassoc', 'LESSTHAN', 'GREATERTHAN'),
('left', 'SEPNOTCH'),
('left','METAL'),
('left','LPAREN'),
('left','DRC'),
    )

# dictionary of names
layer = {}

          

# Parsing rules

#Printing the rule
# By default count is always 0
#def print_expr():
#    global expr
##    expr1 = layer[expr[i]]
#    for i in range(len(expr)):
#        expr1 = layer[expr[i]]
#        if DEBUG >=1:
#            print('expr1 = ',expr1)
#        if i ==0:
##            print(expr1)
#            print('rule ',expr1.split(':')[0],'" {')
#            print('\tcaption',expr1,';')
#        else:
#            print('\t',expr1,';')         
#    print('}')
#
#    expr = []


#Prints the rule in PVL
def print_expr(count):
    global expr
#    expr1 = layer[expr[i]]
    if count == 0:
        for i in range(len(expr)):
            expr1 = layer[expr[i]]
            if DEBUG >=1:
                print('expr1 = ',expr1)
            if i ==0:
    #            print(expr1)
                print('rule ',expr1.split(':')[0],'" {')
                print('\t caption',expr1,';')
            else:
                print('\t',expr1,';')         
        print('}')
        expr = []
    else:
        print('##')
        
    
#def print_expr(count):
#    global expr_sort
##    expr1 = layer[expr[i]]
##    global expr_sort
#    if count == 0:
#        for i in range(len(expr_sort)):
#            expr1 = layer[expr_sort[i]]
#            if DEBUG >=1:
#                print('expr1 = ',expr1)
#            if i ==0:
#    #            print(expr1)
#                print('rule ',expr1.split(':')[0],'" {')
#                print('\tcaption',expr1,';')
#            else:
#                print('\t',expr1,';')         
#        print('}')
#        expr_sort = []
#    else:
#        print('########')
##        for i in range(len(expr)):
##            expr1 = layer[expr[i]]
##            print()
#        expr_sort = []
    
#def print_expr1(count = 0):
#    global expr
#    if count == 0:
#        print()
##        for i in range(len(expr)):
##            expr1 = layer[expr[i]]
##            if i ==0:
##    #            print(expr1)
##                print('rule ',expr1.split(':')[0],'" {')
##                print('\tcaption',expr1,';')
##            else:
##                print('\t',expr1,';')         
##        print('}')
#    else:
#        for i in range(len(expr)):
#            expr1 = layer[expr[i]]
#            if count ==1:
#                print(expr[i])
#    expr = [] 




#From : L998=drc(Metal1 sepNotch<0.06)       
# To : exte Metal1 Metal1 -lt 0.06 -output region -abut lt 90; 
def p_statement_metalspacing(t):
    'expression :  ID EQUALS DRC LPAREN METAL SEPNOTCH LESSTHAN NUMBER RPAREN'
    layer[t[1]] = " ".join([t[6][0],t[5],t[5], t[7],str(t[8]),'-output region -singular -abut lt 90 ' ])
    expr.append(t[1])


#From: L18723=geomWidth(Metal1 keep>0.18)
#To : size Metal1 -by 0.05 -underover L78025; and L78025 Metal1 L80731;
#def p_statement_geomwidth(t):
#    'expression : ID EQUALS GEOMWIDTH LPAREN METAL KEEP GREATERTHAN NUMBER RPAREN'
#    layer[t[1]] = " ".join([t[3][0],t[5],t[6][0],str(t[8]),t[6][1],t[1],';','\n\t',t[3][1],t[1],t[5],t[1]])
#    expr.append(t[1])    

def p_statement_geomwidth(t):
    'expression : ID EQUALS GEOMWIDTH LPAREN METAL KEEP GREATERTHAN NUMBER RPAREN'
    layer[t[1]] = " ".join([t[3][0],t[5],t[6][0],str(t[8]),t[6][1],'L78025',';','\n\t',t[3][1],'L78025',t[5],'L80731'])
    expr.append(t[1])       
    
#from:L79024=geomGetLength(L3396 keep>0.56)    
#To : edge_length L52229 -gt 0.32;
def p_statement_geomGetLength(t):
    'expression : ID EQUALS GEOMGETLENGTH LPAREN ID KEEP GREATERTHAN NUMBER RPAREN'
    layer[t[1]] = " ".join([t[3],'L52229',t[7],str(t[8])])
    expr.append(t[1])
    
#From : L3396=drc(Metal1 L52985 0<sep<0.18 opposite edgeb)
#To: exte Metal1 L67295 -lt 0.1 -output positive2  -output region -project -abut ltgt 0 90 L52229;
def p_statement_drcsep(t):
    'expression : ID EQUALS DRC LPAREN METAL ID NUMBER LESSTHAN SEPNOTCH LESSTHAN NUMBER ID ID RPAREN'
    layer[t[1]] = " ".join([t[3][0],t[5],'L67295',t[8],str(t[11]),'-output region -project -abut ltgt 0 90','L52229'])
    expr.append(t[1])
    
   
#From : L52985=geomGetEdge(Metal1 coincident L18723)
#To : edge_boolean -coincident_only Metal1 L80731 L67295;
def p_statement_geomgetedge(t):
    'expression : ID EQUALS GEOMGETEDGE LPAREN METAL COINCIDENT ID RPAREN'
    layer[t[1]] = " ".join([t[3],t[6],t[5],'L80731','L67295'])
    expr.append(t[1])

   
#errorLayer(L998 "METAL1.SP.1.1: Metal1 to spacing must be >= 0.06 um")
#TODO: debug p_statement_getRUL
def p_statement_getRUL(t):
    'expression : ERRORINFO LPAREN ID RULMESSAGE RPAREN'
    global expr_sort
    elements = 0
    if DEBUG>2:
        print('expr  =',expr)
        print('t[4]  =',t[4])
    layer[t[1]] = t[4]
    if DEBUG>2:
        print('layer[t[1]]  =',layer[t[1]])
        print('layer =',layer)
#    expr.append(t[1])
    expr.insert(index_return(elements))
    
#    Sorting and getting all the repeated IDs first to process them first
#    expr_sort = list(expr)
#    expr_sort.sort(key=Counter(expr_sort).get, reverse=True)
    
#    I have a list
#    If the elements in the list is repeated more than 2 times put it in a seperate list
#    and delete them in the old list
    
    if DEBUG>2:    
        print ('expr_sort =',expr_sort)

    if DEBUG>2:
        print('t[1]  =',t[1])
        print('expr  =',expr)
#    expr.reverse()
    if DEBUG>2:
        print('expr  =',expr)
#    Setting count_ID to 0
#    count_ID = 0
##    I am checking the number of IDs that are repeated in this for loop
#    for i in range(len(expr)):
##    If the count is more than 2 count_ID is set to 1
#        if num_times_var(expr[i]) >2:
#            count_ID = 1
#            print_expr(count_ID,i)
##    If the count is less than 2 count_ID is set to 0
#        else:
#            count_ID = 0
#            print_expr(count_ID,i)
        
###################################################33
#    Setting count_ID to 0
    count_ID = 0
#    print_expr()
#    I am checking the number of IDs that are repeated in this for loop
    for i in range(len(expr)):
#    If the count is more than 2 count_ID is set to 1
        if num_times_var(expr[i]) > 2:
            count_ID = 1
#            for j in range(num_times_var(expr[i])):
#            I am appending all the IDs that are more than 2 in list_more list
            list_more.append(expr[i])
#    If the count is less than 2 count_ID is set to 0
        else:
            count_ID = 0
    for ID in list_more:
#        If the same element in the list_more list exists in expr_sort remove them
        if ID in expr_sort:
            expr_sort.remove(ID)
    if DEBUG>2:
        print('list_more = ',list_more)
        print('expr_sort = ',expr_sort)
        print('count_ID = ',count_ID)
#    After setting the count_ID call the print_expr function
    if count_ID == 0:
        print_expr(count_ID)
    else:
        print_expr(count_ID)

#From :L51265=geomGetNon90(Metal1)
#To:angle inLayer -ltgt 0 90 outLayer;    
def p_statement_geonon90(t):
    'expression : ID EQUALS GEOMGETNON90 LPAREN METAL RPAREN'
    layer[t[1]] = " ".join([t[3][0],t[5], t[3][1],t[1]])
    expr.append(t[1])
    
# From :Metal1_d=layer( 7 type(0) )
# To:layer_def layername arbitary_number;
def p_statement_layermap(t):
    'expression : ID EQUALS LAYER LPAREN NUMBER ID LPAREN NUMBER RPAREN RPAREN '
    layer[t[1]] = " ".join([t[3][1],t[3][1],'arbitary_number;' ])
    expr.append(t[1])
    
#From : L91383=drc(Nburied Nwell enc<0.2)
#To   : enc inLayer2 inLayer1 -lt value -output region -abut lt 90;
def p_statement_enclose(t):
    'expression : ID EQUALS DRC LPAREN NBURIED NWELL ENC LESSTHAN NUMBER RPAREN '
    layer[t[1]] = " ".join([t[7],t[5][0], t[6][0], t[8], t[9],'-output region -abut lt 90;'])
    expr.append(t[1])

    
import ply.yacc as yacc
parser = yacc.yacc()


#lines = data.split('\n')
infl = open("Input_rule_file")
lines = list(infl)
for line in lines:
    if not(line == '\n' or line ==''):
        result = parser.parse(line)
        pass

#f = open('output_rule_file','w');
#sys.stdout = f
#print('#L18723=',num_times_var('L18723'))
#print('#L52985=',num_times_var('L52985'))
#print('#L3396=',num_times_var('L3396'))
#print('#L79024=',num_times_var('L79024'))




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
