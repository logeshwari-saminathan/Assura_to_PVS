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
#expr = ['L51265', 'errorLayer', 'L51265', 'L28380']

count = 0
DEBUG = 0
expr_sort = []
list_more = []
Write2File = True
read_from_data = False
read_from_file = True
#read either data or input file

    


if Write2File:
    sys.stdout=open("output_test","w")
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
#All the expressions before this it will be printed at a higher level
#before printing the drc.

def index_return():
    global inlines,expr
    index=0
    i=0
    if DEBUG>0:
        print(inlines)
    for i in range(len(expr)):
    #If the element has repeated 3 or more than 3 times, then insert that line in the expression
#        inline=inlines[i].split('=')
#        print(inline,inline[0])
        if num_times_var(expr[i])>= 3:
            index = i+1
    return index
                     
    
#    for element_layer_insert_post in lines:
#        if element >= 3:
#            i = i+1
#            insert_at = i
#    return insert_at
    
# List of token names.   This is always required
tokens = (
    'NUMBER',
    'PLUS','MINUS','TIMES','DIVIDE','EQUALS',
    'LPAREN','RPAREN', 'MODULUS','POWER',  'GREATERTHAN', 'LESSTHAN','FLOAT','DRC','SEPNOTCH','ID','METAL','GEOMGETNON90',
    'ERRORINFO','RULMESSAGE' ,'UNDERSCORE' ,'LAYER','CHECK','NBURIED' ,'NWELL','ENC','GEOMGETLENGTH','GEOMWIDTH','KEEP','GEOMGETEDGE','COINCIDENT','TYPE','VIA','GEOMANDNOT','CONT','METALCONN','GEOMAND',
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

def t_METALCONN(t): 
    return t
t_METALCONN.__doc__=r'metal\d_conn' #can be an expression

def t_CONT(t):
    r'Cont'
    t.value = 'Cont'
    return t 


def t_GEOMANDNOT(t):
    r'geomAndNot'
    t.value = 'not'
    return t
t_GEOMANDNOT.__doc__=r'geomAndNot' #can be an expression

    
def t_TYPE(t):
    r'type'
    t.value = '-datatype'
    return t 


def t_COINCIDENT(t):
    r'coincident|inside'
    if t.value=='coincident':
       t.value = '-coincident_only'
    else:
        t.value='-inside'
    return t

def t_GEOMGETEDGE(t):
    r'geomGetEdge'
    t.value = 'edge_boolean'
    return t
    
def t_KEEP(t):
    r'keep'
    t.value = ['-by','-underover',""]
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

def t_VIA (t):
    return t
t_VIA.__doc__=r'Via\d' #can be an expression  
    
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
    t.value =['layer_def','layer_map']
    return t
    

def t_GEOMGETNON90(t):
    r'geomGetNon90'
    t.value = ['angle ', ' -ltgt']
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
    t.value = ['exte','inte']
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
data = '''L51265=geomGetNon90(Metal1)
L51265=geomGetNon90(Metal1)
L28380=geomGetLength(L51265 keep<0.18)
errorLayer(L28380
    "METAL1.L.1: Metal1 non-90 degree segments must be >= 0.18 um")'''


#data = '''L18723=geomWidth(Metal1 keep>0.18)
#L52985=geomGetEdge(Metal1 coincident L18723)
#L52985=geomGetEdge(Metal1 coincident L18723)
#L3396=drc(Metal1 L52985 0<sep<0.18 opposite edgeb)
#L79024=geomGetLength(L3396 keep>0.56)
#errorLayer(L79024 "METAL1.SP.1.2: Metal1 to Metal1 spacing must be >= 0.18 um")'''

#data = '''L45840=geomWidth(Metal1 keep>1.50)
#L4475=geomGetEdge(Metal1 coincident L45840)
#L88697=drc(Metal1 L4475 0<sep<0.50 opposite edgeb)
#L38744=geomGetLength(L88697 keep>1.50)
#errorLayer(L38744 "METAL1.SP.1.3: Metal1 to Metal1 spacing must be >= 0.50 um")'''

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
#layer = {'L28380': 'edge_length L51265 -lt 0.18 ;', 'L51265': 'angle  Metal1  -ltgt 0 90 L51265', 'errorLayer': '"METAL1.L.1: Metal1 non-90 degree segments must be >= 0.18 um"'}
          

# Parsing rules

#Printing the rule
# By default count is always 0
def print_expr():
    global expr
    if DEBUG >=0:
        print('expr =',expr)
#        get the index of errorLayer
    index_error = expr.index('errorLayer')
    for i in range(len(expr)):
        expr1 = layer[expr[i]]
        if DEBUG >=1:
            print('expr1 = ',expr1)
        if expr[i] == 'errorLayer':
            print('rule ',expr1.split(':')[0],'" {')
            print('\t caption',expr1,';')
#If this expression is before errorLayer then 
#don't put '\t' else put '\t'.            
        elif i<index_error:
            print(expr1,';')                     
        else:
            print('\t',expr1,';')         
    print('}')

    expr = []


#Prints the rule in PVL
#def print_expr(count):
#    global expr
##    expr1 = layer[expr[i]]
#    if count == 0:
#        for i in range(len(expr)):
#            expr1 = layer[expr[i]]
#            if DEBUG >=1:
#                print('expr1 = ',expr1)
#            if i ==0:
#    #            print(expr1)
#                print('rule ',expr1.split(':')[0],'" {')
#                print('\t caption',expr1,';')
#            else:
#                print('\t',expr1,';')         
#        print('}')
#        expr = []
#    else:
#        print('##')
        
    
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


#Layer Map##
#From : Metal1_d=layer( 7 type(0) )
#To : layer_def Nwell_drawing 1001;
#     layer_map 2 -datatype 0 1001;

#def p_layer_map(t):
#    'expression : ID EQUALS LAYER LPAREN NUMBER TYPE LPAREN NUMBER RPAREN RPAREN'
#    layer[t[1]] = " ".join([[3][0],t[1],'arbitary_number','\n',t[3][1],t[5],t[6],)



#global expr
#    expr1 = layer[expr[i]] 
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
    layer[t[1]] = " ".join([t[3],t[1],t[7],str(t[8])])
    expr.append(t[1])
    
#From : L3396=drc(Metal1 L52985 0<sep<0.18 opposite edgeb)
#To: exte Metal1 L67295 -lt 0.1 -output positive2  -output region -project -abut ltgt 0 90 L52229;
def p_statement_drcsep(t):
    'expression : ID EQUALS DRC LPAREN METAL ID NUMBER LESSTHAN SEPNOTCH LESSTHAN NUMBER ID ID RPAREN'
    layer[t[1]] = " ".join([t[3][0],t[5],t[1],t[8],str(t[11]),'-output region -project -abut ltgt 0 90',t[6]])
    expr.append(t[1])
    
   
#From : L52985=geomGetEdge(Metal1 coincident L18723)
#To : edge_boolean -coincident_only Metal1 L80731 L67295;
def p_statement_geomgetedge(t):
    'expression : ID EQUALS GEOMGETEDGE LPAREN METAL COINCIDENT ID RPAREN'
    layer[t[1]] = " ".join([t[3],t[6],t[5],t[1],t[7]])
    expr.append(t[1])
    
#From :L51265=geomGetNon90(Metal1)
#To:angle inLayer -ltgt 0 90 outLayer;    
def p_statement_geonon90(t):
    'expression : ID EQUALS GEOMGETNON90 LPAREN METAL RPAREN'
    layer[t[1]] = " ".join([t[3][0],t[5], t[3][1],'0','90',t[1]])
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
    
#From : L66270=drc(metal2_conn Via1 enc<0.005)
#To : enc Via1 metal2_conn -lt 0.005 -output region -singular -abut lt 90 -outside_also;
def p_via_enclosure(t):
    'expression : ID EQUALS DRC LPAREN METALCONN VIA ENC LESSTHAN NUMBER RPAREN'
    layer[t[1]] = " ".join([t[7],t[6],t[5],t[8],str(t[9]),'-output region -singular -abut lt 90 -outside_also;'])
    expr.append(t[1])
    
#From : L80230=geomAndNot(Cont metal1_conn)
#    L80230=geomAndNot(Cont metal1_conn)
   #To: not Cont metal1_conn L24896;
#        copy L24896;
def p_cont_enclosure(t):
    'expression : ID EQUALS GEOMANDNOT LPAREN CONT METALCONN RPAREN'
    layer[t[1]] = " ".join([t[3],t[5],t[6],t[1],'\n\t','copy',t[1]])
    expr.append(t[1])
    
#From:L91458=drc(metal1_conn width<0.12)
#To:    inte metal1_conn metal1_conn -lt 0.06 -output region -singular -abut lt 90;
def p_metal_width(t):
    'expression : ID EQUALS DRC LPAREN METALCONN ID LESSTHAN NUMBER '
    layer[t[1]] = " ".join([t[3][1],t[5],t[5],t[7],str(t[8]),'-output region -singular -abut lt 90;'])
    expr.append(t[1])
    
#From:L83789=drc(L96558 width<0.15)
#To:inte L43550 L43550 -lt 0.32 -output region -abut lt 90;
def p_n_channel_gate_width(t):
    'expression : ID EQUALS DRC LPAREN ID ID LESSTHAN NUMBER RPAREN '
    layer[t[1]] = " ".join([t[1],t[5],t[7],str(t[8]),'-output region -abut lt 90;'])
    expr.append(t[1])
    
#From : L28380=geomGetLength(L51265 keep<0.18)
#To : edge_length L79182 -lt 0.1;
def p_goem_get_length(t):
    'expression : ID EQUALS GEOMGETLENGTH LPAREN ID KEEP LESSTHAN NUMBER RPAREN'
    layer[t[1]] = " ".join([t[3],t[5],t[7],str(t[8]),';'])
    expr.append(t[1])

    
#From:L51265=geomGetNon90(Metal1)
#To:angle Metal1 -ltgt 0 90 L79182;
#def p_geom_get_non_99(t):
#    'expression : ID EQUALS GEOMGETNON90 LPAREN METAL RPAREN'    
#    layer[t[1]] = " ".join([t[3][1],t[5],t[3][1],'0','90',t[1]])    
#    expr.append(t[1])

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
    err_layer_insert_pos = index_return()
    expr.insert(err_layer_insert_pos,t[1])
    
#    Sorting and getting all the repeated IDs first to process them first
#    expr_sort = list(expr)
#    expr_sort.sort(key=Counter(expr_sort).get, reverse=True)
    
#    I have a list
#    If the elements in the list is repeated more than 2 times put it in a seperate list
#    and delete them in the old list
    
#    if DEBUG>2:    
#        print ('expr_sort =',expr_sort)

    if DEBUG>2:
        print('t[1]  =',t[1])
        print('expr  =',expr)
#    expr.reverse()
#    Setting count_ID to 0
#    count_ID = 0
    print_expr()

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
##    Setting count_ID to 0
#    count_ID = 0
##    print_expr()
##    I am checking the number of IDs that are repeated in this for loop
#    for i in range(len(expr)):
##    If the count is more than 2 count_ID is set to 1
#        if num_times_var(expr[i]) > 2:
#            count_ID = 1
##            for j in range(num_times_var(expr[i])):
##            I am appending all the IDs that are more than 2 in list_more list
#            list_more.append(expr[i])
##    If the count is less than 2 count_ID is set to 0
#        else:
#            count_ID = 0
#    for ID in list_more:
##        If the same element in the list_more list exists in expr_sort remove them
#        if ID in expr_sort:
#            expr_sort.remove(ID)
#    if DEBUG>2:
#        print('list_more = ',list_more)
#        print('expr_sort = ',expr_sort)
#        print('count_ID = ',count_ID)
##    After setting the count_ID call the print_expr function
#    if count_ID == 0:
#        print_expr(count_ID)
#    else:
#        print_expr(count_ID)




    
import ply.yacc as yacc
parser = yacc.yacc()



        

#Function which returns number of '(' in a line
    # for each character of the line 
        #check if its '(' and increment the count
    # return the final count
def left_paren_count(line):
    left_paren_count = 0
    for element in line:
        if '(' == element:
            left_paren_count = left_paren_count+1
    return left_paren_count

#Function which returns number of ')' in a line
    # for each character of the line 
        #check if its ')' and increment the count
    # return the final count
def right_paren_count(line):
    right_paren_count = 0
    for element in line:
        if ')' == element:
            right_paren_count = right_paren_count+1
    return right_paren_count

#Ignore the line which doesn't have left paren and right paren
#Function to compare if there are equal number of ( and ) in a line
#def compare_paren():
#    if left_paren_count() == right_paren_count() in line:
        
        
        
#

if read_from_data :
    inlines = data.split('\n')
if read_from_file :
    infl = open("Input")
    inlines = list(infl)
lines=[]

incomplete=False
left_count=0
right_count=0
temp_line = ''

for inline in inlines:
    # if Am I incomplete
        # count number of ( and ) and add it to my previous number
        # I check if ( == ) now
            # incomplete False
            # add this line to temp_line
            # add temp_line to lines
    # not incomplete
        # if '(' is not there its a regular line 
            # add it to lines
        # else count number of ( and check if its != count )
            # then add this to a temp_line and state that its incomplete
    if incomplete:
#        inline.strip(' ')
        inline_split = inline.split()
        inline_join = ' '.join(inline_split)
        newline = temp_line+' '+inline_join
#        newline.split()
#        ' '.join(newline.split())
        lines.append(newline)
        incomplete = False
    #    else:
        
    else:
        left_count = left_paren_count(inline) 
        right_count = right_paren_count(inline)
        if '(' not in inline:
            lines.append(inline)
        elif left_count!=right_count:
            temp_line = inline
            incomplete = True
        else:
            lines.append(inline)
        
for line in lines:
    if not (line == '\n' or line ==''):
        result = parser.parse(line)
        pass

if Write2File:
    sys.stdout.close()
#f = open('output_rule_file','w')
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
#    tok=parser.parse(data)
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
