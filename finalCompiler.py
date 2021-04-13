#Humberto Tello
#Lenguajes y Traductores
#Entrega Proyecto final

import ply.lex as lex
import ply.yacc as yacc
import sys
import math

symbolTable = {}
temporals = {}
procedureTable = {}
cuadruplos = []
IDStack = []
jumpStack = []
callStack = []
temp = 0
base = 0
memory = []
M = 1
d = []
debugM = 0


reserved = {
   'if' : 'IF',
   'else' : 'ELSE',
   'while' : 'WHILE',
   'inP' : 'INPUT',
   'outP' : 'OUTPUT',
   'do' : 'DO',
   'until' : 'UNTIL',
   'for' : 'FOR',
   'in' : 'IN',
   'range' : 'RANGE',
   'of' : 'OF',
   'begin' : 'BEGIN',
   'end' : 'END',
   'return' : 'RETURN',
   'int' : 'INT',
   'double' : 'DOUBLE',
   'sin' : 'SIN',
   'cos' : 'COS',
   'tan' : 'TAN',
   'cot' : 'COT',
   'sec' : 'SEC',
   'csc' : 'CSC',
   'debug' : 'DEBUG',
   'on' : 'ON',
   'off' : 'OFF',
   'pi' : 'PI',
   'abs' : 'ABS'
}

tokens = [
    'EQUAL',
    'OPENPAR',
    'CLOSEPAR',
    'OPENKEY',
    'CLOSEKEY',
    'TWOP',
    'ID',
    'ADD',
    'MINUS',
    'TIMES',
    'DIVISION',
    'LESSTHAN',
    'GREATERTHAN',
    'LESSEQUAL',
    'GREATEREQUAL',
    'COMA',
    'AND',
    'OR',
    'OPENBRAC',
    'CLOSEBRAC',
    'NUMBER',
    'FLOAT',
    'STRING',
    'DIFFERENT'
] + list(reserved.values())


t_EQUAL = r'\='
t_DIFFERENT = r'\!'
t_OPENPAR = r'\('
t_CLOSEPAR = r'\)'
t_OPENKEY = r'\{'
t_CLOSEKEY = r'\}'
t_TWOP = r'\:'
t_ADD = r'\+'
t_MINUS = r'\-'
t_TIMES = r'\*'
t_DIVISION = r'\/'
t_LESSTHAN = r'\<'
t_GREATERTHAN = r'\>'
t_LESSEQUAL = r'\<\='
t_GREATEREQUAL = r'\>\='
t_COMA = r'\,'
t_AND = r'\&\&'
t_OR = r'\|\|'
t_OPENBRAC = r'\['
t_CLOSEBRAC = r'\]'

def t_newline(t):
    r'\n+'
    t.lexer.lineno += len(t.value)

t_ignore = ' \t'

def t_ID(t):
    r'[a-zA-Z_][a-zA-Z_0-9]*'
    t.type = reserved.get(t.value,'ID')
    return t

def t_STRING(t):
    r'"[a-zA-Z_ \\,()-][a-zA-Z_0-9 :?!$#%&()\\,]*"'
    t.type = 'STRING'
    return t


def t_FLOAT(t):
    r'\-?\d+\.\d+'
    t.type = 'FLOAT'
    return t

def t_NUMBER(t):
    r'\-?\d+'
    t.type = 'NUMBER'
    return t


def t_error(t):
	print("Illegal characters!")
	t.lexer.skip(1)

lexer = lex.lex()

def p_MAIN(p):
    '''
    MAIN : MAINAUX DEBUGAUX C BEGIN MAINAUX2 A B END
    '''
    cuadruplos.append(["ENDPROGRAM"])

def p_MAINAUX(p):
    '''
    MAINAUX :
    '''
    cuadruplos.append(["GOTO",0])

def p_DEBUGAUX(p):
    '''
    DEBUGAUX : DEBUG ON
    DEBUGAUX : DEBUG OFF
    DEBUGAUX : 
    '''
    if(len(p) > 1 and p[2] == 'on'):
        global debugM
        debugM = 1


def p_MAINAUX2(p):
    '''
    MAINAUX2 : 
    '''
    cuadruplos[0][1] = len(cuadruplos)

def p_A(p):
    '''
    A : A VAR
    A : A VAR ARRAY
    A : 
    '''
    if(len(p) == 4):
        global M
        global base
        global memory
        temp = []
        temp.append([int(d[0]),int(M/int(d[0]))])
        for i in range(1,len(d)):
            temp.append([int(d[i]),int(temp[i - 1][1] / int(d[i - 1]))])
        temp[len(temp) - 1][1] = base
        memory = memory + ['U'] * M
        base = base + M
        M = 1
        symbolTable[p[2]][1] = temp[:]
        d.clear()

def p_VAR(p):
    '''
    VAR : TYPE ID
    '''
    if(len(p) > 1):
        if p[2] in symbolTable.keys():
            print("Variable: " + p[2] + " already declared")
        symbolTable[p[2]] = [p[1],"U"]
        p[0] = p[2]

def p_TYPE(p):
    '''
    TYPE : INT
    TYPE : DOUBLE
    '''
    p[0] = p[1]

    
def p_ARRAY(p):
    '''
    ARRAY : ARRAYAUX OPENBRAC NUMBER CLOSEBRAC
    '''
    global M
    d.append(p[3])
    M = M * int(p[3])
def p_ARRAYAUX(p):
    '''
    ARRAYAUX : ARRAYAUX OPENBRAC NUMBER CLOSEBRAC
    ARRAYAUX :
    '''
    if(len(p) > 1):
        global M
        d.append(p[3])
        M = M * int(p[3])
def p_LOGICE(p):
    '''
    LOGICE : ORL
    '''

def p_ORL(p):
    '''
    ORL : ORL OR ANDL
    ORL : ANDL
    '''
    if(len(p) == 4):
        newtemp = next_temp()
        n2 = IDStack.pop()
        n1 = IDStack.pop()
        cuadruplos.append(["OR",n1,n2,newtemp])
        IDStack.append(newtemp)

def p_ANDL(p):
    '''
    ANDL : ANDL AND OPERL
    ANDL : OPERL
    '''
    if(len(p) == 4):
        newtemp = next_temp()
        n2 = IDStack.pop()
        n1 = IDStack.pop()
        cuadruplos.append(["AND",n1,n2,newtemp])
        IDStack.append(newtemp)

def p_OPERL(p):
    '''
    OPERL : OPERL OPERLOGIC E
    OPERL : E
    '''
    if(len(p) == 4):
        newtemp = next_temp()
        n2 = IDStack.pop()
        n1 = IDStack.pop()
        if(p[2] == '<='):
            cuadruplos.append(["<=",n1,n2,newtemp])
        elif(p[2] == '<'):
            cuadruplos.append(["<",n1,n2,newtemp])
        elif(p[2] == '>='):
            cuadruplos.append([">=",n1,n2,newtemp])
        elif(p[2] == '>'):
            cuadruplos.append([">",n1,n2,newtemp])
        elif(p[2] == "=="):
            cuadruplos.append(["==",n1,n2,newtemp])
        elif(p[2] == "!="):
            cuadruplos.append(["!=",n1,n2,newtemp])
        IDStack.append(newtemp)

def p_E(p):
    '''
    E : E ADDMINUS T
    E : T
    '''
    if(len(p) == 4):
        newtemp = next_temp()
        n2 = IDStack.pop()
        n1 = IDStack.pop()
        if(p[2] == '+'):
            cuadruplos.append(["ADD",n1,n2,newtemp])
        else:
            cuadruplos.append(["MINUS",n1,n2,newtemp])
        IDStack.append(newtemp)

def p_ADDMINUS(p):
    '''
    ADDMINUS : ADD
    ADDMINUS : MINUS
    '''
    p[0] = p[1]

def p_T(p):
    '''
    T : T TIMESDIV F
    T : F
    '''
    if(len(p) == 4):
        newtemp = next_temp()
        n2 = IDStack.pop()
        n1 = IDStack.pop()
        if(p[2] == '*'):
            cuadruplos.append(["TIMES",n1,n2,newtemp])
        else:
            cuadruplos.append(["DIV",n1,n2,newtemp])
        IDStack.append(newtemp)

def p_TIMESDIV(p):
    '''
    TIMESDIV : TIMES
    TIMESDIV : DIVISION
    '''
    p[0] = p[1]
    
def p_F(p):
    '''
    F : OPENPAR LOGICE CLOSEPAR
    F : ID
    F : ID ARRAYE
    F : NUMBER
    F : FLOAT
    F : PI
    F : TRIGFUNC OPENPAR LOGICE CLOSEPAR
    F : ABS OPENPAR LOGICE CLOSEPAR
    '''
    if(len(p) == 2):
        IDStack.append(p[1])
    if(len(p) == 3):
        global d
        IDStack.append([p[1],d[:]])
        d.clear()
    if(len(p) == 5):
        if(p[1] == 'abs'):
            newtemp = next_temp()
            cuadruplos.append(["ABS",IDStack.pop(),newtemp])
            IDStack.append(newtemp)
        else:
            newtemp = next_temp()
            cuadruplos.append([p[1],IDStack.pop(),newtemp])
            IDStack.append(newtemp)  

def p_TRIGFUNC(p):
    '''
    TRIGFUNC : SIN
    TRIGFUNC : COS
    TRIGFUNC : TAN
    TRIGFUNC : COT
    TRIGFUNC : SEC
    TRIGFUNC : CSC
    '''
    p[0] = p[1].upper()

def p_ARRAYE(p):
    '''
    ARRAYE : ARRAYAUXE OPENBRAC E CLOSEBRAC
    '''
    d.append(IDStack.pop())
def p_ARRAYAUXE(p):
    '''
    ARRAYAUXE : ARRAYAUXE OPENBRAC E CLOSEBRAC
    ARRAYAUXE :
    '''
    if(len(p) > 1):
        d.append(IDStack.pop())

def p_OPERLOGIC(p):
    '''
    OPERLOGIC : EQUAL EQUAL
    OPERLOGIC : LESSTHAN
    OPERLOGIC : GREATERTHAN
    OPERLOGIC : LESSEQUAL
    OPERLOGIC : GREATEREQUAL
    OPERLOGIC : DIFFERENT EQUAL
    '''
    if(len(p) == 2):
        p[0] = p[1]
    elif(len(p) == 3):
        p[0] = p[1] + p[2]



def p_B(p):
    '''
    B : ASSIGN B
    B : INPUTRULE B
    B : OUTPUTRULE B
    B : IFRULE B
    B : WHILERULE B
    B : DOWHILERULE B
    B : FORRULE B
    B : CALLRULE B
    B : RETURNRULE B
    B : 
    '''

def p_RETURNRULE(p):
    '''
    RETURNRULE : RETURN
    '''
    cuadruplos.append(["ENDPROCEDURE"])

def p_INPUTRULE(p):
    '''
    INPUTRULE : ID EQUAL INPUT OPENPAR CLOSEPAR
    INPUTRULE : ID ARRAYE EQUAL INPUT OPENPAR CLOSEPAR
    '''
    if(len(p) == 7):
        global d
        cuadruplos.append(["INPUT",[p[1],d[:]]])
        d.clear()
    else:
        cuadruplos.append(["INPUT",p[1]])

def p_OUTPUTRULE(p):
    '''
    OUTPUTRULE : OUTPUT OPENPAR OUTAUX CLOSEPAR
    '''

def p_OUTAUX(p):
    '''
    OUTAUX : LOGICE
    OUTAUX : STRING STRINGAUX
    '''
    if(len(p) == 2):
        cuadruplos.append(["OUTPUT",IDStack.pop(),'0'])
    else:
        cuadruplos.append(["OUTPUT",p[1],'1'])

def p_STRINGAUX(p):
    '''
    STRINGAUX :
    '''




def p_CALLRULE(p):
    '''
    CALLRULE : ID OPENPAR CLOSEPAR
    '''
    cuadruplos.append(["CALL",procedureTable[p[1]]])

def p_DOWHILERULE(p):
    '''
    DOWHILERULE : DO DOWHILEAUX B UNTIL OPENPAR LOGICE CLOSEPAR
    '''
    cuadruplos.append(["GOTOF",IDStack.pop(),jumpStack.pop()])

def p_DOWHILEAUX(p):
    '''
    DOWHILEAUX :
    '''
    jumpStack.append(len(cuadruplos))

def p_FORRULE(p):
    '''
    FORRULE : FOR FORAUX1 IN RANGE OF OPENPAR FORAUX2 COMA FORAUX3 CLOSEPAR B END
    '''
    top = IDStack[len(IDStack) - 1]
    IDStack.pop()
    cuadruplos.append(["ADD",top,'1','t0'])
    cuadruplos.append(["EQUAL",'t0',top])
    RETURN = jumpStack.pop()
    cuadruplos.append(["GOTO",RETURN])
    cuadruplos[RETURN + 1][2] = len(cuadruplos)

def p_FORAUX1(p):
    '''
    FORAUX1 : ID
    '''
    IDStack.append(p[1])

def p_FORAUX2(p):
    '''
    FORAUX2 : E
    '''
    Exp = IDStack.pop()
    top = IDStack[len(IDStack) - 1]
    cuadruplos.append(["EQUAL",Exp,top])

def p_FORAUX3(p):
    '''
    FORAUX3 : E
    '''
    newtemp1 = next_temp()
    Exp = IDStack.pop()
    newtemp2 = next_temp()
    cuadruplos.append(["ADD",Exp,'0',newtemp1])
    cuadruplos.append(["<=",IDStack[len(IDStack) - 1],newtemp1,newtemp2])
    cuadruplos.append(["GOTOF",newtemp2,0])
    jumpStack.append(len(cuadruplos) - 2)



def p_WHILERULE(p):
    '''
    WHILERULE : WHILE WHILEAUX1 OPENPAR LOGICE CLOSEPAR DO WHILEAUX2 B END
    '''
    jump = jumpStack.pop()
    ret = jumpStack.pop()
    cuadruplos.append(["GOTO",ret])
    cuadruplos[jump][2] = len(cuadruplos)

def p_WHILEAUX1(p):
    '''
    WHILEAUX1 :
    '''
    jumpStack.append(len(cuadruplos))

def p_WHILEAUX2(p):
    '''
    WHILEAUX2 : 
    '''
    cuadruplos.append(["GOTOF",IDStack.pop(),0])
    jumpStack.append(len(cuadruplos) - 1)

def p_IFRULE(p):
    '''
    IFRULE : IF OPENPAR LOGICE IFAUX1 CLOSEPAR OPENKEY B CLOSEKEY X
    '''
    jump = jumpStack.pop()
    if(len(cuadruplos[jump]) == 2):
        cuadruplos[jump][1] = len(cuadruplos)
    else:
        cuadruplos[jump][2] = len(cuadruplos)

def p_ASSIGN(p):
    '''
    ASSIGN : ID EQUAL LOGICE
    ASSIGN : ID ARRAYE EQUAL ASSIGNAUX LOGICE
    '''
    if(len(p) == 4):
        cuadruplos.append(["EQUAL",IDStack.pop(),p[1]])
    else:
        global d
        cuadruplos.append(["EQUAL",IDStack.pop(),[p[1],p[4]]])
        d.clear()
    

def p_ASSIGNAUX(p):
    '''
    ASSIGNAUX : 
    '''
    global d
    p[0] = d[:]
    d.clear()
    
            
def p_IFAUX1(p):
    '''
    IFAUX1 :
    '''
    cuadruplos.append(["GOTOF",IDStack.pop(),0])
    jumpStack.append(len(cuadruplos) - 1)

def p_ELSEAUX(p):
    '''
    ELSEAUX : 
    '''
    cuadruplos.append(["GOTO",0])
    jump = jumpStack.pop()
    cuadruplos[jump][2] = len(cuadruplos)
    jumpStack.append(len(cuadruplos) - 1)

def p_X(p):
    '''
    X : ELSE ELSEAUX OPENKEY B CLOSEKEY
    X : 
    '''


def p_C(p):
    '''
    C : PROCEDUREAUX TWOP A B PROCEDUREAUX2 END C
    C : 
    '''
        
def p_PROCEDUREAUX(p):
    '''
    PROCEDUREAUX : ID
    '''
    procedureTable[p[1]] = len(cuadruplos)

def p_PRCEDUREAUX2(p):
    '''
    PROCEDUREAUX2 : 
    '''
    cuadruplos.append(["ENDPROCEDURE"])


def p_error(p):
    print("Error in line " + str(p.lineno))
    sys.exit()


def next_temp():
    global temp
    temp = temp + 1
    tempn = "t" + str(temp)
    return tempn

parser = yacc.yacc()

'''
Test file name
'''
file1 = open('test.txt','r')
info = file1.read()
parser.parse(info)

if(debugM):
    print("Symbol Table: ",symbolTable )
    print("Variables finales: ",IDStack)
    print("Cuadruplos generados: ",cuadruplos)
    print("Pocedures", procedureTable)
    print("jump",jumpStack)
    print("digits",d)
    print("memory",memory)
    print("------------------------DEBUG END------------------------")        

def findAddress(temp):
    address = 0
    for i in range(len(temp[1]) - 1):
        '''
        print(findNumber(symbolTable[temp[0]][1][i][1]))
        print(findNumber(temp[1][i]))
        '''
        address = address + findNumber(symbolTable[temp[0]][1][i][1])*findNumber(temp[1][i])
    '''
    print(findNumber(symbolTable[temp[0]][1][len(symbolTable[temp[0]][1]) - 1][1]))
    print(findNumber(temp[1][len(temp[1]) - 1]))
    '''
    address = address + findNumber(symbolTable[temp[0]][1][len(symbolTable[temp[0]][1]) - 1][1]) + findNumber(temp[1][len(temp[1]) - 1])
    '''
    print(address)
    '''
    return address

def findNumber(temp):
    temp1 = 0
    if(type(temp) == list):
        temp1 = memory[findAddress(temp)]
        if(symbolTable[temp[0]][0] == 'int'):
            temp1 = int(temp1)
        else:
            temp1 = float(temp1)
    else:
        if(type(temp) == int or temp.isdigit()):
            temp1 = int(temp)
        elif(temp.find('.') != -1):
            temp1 = float(temp)
        elif(temp == 'pi'):
            temp1 = math.pi
        else:
            temp1 = symbolTable.get(temp,temporals.get(temp))
            if(type(temp1) == list):
                if(temp1[0] == 'int'):
                    temp1 = int(temp1[1])
                else:
                    temp1 = float(temp1[1])     
    return temp1

def Execute():
    PC = 0
    while(cuadruplos[PC][0] != "ENDPROGRAM"):
        if(cuadruplos[PC][0] == "GOTO"):
            PC = cuadruplos[PC][1]
        elif(cuadruplos[PC][0] == "CALL"):
            callStack.append(PC + 1)
            PC = cuadruplos[PC][1]
        elif(cuadruplos[PC][0] == "ENDPROCEDURE"):
            PC = callStack.pop()
        elif(cuadruplos[PC][0] == "GOTOF"):
            falsetemp = 0
            falsetemp = findNumber(cuadruplos[PC][1])
            if(falsetemp < 1):
                PC = cuadruplos[PC][2]
            else:
                PC = PC + 1
        else:
            PC = PC + 1
        if(cuadruplos[PC][0] == "ADD" or cuadruplos[PC][0] == "MINUS" or cuadruplos[PC][0] == "TIMES" or cuadruplos[PC][0] == "DIV" or cuadruplos[PC][0] == "<" or cuadruplos[PC][0] == "<=" or cuadruplos[PC][0] == ">" or cuadruplos[PC][0] == ">=" or cuadruplos[PC][0] == "==" or cuadruplos[PC][0] == "OR" or cuadruplos[PC][0] == "AND" or cuadruplos[PC][0] == "!="):
            temp1 = 0
            temp2 = 0
            temp1 = findNumber(cuadruplos[PC][1])
            temp2 = findNumber(cuadruplos[PC][2])
            if(cuadruplos[PC][0] == "ADD"):
                temporals[cuadruplos[PC][3]] = temp1 + temp2
            elif(cuadruplos[PC][0] == "MINUS"):
                temporals[cuadruplos[PC][3]] = temp1 - temp2
            elif(cuadruplos[PC][0] == "TIMES"):
                temporals[cuadruplos[PC][3]] = temp1 * temp2
            elif(cuadruplos[PC][0] == "DIV"):
                temporals[cuadruplos[PC][3]] = temp1 / temp2
            elif(cuadruplos[PC][0] == "OR"):
                temporals[cuadruplos[PC][3]] = temp1 or temp2
            elif(cuadruplos[PC][0] == "AND"):
                temporals[cuadruplos[PC][3]] = temp1 and temp2
            elif(cuadruplos[PC][0] == "<="):
                if(temp1 <= temp2):
                    temporals[cuadruplos[PC][3]] = 1
                else:
                    temporals[cuadruplos[PC][3]] = 0
            elif(cuadruplos[PC][0] == "<"):
                if(temp1 < temp2):
                    temporals[cuadruplos[PC][3]] = 1
                else:
                    temporals[cuadruplos[PC][3]] = 0
            elif(cuadruplos[PC][0] == ">"):
                if(temp1 > temp2):
                    temporals[cuadruplos[PC][3]] = 1
                else:
                    temporals[cuadruplos[PC][3]] = 0
            elif(cuadruplos[PC][0] == ">="):
                if(temp1 >= temp2):
                    temporals[cuadruplos[PC][3]] = 1
                else:
                    temporals[cuadruplos[PC][3]] = 0
            elif(cuadruplos[PC][0] == "=="):
                if(temp1 == temp2):
                    temporals[cuadruplos[PC][3]] = 1
                else:
                    temporals[cuadruplos[PC][3]] = 0
            elif(cuadruplos[PC][0] == "!="):
                if(temp1 != temp2):
                    temporals[cuadruplos[PC][3]] = 1
                else:
                    temporals[cuadruplos[PC][3]] = 0
        if(cuadruplos[PC][0] == "SIN" or cuadruplos[PC][0] == "COS" or cuadruplos[PC][0] == "TAN" or cuadruplos[PC][0] == "COT" or cuadruplos[PC][0] == "SEC" or cuadruplos[PC][0] == "CSC" or cuadruplos[PC][0] == "ABS"):
            trigtemp = findNumber(cuadruplos[PC][1])
            if(cuadruplos[PC][0] == "SIN"):
                temporals[cuadruplos[PC][2]] = math.sin(trigtemp)
            elif(cuadruplos[PC][0] == "COS"):
                temporals[cuadruplos[PC][2]] = math.cos(trigtemp)
            elif(cuadruplos[PC][0] == "TAN"):
                temporals[cuadruplos[PC][2]] = math.tan(trigtemp)
            elif(cuadruplos[PC][0] == "COT"):
                temporals[cuadruplos[PC][2]] = 1/math.tan(trigtemp)
            elif(cuadruplos[PC][0] == "SEC"):
                temporals[cuadruplos[PC][2]] = 1/math.cos(trigtemp)
            elif(cuadruplos[PC][0] == "CSC"):
                temporals[cuadruplos[PC][2]] = 1/math.sin(trigtemp)
            elif(cuadruplos[PC][0] == "ABS"):
                temporals[cuadruplos[PC][2]] = abs(trigtemp)
            
        if(cuadruplos[PC][0] == "EQUAL"):
            equaltemp = findNumber(cuadruplos[PC][1])
            '''
            print("EQUAL")
            print(cuadruplos[PC][2])
            '''
            if(type(cuadruplos[PC][1]) != list):
                if(type(cuadruplos[PC][2]) != list):
                    symbolTable[cuadruplos[PC][2]][1] = equaltemp
                elif(type(symbolTable[cuadruplos[PC][2][0]][1]) == list):
                    memory[findAddress(cuadruplos[PC][2])] = equaltemp
            elif(type(symbolTable[cuadruplos[PC][2][0]][1]) == list):

                 memory[findAddress(cuadruplos[PC][2])] = equaltemp
            else:
                symbolTable[cuadruplos[PC][2]][1] = equaltemp
            

        if(cuadruplos[PC][0] == "OUTPUT"):
            if(cuadruplos[PC][2] == '0'): 
                outtemp = findNumber(cuadruplos[PC][1])
                print (outtemp,end = '')
            else:
                outtemp = cuadruplos[PC][1][1:len(cuadruplos[PC][1]) - 1]
                if(outtemp == "\\n"):
                    print('')
                else:
                    print(cuadruplos[PC][1][1:len(cuadruplos[PC][1]) - 1],end = '')
        elif(cuadruplos[PC][0] == "INPUT"):
            intemp = input()
            if(type(cuadruplos[PC][1]) != list):
                if(symbolTable[cuadruplos[PC][1]][0] == 'int'):
                    symbolTable[cuadruplos[PC][1]][1] = int(intemp)
                else:
                    symbolTable[cuadruplos[PC][1]][1] = float(intemp)
            elif(type(symbolTable[cuadruplos[PC][1][0]][1]) == list):
                if(symbolTable[cuadruplos[PC][1][0]][0] == 'int'):
                    memory[findAddress(cuadruplos[PC][1])] = int(intemp)
                else:
                    memory[findAddress(cuadruplos[PC][1])] = float(intemp)
            else:
                if(symbolTable[cuadruplos[PC][1]][0] == 'int'):
                    symbolTable[cuadruplos[PC][1]][1] = int(intemp)
                else:
                    symbolTable[cuadruplos[PC][1]][1] = float(intemp) 
        
            

Execute()

if(debugM):
    print("Temporals: ",temporals)
    print("Symbol Table: ",symbolTable)
    print("Memory",memory)


'''
Lines = file1.readlines()
s = ""
for line in Lines:2
  try:
    s += line.strip()
  except EOFError:
    break
parser.parse(s)
'''



