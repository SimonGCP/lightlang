import sys

from parser import Parser
from tokens import Token, TokenTypes

SUCCESS = 0
FAILURE = 1

def isDigit(char):
    return char >= "0" and char <= "9"

def isAlpha(char):
    return char >= "a" and char <= "z"

# scans the code and returns a list of tokens
class Scanner():
    def __init__(self, source):
        self.source = source
        self.start = 0
        self.current = 0
        self.line = 1
        self.tokens = []
    
    def scanTokens(self):
        error = False

        while (self.isAtEnd() == False):
            self.start = self.current
            if (self.scanToken() == False):
                error = error(self.line, self.source[self.start:self.current], "Unexpected character")

        self.addToken(TokenTypes.EOF)

        return error
    
    def isAtEnd(self):
        return self.current >= len(self.source)
    
    def advance(self):
        cur = self.source[self.current]
        self.current += 1
        return cur
    
    def peek(self):
        if self.isAtEnd():
            return '\0'

        return self.source[self.current]
    
    def string(self):
        source = self.source

        while (self.peek() != '"' and not self.isAtEnd()):
            self.advance()

        if (self.isAtEnd()):
            return False
        
        self.advance()

        value = source[self.start + 1 : self.current - 1]
        self.addTokenWithLiteral(TokenTypes.STRING, value)

    def number(self):
        source = self.source

        while(isDigit(self.peek()) and not self.isAtEnd()):
            self.advance()

        if (self.peek() == '.' and not self.isAtEnd()): self.advance()

        while(isDigit(self.peek()) and not self.isAtEnd()):
            self.advance()

        number = float(source[self.start :  self.current])
        self.addTokenWithLiteral(TokenTypes.NUMBER, number)

    def match(self, expected):
        if (self.isAtEnd()): return False
        if (self.source[self.current] != expected): return False

        self.current += 1
        return True

    def peekNext(self, expected):

        if (self.isAtEnd()): return False
        if (self.source[self.current] != expected): return False

        return True
        
    
    def scanToken(self):
        c = self.advance()
        match c:
            case "{":
                self.addToken(TokenTypes.LEFT_BRACKET)
            case "}":
                self.addToken(TokenTypes.RIGHT_BRACKET)
            case "(":
                self.addToken(TokenTypes.LEFT_PAREN)
            case ")":
                self.addToken(TokenTypes.RIGHT_PAREN)
            case "+":
                self.addToken(TokenTypes.PLUS)
            case "-":
                self.addToken(TokenTypes.MINUS)
            case "*":
                self.addToken(TokenTypes.TIMES)
            case "/":
                self.addToken(TokenTypes.DIVIDE)
            case ".":
                self.addToken(TokenTypes.DOT)
            case ",":
                self.addToken(TokenTypes.COMMA)
            case "=":
                if (self.match("=")): self.addToken(TokenTypes.EQUALS_EQUALS)
                else: self.addToken(TokenTypes.EQUALS)
            case "<":
                if (self.match("=")): self.addToken(TokenTypes.LESS_EQUALS)
                else: self.addToken(TokenTypes.LESS)
            case ">":
                if (self.match("=")): self.addToken(TokenTypes.GREATER_EQUALS)
                else: self.addToken(TokenTypes.GREATER)
            case "!":
                if (self.match("=")): self.addToken(TokenTypes.EXCLAM_EQUALS)
                if (self.match("t")): self.addToken(TokenTypes.FALSE)
                else: self.addToken(TokenTypes.EXCLAM)
            case "t":
                self.addToken(TokenTypes.TRUE)
            case "p":
                self.addToken(TokenTypes.PRINT_STATEMENT)
            case '"':
                self.string()
            case " ": return True # ignore whitespace
            case "\t": return True
            case "\r": return True
            case "\n":
                self.line += 1
                return True
            case _:
                if (isAlpha(c)):
                    next = self.current + 1
                    if self.peekNext("("):
                        self.addToken(TokenTypes.FUNCTION)
                    else:
                        self.addToken(TokenTypes.IDENTIFIER)
                    return True
                elif (isDigit(c)):
                    self.number()
                    return True
                
                return False
        return True       
    
    def addToken(self, type):
        self.addTokenWithLiteral(type, None)

    def addTokenWithLiteral(self, type, literal):
        text = self.source[self.start : self.current]
        self.tokens.append(Token(type, text, literal, self.line))

# scans a single line for tokens


def error(lineNumber, line, message):
    lineNumber = str(lineNumber)
    print("Error at line", lineNumber, ": " + message + " you dumb BITCH")
    print(lineNumber + ": " + line)

    return FAILURE

def lexer(argv):

    if (len(argv) < 2):
        print("usage: lexer.py <name of .light file>")
        return 1

    file = open(argv[1])
    scanner = Scanner(file.read())
    scanner.scanTokens()
    return scanner.tokens

class TreeNode:
    def __init__(self, token_type, value, children=None):
        self.token_type = token_type
        self.value = value
        self.children = children if children else []

    def __repr__(self):
        return f"TreeNode({self.token_type}, {self.value})"

def build_tree(node):
    if isinstance(node, tuple):
        token_type, value, *children = node
        tree_node = TreeNode(token_type, value)
        
        for child in children:
            tree_node.children.append(build_tree(child))
        
        return tree_node
    elif isinstance(node, list):
        tree_node = TreeNode(TokenTypes.LIST, "list")
        for child in node:
            tree_node.children.append(build_tree(child))

        return tree_node
    else:
        # Handle the case for non-tuple values (e.g., strings, numbers)
        return TreeNode('VALUE', node)
    

def traverse(node):
    if node is None:
        return
    
    children = node.children
    if (len(children) > 0):
        traverse(children[0])
    if (len(children) > 1):
        traverse(children[1])

    node.value = evaluate(node)
    print(node)





def evaluate(node):
    match node.token_type:
        case TokenTypes.EQUALS:
            print(node.value, "=", node.children[0].value)
            return node.children[0].value
        case TokenTypes.PLUS:
            total = float(node.value)
            for children in node.children:
                total += float(children.value)
            return total
        case TokenTypes.MINUS:
            total = float(node.value)
            for children in node.children:
                total -= float(children.value)
            return total
        case TokenTypes.TIMES:
            total = float(node.value)
            for children in node.children:
                total *= float(children.value)
        case TokenTypes.NOT_EQUALS:
            return node.value != node.children[0]

        case TokenTypes.DIVIDE:
            total = float(node.value)
            for children in node.children:
                total /= float(children.value)
            return total
        case TokenTypes.EQUALS_EQUALS:
            return node.value == node.children[0]
        case _:
            return node.value


def main():
    argv = sys.argv
    
    token_list = lexer(argv)
    for token in token_list:
        print(token.toString())

    parser = Parser(token_list)

    a = parser.expression()
    # print(a)
    ast = build_tree(a)

    # print(ast)
    traverse(ast)


    return 0

if __name__ == "__main__":
    main()
