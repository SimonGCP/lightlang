import sys
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

def main(): 
    argv = sys.argv
    
    if (len(argv) < 2):
        print("usage: lexer.py <name of .light file>")
        return 1

    file = open(argv[1])
    scanner = Scanner(file.read())
    scanner.scanTokens()
    for token in scanner.tokens:
        print(token.toString())

    return 0

if __name__ == "__main__":
    main()
