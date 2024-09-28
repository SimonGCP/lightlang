from enum import Enum

class TokenTypes(Enum):
    # single characters
    LEFT_BRACKET=0
    RIGHT_BRACKET=1
    LEFT_PAREN=2
    RIGHT_PAREN=3
    PLUS=4
    MINUS=5
    TIMES=6
    DIVIDE=7
    DOT=12
    COMMA=13
    EQUALS=14
    NOT_EQUALS=15
    EQUAL_TO=16
    LESS_EQUALS=15
    EQUALS_EQUALS=16
    GREATER_EQUALS=17
    LESS=18
    GREATER=19
    EXCLAM=20
    EXCLAM_EQUALS=21
    TRUE=22
    FALSE=23

    # literals
    IDENTIFIER=8
    STRING=9
    NUMBER=10

    # keywords
    FUNCTION=11
    PRINT_STATEMENT=12
    EOF=999


class Token():
    def __init__(self, tokenType, lexeme, literal, line):
        self.tokenType = tokenType
        self.lexeme = lexeme
        self.literal = literal
        self.line = line

    def toString(self):
        return (self.tokenType, self.lexeme, self.literal, self.line)
