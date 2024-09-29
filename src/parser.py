from tokens import TokenTypes, Token
import re


class Parser:
    def __init__(self, token_list):
        self.tokens = token_list
        self.current_index = 0

    #Methods for traversing through the tokens and checking if the syntax is as expected

    def peek(self):
        """Returns the current token without advancing."""
        return self.tokens[self.current_index]

    def is_at_end(self):
        """Checks if the parser has reached the end of input."""
        return self.peek().tokenType == TokenTypes.EOF

    def match(self, *token_types):
        """Tries to match the current token to the expected types."""
        if self.check_token(*token_types):
            return self.advance()
        return None

    def check_token(self, *expected_types):

        if self.is_at_end():
            return False

        return self.peek().tokenType.value in expected_types

    def advance(self):
        print(self.tokens[self.current_index].lexeme)
        if not self.is_at_end():
            self.current = self.tokens[self.current_index]
            self.current_index += 1
        return self.previous()

    def previous(self):

        return self.tokens[self.current_index - 1]

    def consume(self, error_msg, *expected_types):

        if self.check_token(*expected_types):

            self.advance()

        else:

            raise Exception(error_msg)

    def peekNext(self):
        print(self.tokens[self.current_index].lexeme)
        return self.tokens[self.current_index]

    #Grammar methods

    #Takes a bottom up approach to create the syntax tree

    def expression(self):
        return self.assignment()

    def assignment(self):
        # Check if we are looking at an assignment operation
        expr = self.equality()

        if self.match(TokenTypes.EQUALS.value):
            equals_token = self.previous()
            value = self.assignment()  # Recursively handle the right-hand side
            if isinstance(expr, str):  # The left-hand side should be a variable (IDENTIFIER)
                return (equals_token.tokenType, expr, value)
            raise Exception("Invalid assignment target.")

        return expr

    def equality(self):

        expr = self.comparison()

        while self.match(TokenTypes.EXCLAM_EQUALS.value, TokenTypes.EQUAL_TO.value):
            operator = self.previous()
            right_operand = self.comparison()
            expr = (operator.tokenType, expr, right_operand)

        return expr

    def comparison(self):
        expr = self.term()

        while self.match(TokenTypes.GREATER.value, TokenTypes.GREATER_EQUALS.value, TokenTypes.LESS.value,
                         TokenTypes.LESS_EQUALS.value):
            operator = self.previous()
            right = self.term()
            expr = (operator.type, expr, right)

        return expr

    def term(self):

        expr = self.factor()

        while self.match(TokenTypes.PLUS.value, TokenTypes.MINUS.value):
            operator = self.previous()
            right_operand = self.factor()
            expr = (operator.tokenType, expr, right_operand)

        return expr

    def factor(self):

        expr = self.unary()

        while self.match(TokenTypes.DIVIDE.value, TokenTypes.TIMES.value):
            operator = self.previous()
            right_operand = self.unary()
            expr = (operator.tokenType, expr, right_operand)

        return expr

    def unary(self):
        if self.match(TokenTypes.EXCLAM.value, TokenTypes.MINUS.value):
            operator = self.previous()
            right_operand = self.unary()
            return (operator.tokenType, right_operand)

        return self.primary()

    def primary(self):

        if self.match(TokenTypes.NUMBER.value, TokenTypes.STRING.value, TokenTypes.TRUE.value, TokenTypes.FALSE.value,
                      TokenTypes.IDENTIFIER.value):
            return self.previous().lexeme

        if self.match(TokenTypes.FUNCTION.value):
            return self.function_call()

        if self.match(TokenTypes.LEFT_PAREN.value):
            expr = self.expression()
            self.consume("Expect ')' after expression.", TokenTypes.RIGHT_PAREN.value)
            return expr

        return Exception("Expect Expression.")

    def function_call(self):

        function = self.previous()

        func_name = function.lexeme
        self.consume("Expect '(' after expression.", TokenTypes.LEFT_PAREN.value, TokenTypes.RIGHT_PAREN.value)

        if (function.tokenType == TokenTypes.PRINT_STATEMENT):
            print(self.peek())
            return
        
        args = []

        if not self.check_token(TokenTypes.RIGHT_PAREN.value):
            args = self.arguments()

        self.consume("Expect ')' after arguments.", TokenTypes.RIGHT_PAREN.value)

        if self.peekNext().tokenType.value == TokenTypes.LEFT_BRACKET.value:
            self.consume("Expect '{' to start function body.", TokenTypes.LEFT_BRACKET.value)

            # Parse the block within the curly braces
            block = self.block()

            # Consume the closing curly brace '}'
            self.consume("Expect '}' after function body.", TokenTypes.RIGHT_BRACKET.value)

            return (function.tokenType, func_name, args, block)

        return (function.tokenType, func_name, args)

    def arguments(self):

        args = [self.expression()]
        while self.match(TokenTypes.COMMA.value):
            args.append(self.expression())
        return args

    def block(self):
        statements = []

        # Keep parsing statements until we encounter the closing '}'
        while not self.check_token(TokenTypes.RIGHT_BRACKET.value) and not self.is_at_end():
            statements.append(self.statement())

        return statements

    def statement(self):
        # In a language without control flow constructs,
        # we'll only deal with expressions and assignments.

        # Handle expression statements like function calls or arithmetic
        expr = self.expression()

        return expr
