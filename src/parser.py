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
        return self.peek().tokenType == 'EOF'

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

        if not self.is_at_end():
            self.current_index += 1

        return self.previous()

    def previous(self):

        return self.tokens[self.current_index - 1]

    def consume(self, error_msg, *expected_types):

        if self.check_token(*expected_types):

            self.advance()

        else:

            raise Exception(error_msg)

    #Grammar methods

    #Takes a bottom up approach to create the syntax tree

    def expression(self):
        print(len(self.tokens))
        return self.equality()

    def equality(self):

        expr = self.comparison()

        while self.match(TokenTypes.EXCLAM_EQUALS.value, TokenTypes.EQUAL_TO.value):
            operator = self.previous()
            right_operand = self.comparison()
            expr = (operator.tokenType, expr, right_operand)

        return expr

    def comparison(self):
        expr = self.term()

        while self.match(TokenTypes.GREATER.value, TokenTypes.GREATER_EQUALS.value, TokenTypes.LESS.value, TokenTypes.LESS_EQUALS.value):
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

        if self.match(TokenTypes.NUMBER.value, TokenTypes.STRING.value, TokenTypes.TRUE.value, TokenTypes.FALSE.value):

            return self.previous().lexeme

        if self.match(TokenTypes.FUNCTION.value):
            return self.function_call()

        if self.match(TokenTypes.LEFT_PAREN.value):
            expr = self.expression()
            self.consume("Expect ')' after expression.", TokenTypes.RIGHT_PAREN.value)
            return expr

        return Exception("Expect Expression.")

    def function_call(self):

        func_name = self.previous().lexeme

        self.consume("Expect '(' after expression.", TokenTypes.LEFT_PAREN.value, TokenTypes.RIGHT_PAREN.value)

        args = []

        if not self.check_token(TokenTypes.RIGHT_PAREN.value):

            args = self.arguments()

        self.consume("Expect ')' after arguments.", TokenTypes.RIGHT_PAREN.value)

        return ("function_call", func_name, args)

    def arguments(self):

        args = [self.expression()]
        while self.match(TokenTypes.COMMA.value):
            args.append(self.expression())
        return args


