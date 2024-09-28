from tokens import TokenTypes, Token

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

        return self.peek().tokenType in expected_types

    def advance(self):

        if not self.is_at_end():
            self.current_index += 1

        return self.previous()

    def previous(self):

        return self.tokens[self.current_index - 1]

    def consume(self, error_msg, *expected_types):

        if self.check_token(expected_types):

            self.advance()

        else:

            raise Exception(error_msg)

    #Grammar methods

    #Takes a bottom up approach to create the syntax tree

    def expression(self):

        return self.equality()

    def equality(self):

        expr = self.term()

        while self.match(TokenTypes.NOT_EQUALS, TokenTypes.EQUAL_TO):
            operator = self.previous()
            right_operand = self.term()
            expr = (operator.tokenType, expr, right_operand)

        return expr

    def term(self):

        expr = self.factor()

        while self.match(TokenTypes.PLUS, TokenTypes.MINUS):
            operator = self.previous()
            right_operand = self.factor()
            expr = (operator.tokenType, expr, right_operand)

        return expr

    def factor(self):

        expr = self.unary()

        while self.match(TokenTypes.DIVIDE, TokenTypes.TIMES):

            operator = self.previous()
            right_operand = self.unary()
            expr = (operator.tokenType, expr, right_operand)

        return expr

    def unary(self):
        if self.match(TokenTypes.EXCLAMATION, TokenTypes.MINUS):
            operator = self.previous()
            right_operand = self.unary()
            return (operator.tokenType, right_operand)

        return self.primary()


    def primary(self):

        if self.match(TokenTypes.NUMBER, TokenTypes.STRING, TokenTypes.TRUE, TokenTypes.FALSE):

            return self.previous().lexeme

        if self.match(TokenTypes.FUNCTION):
            return self.function_call()

        if self.match(TokenTypes.LEFT_PAREN):
            expr = self.expression()
            self.consume("Expect ')' after expression.", TokenTypes.RIGHT_PAREN)
            return expr

        return Exception("Expect Expression.")

    def function_call(self):

        func_name = self.previous().lexeme

        self.consume("Expect '(' after expression.", TokenTypes.LEFT_PAREN)

        args = []

        if not self.check_token(TokenTypes.RIGHT_PAREN):

            args = self.arguments()

        self.consume("Expect ')' after arguments.", TokenTypes.RIGHT_PAREN)

        return ("function_call", func_name, args)

    def arguments(self):

        args = [self.expression()]
        while self.match(TokenTypes.COMMA):
            args.append(self.expression())
        return args


