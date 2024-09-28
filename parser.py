class Parser:
    def __init__(self, tokens):
        self.tokens = tokens
        self.current_index = 0

    #Methods for traversing through the tokens and checking if the syntax is as expected

    def peek(self):
        """Returns the current token without advancing."""
        return self.tokens[self.current_index]

    def is_at_end(self):
        """Checks if the parser has reached the end of input."""
        return self.peek().type == 'EOF'

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

    def consume(self, *expected_types, error_msg):

        if self.check_token(expected_types):

            self.advance()

        else:

            raise Exception(error_msg)

    #Grammar methods

    #Takes a bottom up approach to create the syntax tree

    def expression(self):

        return self.equality()

    def equality(self):

        expr = self.comparison()

        while self.match()
