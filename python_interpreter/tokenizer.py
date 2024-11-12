from enum import Enum, auto
from typing import Optional


class TokenType(Enum):
    LEFT_PAREN = auto()
    RIGHT_PAREN = auto()
    INT = auto()
    FLOAT = auto()
    SYMBOL = auto()


class Token:
    token_type: TokenType
    lexeme: str
    literal: object
    line: int

    def __init__(self, token_type: TokenType, literal: object, line: int):
        self.token_type = token_type
        self.literal = literal
        self.line = line

    def __repr__(self):
        text = str(self.token_type)

        if self.literal is not None:
            text += f" {self.literal}"

        return text


class Scanner:
    source: str
    tokens: list[Token]

    start: int
    current: int
    line: int

    def __init__(self, source: str):
        self.tokens = []
        self.source = source
        self.start = 0
        self.current = 0
        self.line = 1

    def isAtEnd(self):
        return self.current >= len(self.source)

    def advance(self):
        c = self.source[self.current]
        self.current += 1
        return c

    def addToken(self, token_type: TokenType, literal: Optional[object] = None):
        self.tokens.append(Token(token_type, literal, self.line))

    def peek(self):
        if self.isAtEnd():
            return '\0'
        return self.source[self.current]

    def peekNext(self):
        if self.current + 1 >= len(self.source):
            return '\0'
        return self.source[self.current + 1]

    def string(self):
        while self.peek() != '"' and not self.isAtEnd():
            if self.peek() == '\n':
                self.line += 1
            self.advance()

        if self.isAtEnd():
            raise Exception("Unterminated string!")

        # eat the closing quote
        self.advance()

        text = self.source[self.start + 1:self.current - 1]
        self.addToken(TokenType.SYMBOL, text)

    def number(self):
        while self.peek().isnumeric():
            self.advance()

        if self.peek() == '.' and self.peekNext().isnumeric():
            self.advance()
            while self.peek().isnumeric():
                self.advance()

        num_string = self.source[self.start: self.current]

        try:
            self.addToken(TokenType.INT, int(num_string))
        except:
            self.addToken(TokenType.FLOAT, float(num_string))

    def is_symbolic(self, c: str):
        return c.isalnum() or c == "-" or c == "_"

    def symbol(self):
        while self.is_symbolic(self.peek()) and not self.isAtEnd():
            self.advance()

        if self.isAtEnd():
            raise Exception("Unterminated symbol!")

        text = self.source[self.start:self.current]
        self.addToken(TokenType.SYMBOL, text)

    def scanToken(self):
        c = self.advance()
        match c:
            case '(':
                self.addToken(TokenType.LEFT_PAREN)
            case ')':
                self.addToken(TokenType.RIGHT_PAREN)
            case '*':
                self.addToken(TokenType.SYMBOL, "*")
            case '/':
                self.addToken(TokenType.SYMBOL, "/")
            case '-':
                self.addToken(TokenType.SYMBOL, "-")
            case '+':
                self.addToken(TokenType.SYMBOL, "+")
            case ' ':
                pass
            case '\r':
                pass
            case '\t':
                pass
            case '\n':
                self.line += 1
            case '"':
                self.string()
            case _:
                if c.isnumeric():
                    self.number()
                elif self.is_symbolic(c):
                    self.symbol()
                else:
                    raise Exception(f"Invalid character! {c}")

    def scanTokens(self) -> list[Token]:
        while not self.isAtEnd():
            self.start = self.current
            self.scanToken()
        return self.tokens


if __name__ == "__main__":
    source = """
                (begin
                    (define food "my hamburger")
                    (print food)
                    (define x 10) 
                    (define (my-func) 
                        (lambda (y) (* y y)))
                    (print (my-func x))
                )
            """
    scanner = Scanner(source)
    print(scanner.scanTokens())
