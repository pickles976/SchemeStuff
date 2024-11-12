import math
import operator as op
from tokenizer import Scanner, TokenType, Token

Symbol = str  # A Scheme Symbol is implemented as a Python str
Number = (int, float)  # A Scheme Number is implemented as a Python int or float
Atom = (Symbol, Number)  # A Scheme Atom is a Symbol or Number
List = list  # A Scheme List is implemented as a Python list
Exp = (Atom, List)  # A Scheme expression is an Atom or List


class Env(dict):
    "An environment: a dict of {'var': val} pairs, with an outer Env."

    def __init__(self, parms=(), args=(), outer=None):
        self.update(zip(parms, args))
        self.outer = outer

    def find(self, var):
        return self if (var in self) else self.outer.find(var)


class Procedure(object):
    """A user-defined Scheme procedure."""

    def __init__(self, parms, body, env):
        self.parms, self.body, self.env = parms, body, env

    def __call__(self, *args):
        return evaluate(self.body, Env(self.parms, args, self.env))


def standard_env() -> Env:
    """Global environment with standard Scheme procedures"""
    env = Env()
    env.update(vars(math))  # sin, cos, sqrt, pi, ...
    env.update({
        '+': op.add, '-': op.sub, '*': op.mul, '/': op.truediv,
        '>': op.gt, '<': op.lt, '>=': op.ge, '<=': op.le, '=': op.eq,
        'abs': abs,
        'append': op.add,
        'apply': lambda proc, args: proc(*args),
        'begin': lambda *x: x[-1],
        'car': lambda x: x[0],
        'cdr': lambda x: x[1:],
        'cons': lambda x, y: [x] + y,
        'eq?': op.is_,
        'expt': pow,
        'equal?': op.eq,
        'length': len,
        'list': lambda *x: List(x),
        'list?': lambda x: isinstance(x, List),
        'map': map,
        'max': max,
        'min': min,
        'not': op.not_,
        'null?': lambda x: x == [],
        'number?': lambda x: isinstance(x, Number),
        'print': print,
        'procedure?': callable,
        'round': round,
        'symbol?': lambda x: isinstance(x, Symbol),
    })
    return env


global_env = standard_env()


# is a mapping of {variable: value}


# def tokenize(chars: str) -> list:
#     "Convert a string of characters into a list of tokens"
#     return chars.replace("(", " ( ").replace(")", " ) ").split()


def parse(tokens: list[Token]) -> Exp:
    return read_from_tokens(tokens)


def read_from_tokens(tokens: list[Token]) -> Exp:
    """
        Recursively build a list of Atom | List
    """
    if len(tokens) == 0:
        raise SyntaxError("Unexpected EOF")
    token = tokens.pop(0)
    if token.token_type == TokenType.LEFT_PAREN:
        L = []  # start creating a list of tokens
        while tokens[0].token_type != TokenType.RIGHT_PAREN:
            L.append(read_from_tokens(tokens))
        tokens.pop(0)  # pop off ")"
        return L
    elif token.token_type == TokenType.RIGHT_PAREN:
        raise SyntaxError("Unexpected )")
    else:
        return atom(token)


def atom(token: Token) -> Atom:
    if token.token_type == TokenType.SYMBOL:
        return Symbol(token.literal)
    return token.literal  # return number literal


def evaluate(x: Exp, env: dict = global_env) -> Exp:
    "Evaluate an expression in an environment"

    if isinstance(x, Symbol):  # variable reference
        return env.find(x)[x]
    elif not isinstance(x, List):  # constant
        return x

    # Everything else is a list
    op, *args = x
    if op == 'quote':  # Special forms
        return args[0]  # The quote prevents a list from being evaluated
    elif op == 'if':
        (test, conseq, alt) = args
        exp = (conseq if evaluate(test, env) else alt)
        return evaluate(exp, env)
    elif op == 'define':
        (symbol, exp) = args
        env[symbol] = evaluate(exp, env)
    elif op == "set!":
        (symbol, exp) = args
        env.find(symbol)[symbol] = evaluate(exp, env)
    elif op == "lambda":
        (parms, body) = args
        return Procedure(parms, body, env)
    else:  # Procedure call (non-special forms)
        proc = evaluate(op, env)
        vals = [evaluate(arg, env) for arg in args]
        return proc(*vals)


def repl(prompt='lis.py> '):
    "A prompt-read-eval-print loop."
    while True:
        scanner = Scanner(input(prompt))
        val = evaluate(parse(scanner.scanTokens()))
        if val is not None:
            print(schemestr(val))


def schemestr(exp):
    "Convert a Python object back into a Scheme-readable string."
    if isinstance(exp, List):
        return '(' + ' '.join(map(schemestr, exp)) + ')'
    else:
        return str(exp)


program = """
            (begin 
                (define circle-area (lambda (r) (* pi (* r r))))
                (circle-area (+ 5 5))
            )
        """

# scanner = Scanner(program)
# tokens = scanner.scanTokens()
# print(tokens)
# print(parse(tokens))
# print(evaluate(parse(tokens)))
repl()