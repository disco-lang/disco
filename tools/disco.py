# -*- coding: utf-8 -*-
"""
    Pygments lexer for disco.
"""

import re

from pygments.lexer import Lexer, RegexLexer, bygroups, default, include
from pygments.token import Text, Comment, Operator, Keyword, Name, String, \
    Number, Punctuation, Generic
from pygments import unistring as uni

__all__ = ['DiscoLexer']

class DiscoLexer(RegexLexer):
    """
    A Disco lexer.
    """
    name = 'Disco'
    aliases = ['disco']
    filenames = ['*.disco']

    flags = re.MULTILINE | re.UNICODE

    reserved = ('let', 'in', 'is', 'if', 'when', 'otherwise', 'forall', 'type', 'import')
    ascii = ('NUL', 'SOH', '[SE]TX', 'EOT', 'ENQ', 'ACK',
             'BEL', 'BS', 'HT', 'LF', 'VT', 'FF', 'CR', 'S[OI]', 'DLE',
             'DC[1-4]', 'NAK', 'SYN', 'ETB', 'CAN',
             'EM', 'SUB', 'ESC', '[FGRU]S', 'SP', 'DEL')
    builtin = ('and', 'or', 'not', 'mod', 'choose', 'sqrt', 'lg', 'implies', 'size',
               'union', 'intersect', 'enumerate', 'count', 'floor', 'ceiling', 'divides')

    tokens = {
        'root': [
            # Whitespace:
            (r'\s+', Text),
            (r'\|\|\|.*?$', Comment.Doc),
            (r'--(?![!#$%&*+./<=>?@^|_~:\\]).*?$', Comment.Single),
            # Lexemes:
            #  Identifiers
            (r'\bimport\b', Keyword.Reserved, 'import'),
            (r'\b(%s)(?!\')\b' % '|'.join(reserved), Keyword.Reserved),
            (r"'[^\\]'", String.Char),
            (r'^[_' + uni.Ll + r'][\w\']*', Name.Function),
            (r"'?[_" + uni.Ll + r"][\w']*", Name),
            (r"('')?[" + uni.Lu + r"][\w\']*", Keyword.Type),
            #  Operators
            (r'\\(?![:!#$%&*+.\\/<=>?@^|~-]+)', Name.Function),  # lambda operator
            (r'(<-|:|->|=)(?![:!#$%&*+.\\/<=>?@^|~-]+)', Operator.Word),  # specials
            (r'[:!#$%&*+.\\/<=>?@^|~-]+', Operator),  # Other operators
            #  Numbers
            (r'\d+[eE][+-]?\d+', Number.Float),
            (r'\d+\.\d+([eE][+-]?\d+)?', Number.Float),
            (r'0[oO][0-7]+', Number.Oct),
            (r'0[xX][\da-fA-F]+', Number.Hex),
            (r'\d+', Number.Integer),
            #  Character/String Literals
            (r"'", String.Char, 'character'),
            (r'"', String, 'string'),
            #  Special
            (r'\[\]', Keyword.Type),
            (r'\(\)', Name.Builtin),
            (r'[][(),;`{}]', Punctuation),
        ],
        'import': [
            (r'\s+', Text),
            (r'[\w.\/]+', Name.Namespace, '#pop'),
        ],
        'character': [
            # Allows multi-chars, incorrectly.
            (r"[^\\']'", String.Char, '#pop'),
            (r"\\", String.Escape, 'escape'),
            ("'", String.Char, '#pop'),
        ],
        'string': [
            (r'[^\\"]+', String),
            (r"\\", String.Escape, 'escape'),
            ('"', String, '#pop'),
        ],
        'escape': [
            (r'[abfnrtv"\'&\\]', String.Escape, '#pop'),
            (r'\^[][' + uni.Lu + r'@^_]', String.Escape, '#pop'),
            ('|'.join(ascii), String.Escape, '#pop'),
            (r'o[0-7]+', String.Escape, '#pop'),
            (r'x[\da-fA-F]+', String.Escape, '#pop'),
            (r'\d+', String.Escape, '#pop'),
            (r'\s+\\', String.Escape, '#pop'),
        ],
}
