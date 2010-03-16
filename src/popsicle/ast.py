import re

class Position(object):
    def __init__(self, filename, line, column):
        self.filename = filename
        self.line = line
        self.column = column
        
    def with_column(self, column):
        return Position(self.filename, self.line, column)        
        
    def with_line(self, line):
        return Position(self.filename, line, self.column)        
        
    def clone(self):
        return Position(self.filename, self.line, self.column)
    
class Ast(object):
    def __init__(self, pos):
        self.pos = pos
        
    def serialize(self, out):
        self.serialize_indent(out, 0, "Root: ")
        
    def serialize_indent(self, out, indent, label):
        doc = self.__doc__
        out.write(" " * indent)
        out.write(label)
        out.write(self.__class__.__name__)
        out.write(" ")
        out.write(doc)
        out.write("\n")
        fields = re.findall(r"\[([^]]*)\]", self.__doc__)
        for field in fields:
            value = getattr(self, field)
            if not isinstance(value, list) and not isinstance(value, tuple):
                value = (value,)
            for item in value:
                if isinstance(item, Ast):
                    item.serialize_indent(out, indent + 2, field + ": ")
                else:
                    out.write(" " * (indent + 2))
                    out.write(field)
                    out.write(": ")
                    out.write(repr(item))
                    out.write("\n")
                    
class File(Ast):
    "File: [members]"
    def __init__(self, *args):
        Ast.__init__(self, *args)
        self.members = []
    
class WriteTex(Ast):
    "> WriteTex [u_section] [u_filename]"
    def __init__(self, pos, u_section, u_filename):
        Ast.__init__(self, pos)
        self.u_section = u_section
        self.u_filename = u_filename
    
class NamedAst(Ast):
    "Abstract base class for named things."
    def __init__(self, pos, u_name):
        Ast.__init__(self, pos)
        self.u_name = u_name
        
class Section(NamedAst):
    "[u_name]: [members]"
    def __init__(self, *args):
        NamedAst.__init__(self, *args)
        self.members = []
    
class TerminalDecl(Ast):
    "> Terminals [names_u]"
    def __init__(self, pos, names_u):
        Ast.__init__(self, pos)
        self.names_u = names_u
        
class NonterminalDecl(NamedAst):
    "[u_name] = [expansions]"
    def __init__(self, *args):
        NamedAst.__init__(self, *args)
        self.expansions = []
        
class TypeRule(NamedAst):
    "[u_name]: [premises] --- [conclusions]"
    def __init__(self, *args):
        NamedAst.__init__(self, *args)
        self.premises = []
        self.conclusions = []
    
class Subscript(Ast):
    "[base]_[sub]"
    def __init__(self, pos, base, sub):
        Ast.__init__(self, pos)
        self.base = base
        self.sub = sub

class Seq(Ast):
    "{[items]}"
    def __init__(self, pos, items):
        Ast.__init__(self, pos)
        self.items = items

class TextAst(Ast):
    "Abstract base class for formatting of u_text"
    def __init__(self, pos, u_text):
        Ast.__init__(self, pos)
        self.u_text = u_text
        
class Identifier(TextAst):
    "Identifier: [u_text]"
        
class Quoted(TextAst):
    "\"[u_text]\""
    
class Latex(TextAst):
    "$[u_text]$"
    
class Operator(TextAst):
    "Operator: [u_text]"
    
class Whitespace(TextAst):
    "Whitespace: [u_text]"


