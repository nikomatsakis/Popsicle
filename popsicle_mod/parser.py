from . import ast
import re

class Token(object):
    def __init__(self, kind, pos, u_text):
        self.kind = kind
        self.pos = pos
        self.u_text = u_text
        
    def __repr__(self):
        return "%s(%r)" % (self.kind, self.u_text)
        
class Expected(Exception):
    def __init__(self, pos, found, texts_u):
        self.pos = pos
        self.found = found
        self.texts_u = texts_u
    
    def __str__(self):
        return "%s:%s:%s: Found %r, expected one of %r" % (
            self.pos.filename, self.pos.line, self.pos.column, self.found, self.texts_u)
        
def tokenize(pos, span, u_text):
    (column, column_max) = span
    
    def extract_while(kind, start, pred):
        end = start
        while end < column_max and pred(u_text[end]):
            end += 1
        return (end, Token(kind, pos.with_column(start), u_text[start:end]))
        
    def extract_wrapped(kind, start, u_term):
        (end, token) = extract_while(kind, start + 1, lambda u: u != u_term)
        if end == len(u_term): 
            raise Expected(token.pos, token, [u_term])
        return (end + 1, token)
        
    def is_word(u_char):
        return u_char.isalpha() or u_char.isdigit()
        
    while column < column_max:
        u_char = u_text[column]
        
        if u_char == u'\\' and column + 1 < column_max:
            u_next_char = u_text[column + 1]
            yield Token('OP', pos.with_column(column+1), u_next_char)
            column += 2
        
        elif is_word(u_char):
            (column, token) = extract_while('WORD', column, is_word)
            yield token
            
        elif u_char == u'{':
            yield Token('LCURLY', pos.with_column(column), u_char)
            column += 1
            
        elif u_char == u'}':
            yield Token('RCURLY', pos.with_column(column), u_char)
            column += 1
            
        elif u_char == u"_":
            yield Token('UNDER', pos.with_column(column), u_char)
            column += 1
            
        elif u_char == u'"':
            (column, token) = extract_wrapped('QUOTED', column, u'"')
            yield token
            
        elif u_char.isspace():
            (column, token) = extract_while('SPACE', column, lambda u: u.isspace())
            yield token
            
        elif u_char == u'$':
            (column, token) = extract_wrapped('LATEX', column, u'$')
            yield token
    
        elif u_char == u'`':
            (column, token) = extract_wrapped('MACRO', column, u'`')
            yield token
    
        else: 
            # Everything else is an OPerator.  Accumulate multiple instances of same char
            # so that "..." comes out as a single OP, for example.
            (column, token) = extract_while('OP', column, lambda u: u == u_char)
            yield token
            
    yield Token('EOL', pos.with_column(column), u"")
    
ast_dict = {
    'WORD': ast.Identifier,
    'QUOTED': ast.Quoted,
    'LATEX': ast.Latex,
    'OP': ast.Operator,
    'SPACE': ast.Whitespace
}

class ItemParser(object):
    def __init__(self, tokens):
        self.tokens = tokens
        self.token = tokens.next()
        
    def next(self):
        self.token = self.tokens.next()
        
    def till_eol(self):
        nodes = []
        while self.token.kind != 'EOL':
            nodes.append(self.item())
        return nodes
            
    def item(self):
        over = self.part()
        
        if self.token.kind == 'UNDER':
            self.next()
            under = self.part()
            node = ast.Subscript(self.token.pos, over, under)
            return node
            
        return over
        
    def part(self):
        if self.token.kind == 'LCURLY':
            node = ast.Seq(self.token.pos, [])
            self.next()
            while self.token.kind != 'RCURLY':
                if self.token.kind == 'EOL':
                    raise Expected(self.token.pos, self.token, [u'}'])
                node.items.append(self.item())
            self.next()
            return node
        
        if self.token.kind not in ast_dict:
            raise Expected(self.token.pos, self.token, ast_dict.keys())
        
        ast_cls = ast_dict[self.token.kind]
        node = ast_cls(self.token.pos, self.token.u_text)
        self.next()
        return node
        
re_empty = re.compile(ur"\s*$")
re_comment = re.compile(ur"#.*$")
re_section = re.compile(ur"\s*\[(.*)\]\s*$")
re_terminals = re.compile(ur"> Terminals (.*)")
re_heading = re.compile(ur"> Heading (.*)")
re_write = re.compile(ur'> Write (grammar|type rules|dump) from "([^"]*)" to "([^"]*)"$')
re_nonterm = re.compile(ur"([\w-]+)\s*=\s*(.*?)\s*(?:\\\\\\\\(.*))?$")
re_nonterm_cont = re.compile(ur"\s*=\s*(.*?)\s*(?:\\\\\\\\(.*))?$")
re_typerule = re.compile(ur"([\w-]+):\s*$")
re_typerule_cont = re.compile(ur"\s+(.*?)\s*(?:\\\\\\\\(.*))?$")
re_sep = re.compile(ur"\s*---+\s*$")
re_blank = re.compile(ur"\s*$")

class LineParser(object):
    
    def __init__(self, filename, lines_u):
        self.filename = filename
        self.iterator = enumerate(lines_u)
        
    def next_line(self):
        try:
            if self.iterator:
                (line, self.u_text) = self.iterator.next()
                self.pos = ast.Position(self.filename, line + 1, 1)
        except StopIteration:
            self.iterator = None
            self.u_text = u""
            self.pos = None
            
    def has_line(self):
        return self.pos is not None
        
    def parse_seq(self, span):
        tokens = tokenize(self.pos, span, self.u_text)
        node = ast.Seq(self.pos, ItemParser(tokens).till_eol())
        return node
        
    def parse_line_mo(self, mo, baseidx):
        rhs_span = mo.span(baseidx)
        opt_label = mo.group(baseidx + 1)
        u_label = opt_label.strip() if opt_label is not None else None
        return ast.Line(self.pos, self.parse_seq(rhs_span), u_label)
        
    def parse_nonterm(self, mo):
        u_name = mo.group(1).strip()
        node = ast.NonterminalDecl(self.pos, u_name)
        node.expansions.append(self.parse_line_mo(mo, 2))
        self.next_line()
        
        # Load additional RHS:
        while self.has_line():
            mo = re_nonterm_cont.match(self.u_text)
            if not mo:
                break
            node.expansions.append(self.parse_line_mo(mo, 1))
            self.next_line()
            
        return node
        
    def parse_type_rule(self, mo):
        u_name = mo.group(1).strip()
        node = ast.TypeRule(self.pos, u_name)
        self.next_line()
        
        # Load the premises:
        while self.has_line():
            if re_sep.match(self.u_text) or re_blank.match(self.u_text):
                self.next_line()
                break
            mo = re_typerule_cont.match(self.u_text)
            if not mo:
                break
            node.premises.append(self.parse_line_mo(mo, 1))
            self.next_line()
            
        # Load the conclusion:
        while self.has_line():
            if re_blank.match(self.u_text):
                break
            mo = re_typerule_cont.match(self.u_text)
            if not mo:
                break
            node.conclusions.append(self.parse_line_mo(mo, 1))
            self.next_line()
        
        return node
        
    def parse(self):
        ast_file = ast.Section(ast.Position(self.filename, 1, 1), u'Root')
        ast_section = ast_file
    
        self.next_line()
        while self.has_line():
            if re_empty.match(self.u_text): 
                self.next_line()
                continue
            if re_comment.match(self.u_text): 
                self.next_line()
                continue
    
            mo = re_section.match(self.u_text)
            if mo: 
                u_name = mo.group(1).strip()
                ast_section = ast.Section(self.pos, u_name)
                ast_file.add_member(ast_section)
                self.next_line()
                continue
        
            mo = re_terminals.match(self.u_text)
            if mo:
                names_u = mo.group(1).split()
                node = ast.TerminalDecl(self.pos, names_u)
                ast_section.add_member(node)
                self.next_line()
                continue
                
            mo = re_heading.match(self.u_text)
            if mo:
                u_latex = mo.group(1).strip()
                latex = u_latex.encode("ASCII")
                node = ast.Heading(self.pos, latex)
                ast_section.add_member(node)
                self.next_line()
                continue
                
            mo = re_write.match(self.u_text)
            if mo:
                kind = mo.group(1).strip().encode("ASCII")
                u_section = mo.group(2).strip()
                u_filenm = mo.group(3).strip()
                node = ast.Write(self.pos, kind, u_section, u_filenm)
                ast_section.add_member(node)
                self.next_line()
                continue

            mo = re_nonterm.match(self.u_text)
            if mo:
                node = self.parse_nonterm(mo)
                ast_section.add_member(node)
                continue
                
            mo = re_typerule.match(self.u_text)
            if mo:
                node = self.parse_type_rule(mo)
                ast_section.add_member(node)                    
                continue
            
            raise Expected(self.pos, self.u_text, ['Valid Declaration'])
        return ast_file

def parse_file(filename, encoding):
    lines = open(filename).readlines()
    lines_u = [s.decode(encoding) for s in lines]
    return LineParser(filename, lines_u).parse()