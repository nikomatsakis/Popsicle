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
            (column, token) = extract_while('IDENT', column, is_word)
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
            
        elif u_char == u"^":
            yield Token('OVER', pos.with_column(column), u_char)
            column += 1
            
        elif u_char == u"'":
            yield Token('PRIME', pos.with_column(column), u_char)
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
            (column, token) = extract_wrapped('IDENT', column, u'`')
            yield token
    
        else: 
            # Everything else is an OPerator.  Accumulate multiple instances of same char
            # so that "..." comes out as a single OP, for example.
            (column, token) = extract_while('OP', column, lambda u: u == u_char)
            yield token
            
    yield Token('EOL', pos.with_column(column), u"")
    
ast_dict = {
    'IDENT': ast.Identifier,
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
        node = self.part()
        while self.token.kind in ['UNDER', 'OVER', 'PRIME']:
            pos = self.token.pos
            if self.token.kind == 'UNDER':
                self.next()
                under = self.part()
                node = ast.Subscript(pos, node, under)
            elif self.token.kind == 'PRIME':
                self.next()
                node = ast.Prime(pos, node)
            elif self.token.kind == 'OVER':
                self.next()
                over = self.part()
                node = ast.Supscript(pos, node, over)
        return node
        
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
    
# This little class is useful for debugging reg exp performance:
class ReWrap(object):
    def __init__(self, text, re):
        self.text = text
        self.re = re
    
    def match(self, text):
        print "match: %s against %r" % (self.text, text)
        mo = self.re.match(text)
        print "  done"
        return mo
    
def recompile(text):
    #return ReWrap(text, re.compile(text))
    return re.compile(text)

re_skip = recompile(ur"^\s*(?:#.*)?$")
re_section = recompile(ur"\s*(\[+)(.*?)(\]+)\s*$")
re_terminals = recompile(ur"> Terminals (.*)")
re_quoted = recompile(ur"> Quoted (.*)")
re_insert = recompile(ur"> Insert (Start|Middle|End) (.*)")
re_macro = recompile(ur"> Macro ([^ ]*) (.*)")
re_link = recompile(ur"> Link ([a-zA-Z]+) (.*)")
re_write_sec = recompile(ur'> Write (grammar|type rules|dump) from "([^"]*)" to "([^"]*)"$')
re_write_all = recompile(ur'> Write all\s*$')
re_nonterm = recompile(ur"([\w-]+(?:\s+[\w-]+)*)\s*=\s*(.*?)\s*(?:\\\\\\\\(.*))?$")
re_nonterm_cont = recompile(ur"\s*=\s*(.*?)\s*(?:\\\\\\\\(.*))?$")
re_typerule = recompile(ur"([\w-]+):\s*$")
re_typerule_cont = recompile(ur"\s+(.*?)\s*(?:\\\\\\\\(.*))?$")
re_sep = recompile(ur"\s*---+\s*$")
re_subst = recompile(ur"([\w-]+): substitute into ([a-zA-Z]+) ([\w-]+)\s*$")
re_subst_const = recompile(ur"\s+([^\s]*)\s*=>\s*(.*)$")

class LineParser(object):
    
    def __init__(self, filename, lines_u):
        self.filename = filename
        self.iterator = enumerate(lines_u)
        
    def next_line(self):
        try:
            while self.iterator:
                (line, self.u_text) = self.iterator.next()
                self.pos = ast.Position(self.filename, line + 1, 1)
                if not re_skip.match(self.u_text):
                    return
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
        names_u = [u.strip() for u in mo.group(1).split()]
        node = ast.NonterminalDecl(self.pos, names_u)
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
        separated = False
        while self.has_line():
            if re_sep.match(self.u_text):
                separated = True
                self.next_line()
                break
            mo = re_typerule_cont.match(self.u_text)
            if not mo:
                break
            node.premises.append(self.parse_line_mo(mo, 1))
            self.next_line()
            
        if not separated:
            raise Expected(self.pos, self.u_text, ['Type Rule Separator (----)'])
            
        # Load the conclusion:
        while self.has_line():
            mo = re_typerule_cont.match(self.u_text)
            if not mo:
                break
            node.conclusions.append(self.parse_line_mo(mo, 1))
            self.next_line()
        
        return node
        
    def parse_subst(self, mo):
        u_name = mo.group(1)
        kind = mo.group(2).encode('ASCII')
        u_orig_name = mo.group(3)
        snode = ast.Substitution(self.pos, u_name, kind, u_orig_name)
        self.next_line()
        
        # Load substitutions:
        while self.has_line():
            mo = re_subst_const.match(self.u_text)
            if not mo: 
                break
                
            from_seq = self.parse_seq(mo.span(1))
            to_seq = self.parse_seq(mo.span(2))
            mnode = ast.Mapping(self.pos, from_seq, to_seq)
            snode.substitutions.append(mnode)
            self.next_line()
        
        return snode
        
    def parse(self):
        ast_file = ast.Section(ast.Position(self.filename, 1, 1), u'Root')
        ast_sections = [ast_file]
    
        self.next_line()
        while self.has_line():
            mo = re_section.match(self.u_text)
            if mo: 
                u_start = mo.group(1)
                u_name = mo.group(2).strip()
                u_term = mo.group(3)
                
                if len(u_start) != len(u_term):
                    raise Exception(self.pos, self.u_text, ['Balanced brackets'])
                    
                depth = len(u_start)
                assert depth > 0
                if len(ast_sections) < depth:
                    raise Exception(self.pos, self.u_term, ['Depth < %d' % len(ast_sections)])
                ast_sections[depth:] = [ast.Section(self.pos, u_name)]
                ast_sections[-2].add_member(ast_sections[-1])
                self.next_line()
                continue
        
            mo = re_terminals.match(self.u_text)
            if mo:
                names_u = mo.group(1).split()
                node = ast.TerminalDecl(self.pos, names_u)
                ast_sections[-1].add_member(node)
                self.next_line()
                continue
                
            mo = re_quoted.match(self.u_text)
            if mo:
                names_u = mo.group(1).split()
                node = ast.QuotedDecl(self.pos, names_u)
                ast_sections[-1].add_member(node)
                self.next_line()
                continue
                
            mo = re_macro.match(self.u_text)
            if mo:
                u_replace = mo.group(1)
                latex = mo.group(2).strip().encode("ASCII")
                node = ast.MacroDecl(self.pos, u_replace, latex)
                ast_sections[-1].add_member(node)
                self.next_line()
                continue
                
            mo = re_link.match(self.u_text)
            if mo:
                kind = mo.group(1).encode("ASCII")
                u_name = mo.group(2).strip()
                node = ast.Link(self.pos, kind, u_name)
                ast_sections[-1].add_member(node)
                self.next_line()
                continue
                
            mo = re_insert.match(self.u_text)
            if mo:
                kind = mo.group(1).encode("ASCII")
                latex = mo.group(2).strip().encode("ASCII")
                node = ast.Insert(self.pos, kind, latex)
                ast_sections[-1].add_member(node)
                self.next_line()
                continue
                
            mo = re_write_sec.match(self.u_text)
            if mo:
                kind = mo.group(1).strip().encode("ASCII")
                u_section = mo.group(2).strip()
                u_filenm = mo.group(3).strip()
                node = ast.WriteSection(self.pos, kind, u_section, u_filenm)
                ast_sections[-1].add_member(node)
                self.next_line()
                continue

            mo = re_write_all.match(self.u_text)
            if mo:
                node = ast.WriteAll(self.pos)
                ast_sections[-1].add_member(node)
                self.next_line()
                continue

            mo = re_nonterm.match(self.u_text)
            if mo:
                node = self.parse_nonterm(mo)
                ast_sections[-1].add_member(node)
                continue
                
            mo = re_typerule.match(self.u_text)
            if mo:
                node = self.parse_type_rule(mo)
                ast_sections[-1].add_member(node)                    
                continue
                
            mo = re_subst.match(self.u_text)
            if mo:
                node = self.parse_subst(mo)
                ast_sections[-1].add_member(node)
                continue
            
            raise Expected(self.pos, self.u_text, ['Valid Declaration'])
        return ast_file

def parse_file(filename, encoding):
    lines = open(filename).readlines()
    lines_u = [s.decode(encoding) for s in lines]
    return LineParser(filename, lines_u).parse()