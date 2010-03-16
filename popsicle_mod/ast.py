# -*- coding: utf-8 -*-

import re, sys

reserved_chars = u"|{}\\\"_'^`"
math_translations = {
    u'≠': r'\neq',
    u'≤': r'\leq',
    u'≥': r'\geq',
    u'∪': r'\cup',
    u'∩': r'\cap',
    u'∀': r'\forall',
    u'∃': r'\exists',
    u'∄': r'\nexists',
    u'∅': r'\emptyset',
    u'⊦': r'\vdash',
    u'⊧': r'\models',
    u'⊑': r'\sqsubseteq',
    u'⊏': r'\sqsubset',
    u'⊆': r'\subseteq',
    u'⊂': r'\subset',
    u'⊒': r'\sqsupseteq',
    u'⊐': r'\sqsupset',
    u'⊇': r'\supseteq',
    u'⊃': r'\supset',
    u'∈': r'\in',
    u'∉': r'\notin',
    u'→': r'\rightarrow',
    u'←': r'\leftarrow',
    u'⇒': r'\Rightarrow',
    u'⇐': r'\Lightarrow',
    u'⊕': r'\oplus',
    u'⊖': r'\ominus',
    u'⊥': r'\bot',
    u'⊤': r'\top',
    u'×': r'\times',
    u'\\': r'\setminus',
    u'|': r'\mid',
    u'...': r'\ldots',    
    u'α': r'\alpha',
    u'β': r'\beta',
    u'γ': r'\gamma',
    u'δ': r'\delta',
    u'ε': r'\epsilon',
    u'ζ': r'\zeta',
    u'η': r'\eta',
    u'θ': r'\theta',
    u'ι': r'\iota',
    u'κ': r'\kappa',
    u'λ': r'\lambda',
    u'μ': r'\mu',
    u'ν': r'\nu',
    u'ο': r'\omicron',
    u'π': r'\pi',
    u'ρ': r'\ro',
    u'σ': r'\sigma',
    u'τ': r'\tau',
    u'φ': r'\phi',
    u'χ': r'\xi',
    u'ψ': r'\psi',
    u'ω': r'\omega',
    u'Α': r'A',
    u'Β': r'B',
    u'Γ': r'\Gamma',
    u'Δ': r'\Delta',
    u'Ε': r'E',
    u'Ζ': r'Z',
    u'Η': r'H',
    u'Θ': r'\Theta',
    u'Ι': r'I',
    u'Κ': r'K',
    u'Λ': r'\Lambda',
    u'Μ': r'M',
    u'Ν': r'N',
    u'Ο': r'O',
    u'Π': r'\Pi',
    u'Ρ': r'P',
    u'Σ': r'\Sigma',
    u'Τ': r'T',
    u'Φ': r'\Phi',
    u'Χ': r'X',
    u'Ψ': r'\Psi',
    u'Ω': r'\Omega'    
}

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
        
    def __str__(self):
        return "%s:%s:%s" % (self.filename, self.line, self.column)
        
class Context(object):
    
    def __init__(self, root):
        self.root = root
        self.terminals = []
        self.nonterminals = []
        
    def math_char(self, u):
        if u in math_translations:
            return math_translations[u]
        return u.encode("ASCII")
        
    def math(self, u_text):
        "Convert to latex suitable for inclusion in a math context"
        return "".join([self.math_char(u) for u in u_text])
        
    def plain_char(self, u):
        if u in math_translations:
            return "$" + math_translations[u] + "$"
        return u.encode("ASCII")
        
    def plain(self, u_text):
        "Convert to latex suitable for inclusion in a non-math context"
        return "".join([self.plain_char(u) for u in u_text])
    
class Ast(object):
    def __init__(self, pos):
        self.pos = pos
        
    def execute(self):
        ctx = Context(self)
        self.append_context(ctx)
        self.execute_directives(ctx)
        return ctx
        
    def execute_directives(self, ctx):
        "Executes any executable directives"
        pass
        
    def append_context(self, ctx):
        "Adjusts the context object to account for things declared in this subtree"
        pass
        
    def find_section(self, u_name):
        "Returns the AST node for the named section"
        return None
        
    def write_grammar_row(self, out, ctx):
        "Writes a nonterminal declaration into the grammar table, if appl."
        return
        
    def write_type_rule(self, out, ctx):
        "Writes a type rule into the grammar table, if appl."
        return
        
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
                    
class NamedAst(Ast):
    r"Abstract base class for named things."
    def __init__(self, pos, u_name):
        Ast.__init__(self, pos)
        self.u_name = u_name
    
class Section(NamedAst):
    r"[u_name]: [members]"
    def __init__(self, *args):
        NamedAst.__init__(self, *args)
        self.members = []
    
    def find_section(self, u_name):
        "Returns the AST node for the named section"
        if self.u_name == u_name:
            return self
        for mem in self.members:
            sec = mem.find_section(u_name)
            if sec: 
                return sec
        return None
        
    def add_member(self, mem):
        assert mem
        self.members.append(mem)
        
    def execute_directives(self, ctx):
        for mem in self.members:
            mem.execute_directives(ctx)
        
    def append_context(self, ctx):
        for mem in self.members: 
            mem.append_context(ctx)
        
    def write_grammar(self, out, ctx):
        out.write("\\begin{tabular}{llll}\n")
        self.write_grammar_row(out, ctx)
        out.write("\\end{tabular}\n")
        
    def write_grammar_row(self, out, ctx):
        for mem in self.members:
            mem.write_grammar_row(out, ctx)
        
    def write_type_rules(self, out, ctx):
        out.write("\\begin{mathpar}\n")
        self.write_type_rule(out, ctx)
        out.write("\\end{mathpar}\n")
        
    def write_type_rule(self, out, ctx):
        for mem in self.members:
            mem.write_type_rule(out, ctx)
        
class Write(Ast):
    r'> Write [kind] from "[u_section]" to "[u_filename]"'
    def __init__(self, pos, kind, u_section, u_filename):
        Ast.__init__(self, pos)
        self.kind = kind
        self.u_section = u_section
        self.u_filename = u_filename
    
    def execute_directives(self, ctx):
        sec = ctx.root.find_section(self.u_section)
        if not sec:
            sys.stderr.write("%s: No section named %r!\n" % (self.pos, self.u_section))
        else:
            filename = self.u_filename.encode("UTF-8")
            out = open(filename, 'w')
            if self.kind == 'grammar':
                sec.write_grammar(out, ctx)
            elif self.kind == 'type rules':
                sec.write_type_rules(out, ctx)
            elif self.kind == 'dump':
                sec.serialize(out)
            out.close()
        
class TerminalDecl(Ast):
    r"> Terminals [names_u]"
    def __init__(self, pos, names_u):
        Ast.__init__(self, pos)
        self.names_u = names_u
        
    def append_context(self, ctx):
        ctx.terminals.extend(self.names_u)
        
class NonterminalDecl(NamedAst):
    r"[u_name] = [expansions] \\\\ [label]"
    def __init__(self, *args):
        NamedAst.__init__(self, *args)
        self.expansions = []
        
    def append_context(self, ctx):
        ctx.terminals.append(self.u_name)
    
    def write_grammar_row(self, out, ctx):
        out.write("%s & :=" % ctx.plain(self.u_name))
        
        sep = " & "
        conc = " & \\\\"
        for expansion in self.expansions:
            out.write("\n    ")
            out.write(sep)
            out.write("$")
            expansion.seq.write_math_latex(out, ctx)
            out.write("$")
            
            if expansion.u_label is not None:
                out.write(" & %s \\\\" % ctx.plain(self.u_label))
                sep = " & $\\mid$ & "
                conc = ""
            else:
                sep = " $\\mid$ "
                conc = " & \\\\"
            
        out.write(conc)
        out.write("\n")

class TypeRule(NamedAst):
    r"[u_name]: [premises] --- [conclusions]"
    def __init__(self, *args):
        NamedAst.__init__(self, *args)
        self.premises = []
        self.conclusions = []
        
    def write_part(self, out, ctx, lines):
        if not lines:
            out.write("{}\n")
        else:
            out.write("{")
            for line in lines:
                line.seq.write_math_latex(out, ctx)
                out.write(r" \\")
                if line.u_label:
                    out.write(r"\\")
                out.write("\n")
            out.write("}\n")
        
    def write_type_rule(self, out, ctx):
        out.write("\\inferrule[%s]" % ctx.plain(self.u_name))
        self.write_part(out, ctx, self.premises)
        self.write_part(out, ctx, self.conclusions)
    
class Line(Ast):
    r"[seq] \\\\ [u_label]"
    def __init__(self, pos, seq, u_label):
        Ast.__init__(self, pos)
        self.seq = seq
        # Note: if u_label is None, there was no \\\\.
        self.u_label = u_label
        
class Subscript(Ast):
    r"[base]_{[sub]}"
    def __init__(self, pos, base, sub):
        Ast.__init__(self, pos)
        self.base = base
        self.sub = sub
    
    def write_math_latex(self, out, ctx):
        self.base.write_math_latex(out, ctx)
        out.write("_{")
        self.sub.write_math_latex(out, ctx)
        out.write("}")

class Seq(Ast):
    r"{[items]}"
    def __init__(self, pos, items):
        Ast.__init__(self, pos)
        self.items = items
        
    def write_math_latex(self, out, ctx):
        for item in self.items:
            item.write_math_latex(out, ctx)

class TextAst(Ast):
    r"Abstract base class for formatting of u_text"
    def __init__(self, pos, u_text):
        Ast.__init__(self, pos)
        self.u_text = u_text
        
class Identifier(TextAst):
    r"Identifier: [u_text]"
    
    def write_math_latex(self, out, ctx):
        out.write(ctx.math(self.u_text))
        
class Quoted(TextAst):
    r'"[u_text]"'
    
    def write_math_latex(self, out, ctx):
        out.write("\mathtt{")
        out.write(self.u_text.encode("ASCII"))
        out.write("}")
        
class Latex(TextAst):
    r"$[u_text]$"

    def write_math_latex(self, out, ctx):
        out.write(u_text.encode("ASCII"))
            
class Operator(TextAst):
    r"Operator: [u_text]"
    
    def write_math_latex(self, out, ctx):
        out.write(ctx.math(self.u_text))
    
class Whitespace(TextAst):
    r"Whitespace: [u_text]"

    def write_math_latex(self, out, ctx):
        out.write(" ")