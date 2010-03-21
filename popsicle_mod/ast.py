# -*- coding: utf-8 -*-

import re, sys

reserved_chars = u"|{}\\\"_'^`"
math_translations = {
    u'≠': '\\neq ',
    u'≤': '\\leq ',
    u'≥': '\\geq ',
    u'∪': '\\cup ',
    u'∩': '\\cap ',
    u'∀': '\\forall ',
    u'∃': '\\exists ',
    u'∄': '\\nexists ',
    u'∅': '\\emptyset ',
    u'⊦': '\\vdash ',
    u'⊧': '\\models ',
    u'⊑': '\\sqsubseteq ',
    u'⊏': '\\sqsubset ',
    u'⊆': '\\subseteq ',
    u'⊂': '\\subset ',
    u'⊒': '\\sqsupseteq ',
    u'⊐': '\\sqsupset ',
    u'⊇': '\\supseteq ',
    u'⊃': '\\supset ',
    u'∈': '\\in ',
    u'∉': '\\notin ',
    u'→': '\\rightarrow ',
    u'←': '\\leftarrow ',
    u'⇒': '\\Rightarrow ',
    u'⇐': '\\Lightarrow ',
    u'⊕': '\\oplus ',
    u'⊖': '\\ominus ',
    u'⊥': '\\bot ',
    u'⊤': '\\top ',
    u'×': '\\times ',
    u'\\': '\\setminus ',
    u'|': '\\mid ',
    u'α': '\\alpha ',
    u'β': '\\beta ',
    u'γ': '\\gamma ',
    u'δ': '\\delta ',
    u'ε': '\\epsilon ',
    u'ζ': '\\zeta ',
    u'η': '\\eta ',
    u'θ': '\\theta ',
    u'ι': '\\iota ',
    u'κ': '\\kappa ',
    u'λ': '\\lambda ',
    u'μ': '\\mu ',
    u'ν': '\\nu ',
    u'ο': '\\omicron ',
    u'π': '\\pi ',
    u'ρ': '\\ro ',
    u'σ': '\\sigma ',
    u'τ': '\\tau ',
    u'φ': '\\phi ',
    u'χ': '\\xi ',
    u'ψ': '\\psi ',
    u'ω': '\\omega ',
    u'Α': r'A',
    u'Β': r'B',
    u'Γ': '\\Gamma ',
    u'Δ': '\\Delta ',
    u'Ε': r'E',
    u'Ζ': r'Z',
    u'Η': r'H',
    u'Θ': '\\Theta ',
    u'Ι': r'I',
    u'Κ': r'K',
    u'Λ': '\\Lambda ',
    u'Μ': r'M',
    u'Ν': r'N',
    u'Ο': r'O',
    u'Π': '\\Pi ',
    u'Ρ': r'P',
    u'Σ': '\\Sigma ',
    u'Τ': r'T',
    u'Φ': '\\Phi ',
    u'Χ': r'X',
    u'Ψ': '\\Psi ',
    u'Ω': '\\Omega ',
}

u_escape = u"{}_"

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
        self.macros = {}
        self.sep = ""
        
    def math_char(self, u):
        if u in math_translations:
            return math_translations[u]
        if u in u_escape:
            return "\\" + u.encode("ASCII")
        return u.encode("ASCII")
        
    def multi(self, res):
        return res.replace("...", "\ldots ")
        
    def math(self, u_text):
        "Convert to latex suitable for inclusion in a math context"
        return self.multi("".join([self.math_char(u) for u in u_text]))
        
    def plain_char(self, u):
        if u in math_translations:
            return "$" + math_translations[u] + "$"
        if u in u_escape:
            return "\\" + u.encode("ASCII")
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
        
    @property
    def writes_grammar_row(self):
        return hasattr(self, 'write_grammar_row')
        
    @property
    def writes_insert(self):
        return hasattr(self, 'write_insert')
        
    @property
    def writes_type_rule(self):
        return hasattr(self, 'write_type_rule')
        
    def execute_directives(self, ctx):
        "Executes any executable directives"
        pass
        
    def append_context(self, ctx):
        "Adjusts the context object to account for things declared in this subtree"
        pass
        
    def find_named_node(self, kind, u_name):
        "Returns the AST node of the given kind and name"
        return None
        
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
        
    def find_named_node(self, kind, u_name):
        if kind == self.__class__.__name__ and u_name == self.u_name:
            return self
        return None
    
class Section(NamedAst):
    r"[u_name]: [members]"
    
    def __init__(self, *args):
        NamedAst.__init__(self, *args)
        self.members = []
    
    def find_named_node(self, kind, u_name):
        "Returns the AST node for the named section"
        if kind == 'Section' and self.u_name == u_name:
            return self
        for mem in self.members:
            node = mem.find_named_node(kind, u_name)
            if node: return node
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
            
    @property
    def subsections(self):
        return [mem for mem in self.members if isinstance(mem, Section)]
        
    def write_inserts(self, kind, out, ctx):
        for mem in self.members:
            if mem.writes_insert:
                mem.write_insert(kind, out, ctx)
        
    def write_grammar(self, out, ctx):
        self.write_inserts('Start', out, ctx)
        rowmems = [mem for mem in self.members if mem.writes_grammar_row]
        if rowmems:
            out.write("\\begin{tabular}{llll}\n")
            for rowmem in rowmems:
                rowmem.write_grammar_row(out, ctx)
            out.write("\\end{tabular}\n")
        self.write_inserts('Middle', out, ctx)
        for mem in self.subsections: 
            mem.write_grammar(out, ctx)
        self.write_inserts('End', out, ctx)
        
    def write_type_rules(self, out, ctx):
        self.write_inserts('Start', out, ctx)
        rulemems = [mem for mem in self.members if mem.writes_type_rule]
        if rulemems:
            out.write("\\begin{mathpar}\n")
            ctx.sep = ""
            for rulemem in rulemems: 
                rulemem.write_type_rule(out, ctx)
            out.write("\\end{mathpar}\n")
        self.write_inserts('Middle', out, ctx)
        for mem in self.subsections:
            mem.write_type_rules(out, ctx)
        self.write_inserts('End', out, ctx)
        
class Insert(Ast):
    r'> Insert [kind] [latex]'
    
    def __init__(self, pos, kind, latex):
        Ast.__init__(self, pos)
        self.kind = kind
        self.latex = latex
        
    def write_insert(self, kind, out, ctx):
        if kind == self.kind:
            out.write(self.latex)
            out.write("\n")
        
class Write(Ast):
    r'> Write [kind] from "[u_section]" to "[u_filename]"'
    def __init__(self, pos, kind, u_section, u_filename):
        Ast.__init__(self, pos)
        self.kind = kind
        self.u_section = u_section
        self.u_filename = u_filename
    
    def execute_directives(self, ctx):
        sec = ctx.root.find_named_node('Section', self.u_section)
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
        
class MacroDecl(Ast):
    r"> Macro [u_replace] [latex]"
    def __init__(self, pos, u_replace, latex):
        Ast.__init__(self, pos)
        self.u_replace = u_replace
        self.latex = latex
        
    def append_context(self, ctx):
        ctx.macros[self.u_replace] = self.latex
        
class NonterminalDecl(NamedAst):
    r"[u_name] = [expansions]"
    
    def __init__(self, *args):
        NamedAst.__init__(self, *args)
        self.expansions = []
        
    def append_context(self, ctx):
        ctx.nonterminals.append(self.u_name)
    
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
                out.write(" & %s \\\\" % ctx.plain(expansion.u_label))
                sep = " & $\\mid$ & "
                conc = ""
            else:
                sep = " $\\mid$ "
                conc = " & \\\\"
            
        out.write(conc)
        out.write("\n")

class Link(Ast): # Not NamedAst: the Link itself is not named u_name
    r"> Link [kind] [u_name]"
    
    def __init__(self, pos, kind, u_name):
        Ast.__init__(self, pos)
        self.kind = kind
        self.u_name = u_name
    
    def resolve(self, ctx):
        return ctx.root.find_named_node(self.kind, self.u_name)
        
    def write_grammar_row(self, out, ctx):
        return self.resolve(ctx).write_grammar_row(out, ctx)
        
    def write_type_rule(self, out, ctx):
        return self.resolve(ctx).write_type_rule(out, ctx)

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
            out.write("{\n")
            for line in lines:
                line.seq.write_math_latex(out, ctx)
                out.write(r" \\")
                if line.u_label is not None:
                    out.write(r"\\")
                out.write("\n")
            out.write("}")
        
    def write_type_rule(self, out, ctx):
        out.write(ctx.sep)
        out.write("\\inferrule[%s]" % ctx.plain(self.u_name))
        self.write_part(out, ctx, self.premises)
        self.write_part(out, ctx, self.conclusions)
        ctx.sep = "\n\\and\n"
    
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
        if self.u_text in ctx.macros:
            out.write(ctx.macros[self.u_text])
        elif self.u_text in ctx.terminals:
            out.write("\\texttt{%s}" % ctx.plain(self.u_text))
        elif len(self.u_text) == 1:
            out.write(ctx.math(self.u_text))
        else:
            out.write("\\textit{%s}" % ctx.plain(self.u_text))
        
class Quoted(TextAst):
    r'"[u_text]"'
    
    def write_math_latex(self, out, ctx):
        out.write("\\texttt{%s}" % ctx.math(self.u_text))
        
class Latex(TextAst):
    r"$[u_text]$"

    def write_math_latex(self, out, ctx):
        out.write(self.u_text.encode("ASCII"))
            
class Operator(TextAst):
    r"Operator: [u_text]"
    
    def write_math_latex(self, out, ctx):
        out.write(ctx.math(self.u_text))
    
class Whitespace(TextAst):
    r"Whitespace: [u_text]"

    def write_math_latex(self, out, ctx):
        # Latex spaces: \!, \, \: \;, \quad, \qquad
        if self.u_text == u" ": out.write("\:")
        if self.u_text == u"  ": out.write("\;")
        if self.u_text == u"   ": out.write("\quad")
        if self.u_text == u"    ": out.write("\qquad")
