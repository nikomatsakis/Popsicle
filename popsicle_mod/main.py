"""
popsicle [encoding] [filenames...]

Executes the given filenames, which are read in with the given encoding.
"""

import sys
from . import parser

def main(args):
    if len(args) < 2:
        sys.stderr.write(__doc__.strip())
        sys.stderr.write("\n")
        return 1
        
    encoding = args[0]
    for filename in args[1:]:
        try:
            node = parser.parse_file(filename, encoding)
            #node.serialize(sys.stdout)
            node.execute()
        except parser.Expected, e:
            sys.stderr.write(str(e))
            sys.stderr.write("\n")
            return 1
        
    return 0
    
if __name__ == "__main__":
    sys.exit(main(sys.argv[1:]))
    parser.dump_re_stats()