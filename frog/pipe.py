# This allows us to launch Python and pygments once, and pipe to it
# continuously. Input format is:
#
#     <lexer-name>
#     <code>
#     ...
#     __END__
#
#  OR
#
#     __EXIT__
#
# Output format is:
#
#     <html>
#     ...
#     __END__

import sys
from pygments import highlight
from pygments.lexers import get_lexer_by_name
from pygments.formatters import HtmlFormatter

formatter = HtmlFormatter(linenos=True, cssclass="source", encoding="utf-8")
lexer = ""
code = ""
while 1:
    line_raw = sys.stdin.readline()
    if not line_raw:
        break
    # Without trailing space, \n, or \n
    line = line_raw.rstrip()
    if line == '__EXIT__':
        break
    elif line == '__END__':
        # Lex input finished. Lex it.
        sys.stdout.write(highlight(code, lexer, formatter))
        sys.stdout.write('\n__END__\n')
        sys.stdout.flush
        lexer = ""
        code = ""
    elif lexer == "":
        # Starting another lex. First line is the lexer name.
        lexer = get_lexer_by_name(line, encoding="guess")
    else:
        # Accumulate more code
        # Use `line_raw`: Do want trailing space, \n, \r
        code += line_raw

exit(0)
