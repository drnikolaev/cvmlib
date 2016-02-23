import sys
import re

i = 0
ibeg = 0
iend = 100000
with open("../testcvm.cpp") as f:
    for line in f:
        i = i + 1

#        if i > ibeg:
#            sys.stdout.write(str(i) + ' >>>>' + line)

        line = re.sub('\(treal\)\s*', '', line)
        line = re.sub('tcomplex\s*', 'TPC', line)
        line = re.sub('(\s*)CheckBool\s*\((\S+?)\s*==\s*(\S+?),\s*true\s*,\s*(\".+\"),.+', r"\1EXPECT_TRUE(\2 == \3) << \4;", line)
        line = re.sub('(\s*)CheckBool\s*\((\S+?)\s*==\s*(\S+?),\s*false\s*,\s*(\".+\"),.+', r"\1EXPECT_FALSE(\2 == \3) << \4;", line)
        line = re.sub('(\s*)CheckBool\s*\((\S+?)\s*!=\s*(\S+?),\s*true\s*,\s*(\".+\"),.+', r"\1EXPECT_TRUE(\2 != \3) << \4;", line)
        line = re.sub('(\s*)CheckBool\s*\((\S+?)\s*!=\s*(\S+?),\s*false\s*,\s*(\".+\"),.+', r"\1EXPECT_FALSE(\2 != \3) << \4;", line)

        line = re.sub('(\s*)Check.+?\s*\((.+?\(.+?,.+?\)),\s*(.+?\(.+?,.+?\)[^,]+),\s*(\".+\"),.+', r'\1EXPECT_EQ(\3, \2) << \4;', line)
        line = re.sub('(\s*)Check.+?\s*\((.+?\(.+?,.+?\)),\s*(.+?\(.+?,.+?\)),\s*(\".+\"),.+', r'\1EXPECT_EQ(\3, \2) << \4;', line)
        line = re.sub('(\s*)Check.+?\s*\((.+?),\s*(.+?\(.+?,.+?\)),\s*(\".+\"),.+', r'\1EXPECT_EQ(\3, \2) << \4;', line)
        line = re.sub('(\s*)Check.+?\s*\((.+?\(.+?,.+?\)),\s*(.+?),\s*(\".+\"),.+', r'\1EXPECT_EQ(\3, \2) << \4;', line)
        line = re.sub('(\s*)Check.+?\s*\((.+?),\s*(.+?),\s*(\".+\"),.+', r'\1EXPECT_EQ(\3, \2) << \4;', line)


        line = re.sub(',(\S+?)', r', \1', line)
        line = re.sub('(.*?\(.*?\(.*?), (.*?\).*?\).*?)', r'\1,\2', line)
        line = re.sub('(.*),\s+(.*?\)\).*?)', r'\1,\2', line)
        line = re.sub('(.*?TPC\(.*?), (.*)', r'\1,\2', line)
        line = re.sub('([\(|\s])(a\d)', r'\1this->\2', line)
        line = re.sub('([\(|\s])(c\d)', r'\1this->\2', line)

        line = re.sub('^(\s*)srbmatrix', r'\1basic_srbmatrix<TP>', line)

        if i > ibeg:
            sys.stdout.write(line)
        if i > iend:
            break
