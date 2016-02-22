import sys
import re

i = 0
with open("../testcvm.cpp") as f:
    for line in f:
        i = i + 1
        if i > 25:
            break

        sys.stdout.write('>>>>' + line)

        line = re.sub('\(treal\)\s*', '', line)
        line = re.sub('tcomplex\s*', 'TPC', line)
        line = re.sub('(\s*)CheckBool\s*\((\S+?)\s*==\s*(\S+?),\s*true\s*,\s*(.+?),.+', r'\1EXPECT_EQ(\2, \3) << \4;', line)
        line = re.sub('(\s*)CheckBool\s*\((\S+?)\s*==\s*(\S+?),\s*false\s*,\s*(.+?),.+', r'\1EXPECT_NE(\2, \3) << \4;', line)
        line = re.sub('(\s*)CheckBool\s*\((\S+?)\s*!=\s*(\S+?),\s*true\s*,\s*(.+?),.+', r'\1EXPECT_NE(\2, \3) << \4;', line)
        line = re.sub('(\s*)CheckBool\s*\((\S+?)\s*!=\s*(\S+?),\s*false\s*,\s*(.+?),.+', r'\1EXPECT_EQ(\2, \3) << \4;', line)


        #if re.match('\s*CheckBool', line):
        #    print 'BBBBBBB'


        #result = re.sub(r"(\d.*?)\s(\d.*?)", r"\1 \2", string1)
        line = re.sub(',(\S+?)', r', \1', line)
        line = re.sub('(.*?\(.*?\(.*?), (.*?\).*?\).*?)', r'\1,\2', line)
        line = re.sub('(.*?TPC\(.*?), (.*)', r'\1,\2', line)
        sys.stdout.write(line)
