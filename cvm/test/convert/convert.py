import sys
import re

i = 0
ibeg = 0
iend = 2500
with open("../testcvm.cpp") as f:
    for line in f:
        i = i + 1

        # if i >= ibeg:
        #     sys.stdout.write(str(i) + ' >>>>' + line)
        # else:
        #     continue

        line = re.sub('cvm::', r'', line)
        line = re.sub('^(\s*)treal', r'\1TP', line)
        line = re.sub('std::complex<treal>', r'TPC', line)
        line = re.sub('\(treal\)\s*', '', line)
        line = re.sub('tcomplex\s*', 'TPC', line)


        line = re.sub('(\s*)CheckBool\s*\((\S+?)\s*==\s*(\S+?),\s*true\s*,\s*(\".+\"),.+', r"\1EXPECT_TRUE(\2 == \3) << \4;", line)
        line = re.sub('(\s*)CheckBool\s*\((\S+?)\s*==\s*(\S+?),\s*false\s*,\s*(\".+\"),.+', r"\1EXPECT_FALSE(\2 == \3) << \4;", line)
        line = re.sub('(\s*)CheckBool\s*\((\S+?)\s*!=\s*(\S+?),\s*true\s*,\s*(\".+\"),.+', r"\1EXPECT_TRUE(\2 != \3) << \4;", line)
        line = re.sub('(\s*)CheckBool\s*\((\S+?)\s*!=\s*(\S+?),\s*false\s*,\s*(\".+\"),.+', r"\1EXPECT_FALSE(\2 != \3) << \4;", line)


        line = re.sub('(\s*)Check.+?\s*\((.+?\(.+?,.+?\)),\s*(.+?\(.+?,.+?\)[^,]+),\s*(\".+\")\s*,.+?Pessimistic.+',
                      r'\1EXPECT_NEAR(\3, \2, s<TP>()) << \4;', line)
        line = re.sub('(\s*)Check.+?\s*\((.+?\(.+?,.+?\)),\s*(.+?\(.+?,.+?\)),\s*(\".+\")\s*,.+?Pessimistic.+',
                      r'\1EXPECT_NEAR(\3, \2, s<TP>()) << \4;', line)
        line = re.sub('(\s*)Check.+?\s*\((.+?),\s*(.+?\(.+?,.+?\)),\s*(\".+\")\s*,.+?Pessimistic.+',
                      r'\1EXPECT_NEAR(\3, \2, s<TP>()) << \4;', line)
        line = re.sub('(\s*)Check.+?\s*\((.+?\(.+?,.+?\)),\s*(.+?),\s*(\".+\")\s*,.+?Pessimistic.+',
                      r'\1EXPECT_NEAR(\3, \2, s<TP>()) << \4;', line)
        line = re.sub('(\s*)Check.+?\s*\((.+?),\s*(.+?),\s*(\".+\")\s*,.+?Pessimistic.+',
                      r'\1EXPECT_NEAR(\3, \2, s<TP>()) << \4;', line)


        line = re.sub('(\s*)Check.+?\s*\((.+?\(.+?,.+?\)\s*\(.+?,.+?\)),\s*(.+?\(.+?,.+?\)),\s*(\".+\")\s*,.+', r'\1EXPECT_EQ(\3, \2) << \4;', line)
        line = re.sub('(\s*)Check.+?\s*\((.+?\(.+?,.+?\)),\s*(.+?\(.+?,.+?\)[^,]+),\s*(\".+\")\s*,.+', r'\1EXPECT_EQ(\3, \2) << \4;', line)
        line = re.sub('(\s*)Check.+?\s*\((.+?\(.+?,.+?\)),\s*(.+?\(.+?,.+?\)),\s*(\".+\")\s*,.+', r'\1EXPECT_EQ(\3, \2) << \4;', line)
        line = re.sub('(\s*)Check.+?\s*\((.+?),\s*(.+?\(.+?,.+?\)),\s*(\".+\")\s*,.+', r'\1EXPECT_EQ(\3, \2) << \4;', line)
        line = re.sub('(\s*)Check.+?\s*\((.+?\(.+?,.+?\)),\s*(.+?),\s*(\".+\")\s*,.+', r'\1EXPECT_EQ(\3, \2) << \4;', line)
        line = re.sub('(\s*)Check.+?\s*\((.+?),\s*(.+?),\s*(\".+\")\s*,.+', r'\1EXPECT_EQ(\3, \2) << \4;', line)

        line = re.sub(',(\S+?)', r', \1', line)
#        line = re.sub('(.*?\(.*?\(.*?), (.*?\).*?\).*?)', r'\1,\2', line)
#        line = re.sub('(.*),\s+(.*?\)\).*?)', r'\1,\2', line)
#        sys.stdout.write(line)
        line = re.sub('(.*?TPC\(.*?), (.*)', r'\1,\2', line)
#        line = re.sub('([\(|\s])(a\d)', r'\1this->\2', line)
#        line = re.sub('([\(|\s])(c\d)', r'\1this->\2', line)

        line = re.sub('^(\s*)rvector', r'\1basic_rvector<TP>', line)
        line = re.sub('^(\s*)rfvector', r'\1basic_rfvector<TP>', line)
        line = re.sub('^(\s*)rmatrix', r'\1basic_rmatrix<TP>', line)
        line = re.sub('^(\s*)rfmatrix', r'\1basic_rfmatrix<TP>', line)
        line = re.sub('^(\s*)srmatrix', r'\1basic_srmatrix<TP>', line)
        line = re.sub('^(\s*)srsmatrix', r'\1basic_srsmatrix<TP>', line)
        line = re.sub('^(\s*)srbmatrix', r'\1basic_srbmatrix<TP>', line)
        line = re.sub('^(\s*)cvector', r'\1basic_cvector<TP,TPC>', line)
        line = re.sub('^(\s*)cfvector', r'\1basic_cfvector<TP,TPC>', line)
        line = re.sub('^(\s*)cmatrix', r'\1basic_cmatrix<TP,TPC>', line)
        line = re.sub('^(\s*)cfmatrix', r'\1basic_cfmatrix<TP,TPC>', line)
        line = re.sub('^(\s*)scmatrix', r'\1basic_scmatrix<TP,TPC>', line)
        line = re.sub('^(\s*)schmatrix', r'\1basic_schmatrix<TP,TPC>', line)
        line = re.sub('^(\s*)scbmatrix', r'\1basic_scbmatrix<TP,TPC>', line)

        line = re.sub('^(\s*const\s+)rvector', r'\1basic_rvector<TP>', line)
        line = re.sub('^(\s*const\s+)rfvector', r'\1basic_rfvector<TP>', line)
        line = re.sub('^(\s*const\s+)rmatrix', r'\1basic_rmatrix<TP>', line)
        line = re.sub('^(\s*const\s+)rfmatrix', r'\1basic_rfmatrix<TP>', line)
        line = re.sub('^(\s*const\s+)srmatrix', r'\1basic_srmatrix<TP>', line)
        line = re.sub('^(\s*const\s+)srsmatrix', r'\1basic_srsmatrix<TP>', line)
        line = re.sub('^(\s*const\s+)srbmatrix', r'\1basic_srbmatrix<TP>', line)
        line = re.sub('^(\s*const\s+)cvector', r'\1basic_cvector<TP,TPC>', line)
        line = re.sub('^(\s*const\s+)cfvector', r'\1basic_cfvector<TP,TPC>', line)
        line = re.sub('^(\s*const\s+)cmatrix', r'\1basic_cmatrix<TP,TPC>', line)
        line = re.sub('^(\s*const\s+)cfmatrix', r'\1basic_cfmatrix<TP,TPC>', line)
        line = re.sub('^(\s*const\s+)scmatrix', r'\1basic_scmatrix<TP,TPC>', line)
        line = re.sub('^(\s*const\s+)schmatrix', r'\1basic_schmatrix<TP,TPC>', line)
        line = re.sub('^(\s*const\s+)scbmatrix', r'\1basic_scbmatrix<TP,TPC>', line)

        line = re.sub('rvector\s*\(', r'basic_rvector<TP>(', line)
        line = re.sub('[^s]+rmatrix\s*\(', r'basic_rmatrix<TP>(', line)
        line = re.sub('rfvector\s*\(', r'basic_rfvector<TP>(', line)
        line = re.sub('rfmatrix\s*\(', r'basic_rfmatrix<TP>(', line)
        line = re.sub('srmatrix\s*\(', r'basic_srmatrix<TP>(', line)
        line = re.sub('srsmatrix\s*\(', r'basic_srsmatrix<TP>(', line)
        line = re.sub('srbmatrix\s*\(', r'basic_srbmatrix<TP>(', line)
        line = re.sub('cvector\s*\(', r'basic_cvector<TP,TPC>(', line)
        line = re.sub('[^s]+cmatrix\s*\(', r'basic_cmatrix<TP,TPC>(', line)
        line = re.sub('cfvector\s*\(', r'basic_cfvector<TP,TPC>(', line)
        line = re.sub('cfmatrix\s*\(', r'basic_cfmatrix<TP,TPC>(', line)
        line = re.sub('scmatrix\s*\(', r'basic_scmatrix<TP,TPC>(', line)
        line = re.sub('schmatrix\s*\(', r'basic_schmatrix<TP,TPC>(', line)
        line = re.sub('scbmatrix\s*\(', r'basic_scbmatrix<TP,TPC>(', line)

        line = re.sub('eye_real', r'basic_eye_real<TP>', line)
        line = re.sub('eye_complex', r'basic_eye_complex<TP,TPC>', line)

        line = re.sub('^(\s*)(EXPECT.+?)\(([\d|\.|e|\+|-]+?)\s*,[^\d]', r'\1\2(TP(\3), ', line)
        line = re.sub('^(\s*)(EXPECT_NEAR)\((TPC\(.+?,.+?\)),\s*(.+?),\s*s<TP>',
                      r'\1\2(std::abs(\3), std::abs(\4), s<TP>', line)

        if i >= ibeg:
            sys.stdout.write(line)
        if i >= iend:
            break
