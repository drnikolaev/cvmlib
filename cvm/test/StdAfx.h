// stdafx.h : include file for standard system include files,
//  or project specific include files that are used frequently, but
//      are changed infrequently
//

#if !defined(AFX_STDAFX_H__DFAA8245_B521_11D3_9052_0020AF5D7A54__INCLUDED_)
#define AFX_STDAFX_H__DFAA8245_B521_11D3_9052_0020AF5D7A54__INCLUDED_

#if defined (_MSC_VER) && (_MSC_VER > 1000)
#pragma once
#endif // _MSC_VER > 1000


#if defined (_MSC_VER) || defined (__CYGWIN__) || defined (__BORLANDC__)
    #define WIN32_LEAN_AND_MEAN        // Exclude rarely-used stuff from Windows headers
    #define _WIN32_WINNT 0x500      // at least Win2000 is required for InitializeCriticalSectionAndSpinCount
    #include <windows.h>
    #include <process.h>
    #include <time.h>
#endif

//{{AFX_INSERT_LOCATION}}
// Microsoft Visual C++ will insert additional declarations immediately before the previous line.

#endif // !defined(AFX_STDAFX_H__DFAA8245_B521_11D3_9052_0020AF5D7A54__INCLUDED_)
