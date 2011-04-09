// qpad.h : main header file for the QPAD application
//

#if !defined(AFX_QPAD_H__072F8684_9DF1_11D2_BCEB_F4A136FD1C64__INCLUDED_)
#define AFX_QPAD_H__072F8684_9DF1_11D2_BCEB_F4A136FD1C64__INCLUDED_

#if _MSC_VER > 1000
#pragma once
#endif // _MSC_VER > 1000

#ifndef __AFXWIN_H__
	#error include 'stdafx.h' before including this file for PCH
#endif

#include "resource.h"       // main symbols

#define WM_USER_INPUT (WM_USER+1)
#define WM_USER_COMPILE (WM_USER+2)

/////////////////////////////////////////////////////////////////////////////
// CQpadApp:
// See qpad.cpp for the implementation of this class
//

class CQpadApp : public CWinApp
{
public:
	CQpadApp();

// Overrides
	// ClassWizard generated virtual function overrides
	//{{AFX_VIRTUAL(CQpadApp)
	public:
	virtual BOOL InitInstance();
	virtual BOOL OnIdle(LONG lCount);
	virtual int ExitInstance();
	//}}AFX_VIRTUAL

// Implementation
	//{{AFX_MSG(CQpadApp)
	afx_msg void OnAppAbout();
	afx_msg void OnHelpFinder();
	//}}AFX_MSG
	DECLARE_MESSAGE_MAP()
};


/////////////////////////////////////////////////////////////////////////////

//{{AFX_INSERT_LOCATION}}
// Microsoft Visual C++ will insert additional declarations immediately before the previous line.

#endif // !defined(AFX_QPAD_H__072F8684_9DF1_11D2_BCEB_F4A136FD1C64__INCLUDED_)
