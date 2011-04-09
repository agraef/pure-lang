#if !defined(AFX_TABSTOPDLG_H__12D0EDE0_A327_11D2_BCEB_ACD57AECD965__INCLUDED_)
#define AFX_TABSTOPDLG_H__12D0EDE0_A327_11D2_BCEB_ACD57AECD965__INCLUDED_

#if _MSC_VER > 1000
#pragma once
#endif // _MSC_VER > 1000
// TabStopDlg.h : header file
//

/////////////////////////////////////////////////////////////////////////////
// CTabStopDlg dialog

class CTabStopDlg : public CDialog
{
// Construction
public:
	CTabStopDlg(CWnd* pParent = NULL);   // standard constructor

// Dialog Data
	//{{AFX_DATA(CTabStopDlg)
	enum { IDD = IDD_TABSTOPS };
	int m_nTabStops;
		// NOTE: the ClassWizard will add data members here
	//}}AFX_DATA


// Overrides
	// ClassWizard generated virtual function overrides
	//{{AFX_VIRTUAL(CTabStopDlg)
	protected:
	virtual void DoDataExchange(CDataExchange* pDX);    // DDX/DDV support
	//}}AFX_VIRTUAL

// Implementation
protected:

	// Generated message map functions
	//{{AFX_MSG(CTabStopDlg)
		// NOTE: the ClassWizard will add member functions here
	//}}AFX_MSG
	DECLARE_MESSAGE_MAP()
};

//{{AFX_INSERT_LOCATION}}
// Microsoft Visual C++ will insert additional declarations immediately before the previous line.

#endif // !defined(AFX_TABSTOPDLG_H__12D0EDE0_A327_11D2_BCEB_ACD57AECD965__INCLUDED_)
