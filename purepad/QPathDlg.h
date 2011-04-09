#if !defined(AFX_QPATHDLG_H__5516BB40_A59C_11D2_BCEB_A5D8005D0267__INCLUDED_)
#define AFX_QPATHDLG_H__5516BB40_A59C_11D2_BCEB_A5D8005D0267__INCLUDED_

#if _MSC_VER > 1000
#pragma once
#endif // _MSC_VER > 1000
// QPathDlg.h : header file
//

/////////////////////////////////////////////////////////////////////////////
// CQPathDlg dialog

class CQPathDlg : public CDialog
{
// Construction
public:
	CQPathDlg(CWnd* pParent = NULL);   // standard constructor

// Dialog Data
	//{{AFX_DATA(CQPathDlg)
	enum { IDD = IDD_QPATH };
	CString	m_strPath;
	//}}AFX_DATA


// Overrides
	// ClassWizard generated virtual function overrides
	//{{AFX_VIRTUAL(CQPathDlg)
	protected:
	virtual void DoDataExchange(CDataExchange* pDX);    // DDX/DDV support
	//}}AFX_VIRTUAL

// Implementation
protected:

	// Generated message map functions
	//{{AFX_MSG(CQPathDlg)
		// NOTE: the ClassWizard will add member functions here
	//}}AFX_MSG
	DECLARE_MESSAGE_MAP()
};

//{{AFX_INSERT_LOCATION}}
// Microsoft Visual C++ will insert additional declarations immediately before the previous line.

#endif // !defined(AFX_QPATHDLG_H__5516BB40_A59C_11D2_BCEB_A5D8005D0267__INCLUDED_)
