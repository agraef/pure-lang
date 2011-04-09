// MainFrm.h : interface of the CMainFrame class
//
/////////////////////////////////////////////////////////////////////////////

#if !defined(AFX_MAINFRM_H__072F8688_9DF1_11D2_BCEB_F4A136FD1C64__INCLUDED_)
#define AFX_MAINFRM_H__072F8688_9DF1_11D2_BCEB_F4A136FD1C64__INCLUDED_

#include "qpadView.h"	// Added by ClassView
#include "EvalView.h"	// Added by ClassView
#if _MSC_VER > 1000
#pragma once
#endif // _MSC_VER > 1000

class CQpadView;
class CEvalView;

class CMainFrame : public CFrameWnd
{
	
protected: // create from serialization only
	CMainFrame();
	DECLARE_DYNCREATE(CMainFrame)

// Attributes
protected:
	CSplitterWnd m_wndSplitter;
	int m_nPaneSplit;
	CString m_strRunPathName, m_strRunPath, m_strRunFile,
		m_strRunTitle;
public:

	// static config information (read from registry at
	// initialization)

	static BOOL m_bLogReset;
	static int m_nTabStops, m_nLogTabStops;
	static LOGFONT m_lfFont, m_lfLogFont;
	static int m_nHistSize, m_nLogMaxLines;

	static CString m_strHistFile;
	// the history file

	static CString m_strQPATH;
	// the library path; default is ".;<apppath>\lib" where <apppath>
	// is the value of the m_strAppPath variable; NOTE: this is
	// reinitialized every time the application path changes

#if 0
	static CString m_strCompileCommand;
	// the command to run the compiler (only used to check for
	// syntax errors, hence the -n option); the first %s indicates the
	// position for the code file name, the second the position
	// for the script filename (if any); default: "qc -n -o %s %s"
#endif

	static CString m_strRunCommand;
	// the command to run the interpreter; default: "q -i -q -o %s %s"

	static CString m_strDebugCommand;
	// the command to run the interpreter in debug mode;
	// default: "q -d -i -q -o %s %s"

	static CString m_strAppPath;
	// the path to the running application

	static CString m_strVersion;
	// application version

	static CString m_strQPS, m_strQPS2, m_strDPS;
	// interpreter and debugger prompts; defaults: "\n==> ", "> "
	// and ": "

	static CString m_strQPSX;
	// pattern derived from the last line of the QPS prompt string

// Operations
public:

// Overrides
	// ClassWizard generated virtual function overrides
	//{{AFX_VIRTUAL(CMainFrame)
	public:
	virtual BOOL OnCreateClient(LPCREATESTRUCT lpcs, CCreateContext* pContext);
	protected:
	virtual BOOL OnCommand(WPARAM wParam, LPARAM lParam);
	//}}AFX_VIRTUAL

// Implementation
public:
	virtual ~CMainFrame();
	static void Initialize();
	static void Terminate();
	void InitialShowWindow(int nCmdShow);
	BOOL IsRunning();
	void UpdateTitle();
	CEvalView* CMainFrame::GetEvalView();
	CQpadView* CMainFrame::GetQpadView();
	void ActivateEvalView();
	void ActivateQpadView();
	void ResetEvalView();
#ifdef _DEBUG
	virtual void AssertValid() const;
	virtual void Dump(CDumpContext& dc) const;
#endif

protected:	// embedded members
	CStatusBar  m_wndStatusBar;
	CToolBar    m_wndToolBar;

	void SetStatusInfo();
	void OnErr(CString& fname, int& lineno,
			   int& ref, CString& msg);

// Generated message map functions
protected:
	//{{AFX_MSG(CMainFrame)
	afx_msg int OnCreate(LPCREATESTRUCT lpCreateStruct);
	afx_msg void OnClose();
	afx_msg void OnScriptRun();
	afx_msg void OnScriptDebug();
	afx_msg void OnScriptBreak();
	afx_msg void OnScriptKill();
	afx_msg void OnUpdateScriptBreak(CCmdUI* pCmdUI);
	afx_msg void OnUpdateScriptKill(CCmdUI* pCmdUI);
	afx_msg void OnScriptReset();
	afx_msg void OnUpdateScriptReset(CCmdUI* pCmdUI);
	afx_msg void OnNextErr();
	afx_msg void OnUpdateNextErr(CCmdUI* pCmdUI);
	afx_msg void OnPrevErr();
	afx_msg void OnUpdatePrevErr(CCmdUI* pCmdUI);
	afx_msg void OnFirstErr();
	afx_msg void OnUpdateFirstErr(CCmdUI* pCmdUI);
	afx_msg void OnLastErr();
	afx_msg void OnUpdateLastErr(CCmdUI* pCmdUI);
	afx_msg void OnInputLine();
	afx_msg void OnUpdateInputLine(CCmdUI* pCmdUI);
	afx_msg void OnScriptOpen();
	afx_msg void OnUpdateScriptOpen(CCmdUI* pCmdUI);
	afx_msg void OnScriptPath();
	afx_msg void OnScriptPrompt();
	afx_msg void OnScriptHistfile();
	afx_msg void OnScriptHistsize();
	//}}AFX_MSG
	LRESULT OnInput(WPARAM, LPARAM);
	LRESULT OnCompile(WPARAM, LPARAM);
	afx_msg void OnUpdateLineNumber(CCmdUI* pCmdUI);
	DECLARE_MESSAGE_MAP()
};

/////////////////////////////////////////////////////////////////////////////

//{{AFX_INSERT_LOCATION}}
// Microsoft Visual C++ will insert additional declarations immediately before the previous line.

#endif // !defined(AFX_MAINFRM_H__072F8688_9DF1_11D2_BCEB_F4A136FD1C64__INCLUDED_)
