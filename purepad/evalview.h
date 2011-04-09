// EvalView.h : interface of the CEvalView class
//
/////////////////////////////////////////////////////////////////////////////

#if !defined(AFX_LEFTVIEW_H__072F868E_9DF1_11D2_BCEB_F4A136FD1C64__INCLUDED_)
#define AFX_LEFTVIEW_H__072F868E_9DF1_11D2_BCEB_F4A136FD1C64__INCLUDED_

#include "pipe.h"	// Added by ClassView
#include "history.h"

#if _MSC_VER > 1000
#pragma once
#endif // _MSC_VER > 1000

class CGotoDlg;
class CQpadDoc;

class CEvalView : public CEditView
{
protected: // create from serialization only
	CEvalView();
	DECLARE_DYNCREATE(CEvalView)

// Attributes
public:
	CFont m_font;
	CHistory m_hist;
	CGotoDlg* m_pGotoDialog;
	CQpadDoc* GetDocument();

// Operations
public:

// Overrides
	// ClassWizard generated virtual function overrides
	//{{AFX_VIRTUAL(CEvalView)
	public:
	virtual void OnDraw(CDC* pDC);  // overridden to draw this view
	virtual BOOL PreCreateWindow(CREATESTRUCT& cs);
	protected:
	virtual BOOL OnPreparePrinting(CPrintInfo* pInfo);
	virtual void OnBeginPrinting(CDC* pDC, CPrintInfo* pInfo);
	virtual void OnEndPrinting(CDC* pDC, CPrintInfo* pInfo);
	//}}AFX_VIRTUAL

// Implementation
public:
	void Reset();
	BOOL Run(LPCTSTR name, LPCTSTR pname);
	BOOL Debug(LPCTSTR name, LPCTSTR pname);
	void Break();
	void Kill();
	void ProcessInput();
	BOOL IsRunning();
	virtual void OnActivateView( BOOL bActivate, CView* pActivateView, CView* pDeactiveView );
	virtual ~CEvalView();
#ifdef _DEBUG
	virtual void AssertValid() const;
	virtual void Dump(CDumpContext& dc) const;
#endif

protected:
	// pipe to interpreter
	CPipe m_pipe;
	// process marker (process output on current
	// command line)
	CString m_strPMark;
	// operations to handle command input and output
	void Append(LPCTSTR lpszText = NULL,
		BOOL mark = FALSE);
	void AppendNewLine(LPCTSTR lpszText = NULL,
		BOOL mark = FALSE);
	void PeekLine(int lineno, int len, CString& str,
		BOOL remove_prompt = FALSE);
	void PeekLine(int lineno, CString& str,
		BOOL remove_prompt = FALSE);
	void SetCmd(CString str);
	BOOL IsCmd();
public:
	void MarkLine(int lineno);
	void GotoLine(int lineno);
	BOOL GetFirstErr(CString& fname, int& lineno, int& ref,
		CString& msg, int ref0 = -1);
	BOOL GetLastErr(CString& fname, int& lineno, int& ref,
		CString& msg, int ref0 = -1);
	BOOL GetPrevErr(CString& fname, int& lineno, int& ref,
		CString& msg, int ref0 = -1);
	BOOL GetNextErr(CString& fname, int& lineno, int& ref,
		CString& msg, int ref0 = -1);
protected:
	BOOL GetErr(CString& fname, int& lineno, int& ref,
		CString& msg);

// Generated message map functions
protected:
	//{{AFX_MSG(CEvalView)
	afx_msg void OnChange();
	afx_msg void OnChar(UINT nChar, UINT nRepCnt, UINT nFlags);
	afx_msg void OnUpdateEditGoto(CCmdUI* pCmdUI);
	afx_msg void OnViewFont();
	afx_msg void OnViewTabstops();
	afx_msg int OnCreate(LPCREATESTRUCT lpCreateStruct);
	afx_msg void OnHistFirst();
	afx_msg void OnHistLast();
	afx_msg void OnHistNext();
	afx_msg void OnHistPrev();
	afx_msg void OnHistSearchNext();
	afx_msg void OnHistSearchPrev();
	//}}AFX_MSG
	void OnEditGoto();
	DECLARE_MESSAGE_MAP()
private:
	void GetSearchStr(CString& str);
};

#ifndef _DEBUG  // debug version in LeftView.cpp
inline CQpadDoc* CEvalView::GetDocument()
   { return (CQpadDoc*)m_pDocument; }
#endif

/////////////////////////////////////////////////////////////////////////////

//{{AFX_INSERT_LOCATION}}
// Microsoft Visual C++ will insert additional declarations immediately before the previous line.

#endif // !defined(AFX_LEFTVIEW_H__072F868E_9DF1_11D2_BCEB_F4A136FD1C64__INCLUDED_)
