// qpadView.h : interface of the CQpadView class
//
/////////////////////////////////////////////////////////////////////////////

#if !defined(AFX_QPADVIEW_H__072F868C_9DF1_11D2_BCEB_F4A136FD1C64__INCLUDED_)
#define AFX_QPADVIEW_H__072F868C_9DF1_11D2_BCEB_F4A136FD1C64__INCLUDED_

#if _MSC_VER > 1000
#pragma once
#endif // _MSC_VER > 1000

class CGotoDlg;
class CQpadDoc;

class CQpadView : public CEditView
{
protected: // create from serialization only
	CQpadView();
	DECLARE_DYNCREATE(CQpadView)

// Attributes
public:
	CQpadDoc* GetDocument();

// Operations
public:
	void GotoLine(int lineno);
	void MarkLine(int lineno);

// Overrides
	// ClassWizard generated virtual function overrides
	//{{AFX_VIRTUAL(CQpadView)
	public:
	virtual void OnDraw(CDC* pDC);  // overridden to draw this view
	virtual BOOL PreCreateWindow(CREATESTRUCT& cs);
	protected:
	virtual BOOL OnPreparePrinting(CPrintInfo* pInfo);
	virtual void OnBeginPrinting(CDC* pDC, CPrintInfo* pInfo);
	virtual void OnEndPrinting(CDC* pDC, CPrintInfo* pInfo);
	virtual void OnActivateView(BOOL bActivate, CView* pActivateView, CView* pDeactiveView);
	//}}AFX_VIRTUAL

// Implementation
public:
	CFont m_font;
	CGotoDlg*    m_pGotoDialog;             // goto line number dialog
	void OnEditGoto();
	virtual ~CQpadView();
#ifdef _DEBUG
	virtual void AssertValid() const;
	virtual void Dump(CDumpContext& dc) const;
#endif

protected:

// Generated message map functions
protected:
	//{{AFX_MSG(CQpadView)
	afx_msg void OnUpdateEditGoto(CCmdUI* pCmdUI);
	afx_msg void OnViewFont();
	afx_msg void OnViewTabstops();
	afx_msg int OnCreate(LPCREATESTRUCT lpCreateStruct);
	//}}AFX_MSG
	DECLARE_MESSAGE_MAP()
};

#ifndef _DEBUG  // debug version in qpadView.cpp
inline CQpadDoc* CQpadView::GetDocument()
   { return (CQpadDoc*)m_pDocument; }
#endif

/////////////////////////////////////////////////////////////////////////////

//{{AFX_INSERT_LOCATION}}
// Microsoft Visual C++ will insert additional declarations immediately before the previous line.

#endif // !defined(AFX_QPADVIEW_H__072F868C_9DF1_11D2_BCEB_F4A136FD1C64__INCLUDED_)
