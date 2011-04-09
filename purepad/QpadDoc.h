// qpadDoc.h : interface of the CQpadDoc class
//
/////////////////////////////////////////////////////////////////////////////

#if !defined(AFX_QPADDOC_H__072F868A_9DF1_11D2_BCEB_F4A136FD1C64__INCLUDED_)
#define AFX_QPADDOC_H__072F868A_9DF1_11D2_BCEB_F4A136FD1C64__INCLUDED_

#if _MSC_VER > 1000
#pragma once
#endif // _MSC_VER > 1000

#include "Pipe.h"

class CQpadView;
class CEvalView;

class CQpadDoc : public CDocument
{
protected: // create from serialization only
	CQpadDoc();
	DECLARE_DYNCREATE(CQpadDoc)

// Attributes
public:

// Operations
public:

// Overrides
	// ClassWizard generated virtual function overrides
	//{{AFX_VIRTUAL(CQpadDoc)
	public:
	virtual BOOL OnNewDocument();
	virtual void Serialize(CArchive& ar);
	virtual BOOL OnOpenDocument(LPCTSTR lpszPathName);
	//}}AFX_VIRTUAL

// Implementation
public:
	virtual ~CQpadDoc();
#ifdef _DEBUG
	virtual void AssertValid() const;
	virtual void Dump(CDumpContext& dc) const;
#endif

// Generated message map functions
protected:
	//{{AFX_MSG(CQpadDoc)
	//}}AFX_MSG
	DECLARE_MESSAGE_MAP()
};

/////////////////////////////////////////////////////////////////////////////

//{{AFX_INSERT_LOCATION}}
// Microsoft Visual C++ will insert additional declarations immediately before the previous line.

#endif // !defined(AFX_QPADDOC_H__072F868A_9DF1_11D2_BCEB_F4A136FD1C64__INCLUDED_)
