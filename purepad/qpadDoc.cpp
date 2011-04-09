// qpadDoc.cpp : implementation of the CQpadDoc class
//

#include "stdafx.h"
#include "qpad.h"
#include "pipe.h"

#include "qpadDoc.h"
#include "MainFrm.h"

#ifdef _DEBUG
#define new DEBUG_NEW
#undef THIS_FILE
static char THIS_FILE[] = __FILE__;
#endif

/////////////////////////////////////////////////////////////////////////////
// CQpadDoc

IMPLEMENT_DYNCREATE(CQpadDoc, CDocument)

BEGIN_MESSAGE_MAP(CQpadDoc, CDocument)
	//{{AFX_MSG_MAP(CQpadDoc)
	//}}AFX_MSG_MAP
END_MESSAGE_MAP()

/////////////////////////////////////////////////////////////////////////////
// CQpadDoc construction/destruction

CQpadDoc::CQpadDoc()
{
}

CQpadDoc::~CQpadDoc()
{
}

BOOL CQpadDoc::OnNewDocument()
{
	if (!CDocument::OnNewDocument())
		return FALSE;

	((CEditView*)m_viewList.GetHead())->SetWindowText(NULL);

	return TRUE;
}



/////////////////////////////////////////////////////////////////////////////
// CQpadDoc serialization

void CQpadDoc::Serialize(CArchive& ar)
{
	((CEditView*)m_viewList.GetHead())->SerializeRaw(ar);
}

/////////////////////////////////////////////////////////////////////////////
// CQpadDoc diagnostics

#ifdef _DEBUG
void CQpadDoc::AssertValid() const
{
	CDocument::AssertValid();
}

void CQpadDoc::Dump(CDumpContext& dc) const
{
	CDocument::Dump(dc);
}
#endif //_DEBUG

/////////////////////////////////////////////////////////////////////////////
// CQpadDoc commands



BOOL CQpadDoc::OnOpenDocument(LPCTSTR lpszPathName) 
{
	if (!CDocument::OnOpenDocument(lpszPathName))
		return FALSE;
	CEditView* edit = ((CEditView*)m_viewList.GetHead());

	CString s;
	edit->GetWindowText(s);
	int n = s.Find(_T('\r'));
	if (n < 0) {
		// Not a single CR in this file, probably a UNIX file (LF endings).
		s.Replace("\n", "\r\n");
		edit->SetWindowText(s);
	}

	// TODO: Speziellen Erstellungscode hier einfügen
	
	return TRUE;
}
