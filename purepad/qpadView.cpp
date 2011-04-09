// qpadView.cpp : implementation of the CQpadView class
//

#include "stdafx.h"
#include "qpad.h"
#include "gotodlg.h"
#include "tabstopdlg.h"

#include "MainFrm.h"
#include "qpadDoc.h"
#include "qpadView.h"

#ifdef _DEBUG
#define new DEBUG_NEW
#undef THIS_FILE
static char THIS_FILE[] = __FILE__;
#endif

/////////////////////////////////////////////////////////////////////////////
// CQpadView

IMPLEMENT_DYNCREATE(CQpadView, CEditView)

BEGIN_MESSAGE_MAP(CQpadView, CEditView)
	//{{AFX_MSG_MAP(CQpadView)
	ON_UPDATE_COMMAND_UI(ID_EDIT_GOTO, OnUpdateEditGoto)
	ON_COMMAND(ID_VIEW_FONT, OnViewFont)
	ON_COMMAND(ID_VIEW_TABSTOPS, OnViewTabstops)
	ON_COMMAND(ID_EDIT_GOTO, OnEditGoto)
	ON_WM_CREATE()
	//}}AFX_MSG_MAP
	// Standard printing commands
	ON_COMMAND(ID_FILE_PRINT, CEditView::OnFilePrint)
	ON_COMMAND(ID_FILE_PRINT_DIRECT, CEditView::OnFilePrint)
	ON_COMMAND(ID_FILE_PRINT_PREVIEW, CEditView::OnFilePrintPreview)
END_MESSAGE_MAP()

/////////////////////////////////////////////////////////////////////////////
// CQpadView construction/destruction

CQpadView::CQpadView()
{
	// TODO: add construction code here
	m_pGotoDialog = new CGotoDlg;
	m_nTabStops = CMainFrame::m_nTabStops;
}

CQpadView::~CQpadView()
{
	delete  m_pGotoDialog;
}

BOOL CQpadView::PreCreateWindow(CREATESTRUCT& cs)
{
	// TODO: Modify the Window class or styles here by modifying
	//  the CREATESTRUCT cs

	return CEditView::PreCreateWindow(cs);
}

/////////////////////////////////////////////////////////////////////////////
// CQpadView drawing

void CQpadView::OnDraw(CDC* pDC)
{
	CQpadDoc* pDoc = GetDocument();
	ASSERT_VALID(pDoc);
}



/////////////////////////////////////////////////////////////////////////////
// CQpadView printing

BOOL CQpadView::OnPreparePrinting(CPrintInfo* pInfo)
{
	return CEditView::OnPreparePrinting(pInfo);
}

void CQpadView::OnBeginPrinting(CDC* pDC, CPrintInfo* pInfo)
{
	CEditView::OnBeginPrinting(pDC, pInfo);
}

void CQpadView::OnEndPrinting(CDC* pDC, CPrintInfo* pInfo)
{
	CEditView::OnEndPrinting(pDC, pInfo);
}

/////////////////////////////////////////////////////////////////////////////
// CQpadView diagnostics

#ifdef _DEBUG
void CQpadView::AssertValid() const
{
	CEditView::AssertValid();
}

void CQpadView::Dump(CDumpContext& dc) const
{
	CEditView::Dump(dc);
}

CQpadDoc* CQpadView::GetDocument() // non-debug version is inline
{
	ASSERT(m_pDocument->IsKindOf(RUNTIME_CLASS(CQpadDoc)));
	return (CQpadDoc*)m_pDocument;
}
#endif //_DEBUG


void CQpadView::OnActivateView(BOOL bActivate, CView* pActivateView, CView* pDeactiveView) 
{
	// TODO: Add your specialized code here and/or call the base class
	
	CEditView::OnActivateView(bActivate, pActivateView, pDeactiveView);
}

void CQpadView::OnUpdateEditGoto(CCmdUI* pCmdUI) 
{
	pCmdUI->Enable(GetWindowTextLength());
}

void CQpadView::OnEditGoto()
{
	if(m_pGotoDialog->DoModal() != IDOK)
		return;

	if(m_pGotoDialog->m_nLineNum < 1)
	{
		CString s1;
		s1.LoadString(IDS_INVALID_LINENUM);
		AfxMessageBox(s1);
		return;
	}

	CEdit &edit = GetEditCtrl();
	int i = edit.LineFromChar();            // this is the current line the cursor is on
	int nLine = m_pGotoDialog->m_nLineNum;  // line number to go to

	if(nLine > (edit.GetLineCount()))
		nLine = edit.GetLineCount();

	// move window and caret
	--nLine;                    // edit control is zero based
	edit.LineScroll(nLine-i);   // new line number - the current line

	int idx;
	idx = edit.LineIndex(nLine);
	edit.SetSel(idx, idx);
}


void CQpadView::OnViewFont() 
{
   CFont* pFont = GetFont();
   LOGFONT lf;
   if (pFont != NULL)
	   pFont->GetObject(sizeof(LOGFONT), &lf);
   else
	   ::GetObject(GetStockObject(SYSTEM_FONT), sizeof(LOGFONT), &lf);

	CFontDialog dlg(&lf, CF_SCREENFONTS|CF_INITTOLOGFONTSTRUCT);
	if (dlg.DoModal() == IDOK)
	{
		m_font.DeleteObject();
		if (m_font.CreateFontIndirect(&lf))
		{
			CWaitCursor wait;
			SetFont(&m_font);
			CMainFrame::m_lfFont = lf;
		}
	}
}

void CQpadView::OnViewTabstops() 
{
	CTabStopDlg dlg;
	dlg.m_nTabStops = m_nTabStops/4;
	if (dlg.DoModal() == IDOK)
	{
		SetTabStops(dlg.m_nTabStops*4);
		CMainFrame::m_nTabStops = m_nTabStops;
	}
}

int CQpadView::OnCreate(LPCREATESTRUCT lpCreateStruct) 
{
	if (CEditView::OnCreate(lpCreateStruct) == -1)
		return -1;
	
	if (CMainFrame::m_lfFont.lfHeight != 0)
	{
		m_font.CreateFontIndirect(&CMainFrame::m_lfFont);
		SetFont(&m_font);
	}
	
	return 0;
}

void CQpadView::GotoLine(int lineno)
{
	CEdit& edit = GetEditCtrl();
	int p = edit.LineIndex(lineno);
	if (p != -1)
		edit.SetSel(p, p);
}

void CQpadView::MarkLine(int lineno)
{
	CEdit& edit = GetEditCtrl();
	int p = edit.LineIndex(lineno);
	if (p != -1) {
		int p2 = edit.LineIndex(lineno+1);
		if (p2 == -1)
			p2 = edit.GetWindowTextLength();
		else
			p2 -= 2; // CR/LF
		edit.SetSel(p, p2);
	}
}
