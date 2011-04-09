// EvalView.cpp : implementation of the CEvalView class
//

#include "stdafx.h"
#include "qpad.h"
#include "gotodlg.h"
#include "tabstopdlg.h"

#include "MainFrm.h"
#include "qpadDoc.h"
#include "EvalView.h"

#ifdef _DEBUG
#define new DEBUG_NEW
#undef THIS_FILE
static char THIS_FILE[] = __FILE__;
#endif

/////////////////////////////////////////////////////////////////////////////
// CEvalView

IMPLEMENT_DYNCREATE(CEvalView, CEditView)

BEGIN_MESSAGE_MAP(CEvalView, CEditView)
	//{{AFX_MSG_MAP(CEvalView)
	ON_CONTROL_REFLECT(EN_CHANGE, OnChange)
	ON_WM_CHAR()
	ON_UPDATE_COMMAND_UI(ID_EDIT_GOTO, OnUpdateEditGoto)
	ON_COMMAND(ID_VIEW_FONT, OnViewFont)
	ON_COMMAND(ID_VIEW_TABSTOPS, OnViewTabstops)
	ON_WM_CREATE()
	ON_COMMAND(ID_HIST_FIRST, OnHistFirst)
	ON_COMMAND(ID_HIST_LAST, OnHistLast)
	ON_COMMAND(ID_HIST_NEXT, OnHistNext)
	ON_COMMAND(ID_HIST_PREV, OnHistPrev)
	ON_COMMAND(ID_HIST_SEARCH_NEXT, OnHistSearchNext)
	ON_COMMAND(ID_HIST_SEARCH_PREV, OnHistSearchPrev)
	ON_COMMAND(ID_EDIT_GOTO, OnEditGoto)
	//}}AFX_MSG_MAP
	// update view when input from interpreter is available
	// Standard printing commands
	ON_COMMAND(ID_FILE_PRINT, CEditView::OnFilePrint)
	ON_COMMAND(ID_FILE_PRINT_DIRECT, CEditView::OnFilePrint)
	ON_COMMAND(ID_FILE_PRINT_PREVIEW, CEditView::OnFilePrintPreview)
END_MESSAGE_MAP()

/////////////////////////////////////////////////////////////////////////////
// CEvalView construction/destruction

CEvalView::CEvalView() : m_hist(CMainFrame::m_nHistSize)
{
	// TODO: add construction code here
	m_pGotoDialog = new CGotoDlg;
	m_nTabStops = CMainFrame::m_nLogTabStops;
	m_strPMark = "";
	m_hist.Read(CMainFrame::m_strHistFile);
}

CEvalView::~CEvalView()
{
	m_hist.Write(CMainFrame::m_strHistFile);
	delete  m_pGotoDialog;
}

BOOL CEvalView::PreCreateWindow(CREATESTRUCT& cs)
{
	// TODO: Modify the Window class or styles here by modifying
	//  the CREATESTRUCT cs

	return CEditView::PreCreateWindow(cs);
}

/////////////////////////////////////////////////////////////////////////////
// CEvalView drawing

void CEvalView::OnDraw(CDC* pDC)
{
	CQpadDoc* pDoc = GetDocument();
	ASSERT_VALID(pDoc);
}


/////////////////////////////////////////////////////////////////////////////
// CEvalView printing

BOOL CEvalView::OnPreparePrinting(CPrintInfo* pInfo)
{
	return DoPreparePrinting(pInfo);
}

void CEvalView::OnBeginPrinting(CDC* pDC, CPrintInfo* pInfo)
{
	CEditView::OnBeginPrinting(pDC, pInfo);
}

void CEvalView::OnEndPrinting(CDC* pDC, CPrintInfo* pInfo)
{
	CEditView::OnEndPrinting(pDC, pInfo);
}



/////////////////////////////////////////////////////////////////////////////
// CEvalView diagnostics

#ifdef _DEBUG
void CEvalView::AssertValid() const
{
	CEditView::AssertValid();
}

void CEvalView::Dump(CDumpContext& dc) const
{
	CEditView::Dump(dc);
}

CQpadDoc* CEvalView::GetDocument() // non-debug version is inline
{
	ASSERT(m_pDocument->IsKindOf(RUNTIME_CLASS(CQpadDoc)));
	return (CQpadDoc*)m_pDocument;
}
#endif //_DEBUG

/////////////////////////////////////////////////////////////////////////////
// CEvalView message handlers

void CEvalView::OnActivateView(BOOL bActivate, CView *pActivateView, CView *pDeactiveView)
{
	CEditView::OnActivateView(bActivate, pActivateView, pDeactiveView);
}


void CEvalView::OnChange() 
{
}

void CEvalView::OnUpdateEditGoto(CCmdUI* pCmdUI) 
{
	pCmdUI->Enable(GetWindowTextLength());
}

void CEvalView::OnEditGoto()
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

void CEvalView::Reset()
{
	CEvalView::SetWindowText(NULL);
	m_strPMark = "";
}

static int count_lines(LPCTSTR s)
{
	if (!s) return 0;
	int n = 0;
	for (; *s; s++) if (*s == 10) n++;
	return n;
}

static void limit_buffer(CEdit& edit, int nlines)
{
	int n = edit.GetLineCount() - nlines;
	if (n <= 0) return;
	int len = edit.GetWindowTextLength()+1;
	LPTSTR buf = new TCHAR[len+1];
	edit.GetWindowText(buf, len);
	LPTSTR s;
	for (s = buf; *s && n; s++)
		if (*s == 10) n--;
	edit.SetWindowText(s);
	delete[] buf;
}

void CEvalView::Append(LPCTSTR lpszText, BOOL mark)
{
	CEdit &edit = GetEditCtrl();
	if (CMainFrame::m_nLogMaxLines > 0)
		limit_buffer(edit, CMainFrame::m_nLogMaxLines -
			count_lines(lpszText));
	int len = edit.GetWindowTextLength();
	edit.SetSel(len, len);
	if (lpszText) {
		edit.ReplaceSel(lpszText);
		if (mark && lpszText) {
			LPCTSTR p = strrchr(lpszText, '\n');
			if (p)
				m_strPMark = p+1;
			else
				m_strPMark += lpszText;
		}
	}
}

void CEvalView::AppendNewLine(LPCTSTR lpszText,
							  BOOL mark)
{
	CEdit &edit = GetEditCtrl();
	int len = edit.GetWindowTextLength();
	int base = edit.LineIndex(edit.GetLineCount()-1);
	if (base < len)
		Append(_T("\r\n"), mark);
	if (lpszText)
		Append(lpszText, mark);
}

BOOL CEvalView::IsCmd()
{
	CEdit& edit = GetEditCtrl();
	return edit.LineIndex(edit.LineFromChar()+1) == -1;
}

void CEvalView::SetCmd(CString str)
{
	CEdit& edit = GetEditCtrl();
	int n1, n2 = GetWindowTextLength();
	int l = edit.GetLineCount();
	if (l <= 0) {
		Append(str);
		edit.SetSel(0, 0);
		return;
	} else if (m_strPMark.IsEmpty())
		n1 = edit.LineIndex(l-1);
	else {
		n1 = edit.LineIndex(l-1);
		CString line;
		PeekLine(l-1, line);
		l = strlen(m_strPMark);
		if (strncmp(m_strPMark, line, l) == 0)
			n1 += l;
	}
	int m1, m2;
	edit.GetSel(m1, m2);
	edit.SetSel(n1, n2);
	edit.ReplaceSel(str);
	// set cursor position
	if (m2 >= n1) {
		n2 = edit.GetWindowTextLength();
		if (m2 > n2) m2 = n2;
		else if (m2 < n1) m2 = n1;
		edit.SetSel(m2, m2);
	}
}

void CEvalView::PeekLine(int lineno, int len, CString& str,
						 BOOL remove_prompt)
{
	CEdit& edit = GetEditCtrl();
	if (edit.LineIndex(lineno) == -1) {
		str = "";
		return;
	}
	int ofs = 0;
	LPTSTR line = new TCHAR[len+4];
	ASSERT(line != NULL);
	edit.GetLine(lineno, line, len);
	line[len] = '\0';
	if (remove_prompt) {
		if (edit.LineIndex(lineno+1) < 0) {
			int l = strlen(m_strPMark);
			if (strncmp(m_strPMark, line, l) == 0)
				ofs = l;
		} else {
			if (strncmp(CMainFrame::m_strQPSX, line,
				strlen(CMainFrame::m_strQPSX)) == 0)
				ofs = strlen(CMainFrame::m_strQPSX);
			else if (strncmp(CMainFrame::m_strQPS2, line,
				strlen(CMainFrame::m_strQPS2)) == 0)
				ofs	= strlen(CMainFrame::m_strQPS2);
			else if (strncmp(CMainFrame::m_strDPS, line,
				strlen(CMainFrame::m_strDPS)) == 0)
				ofs	= strlen(CMainFrame::m_strDPS);
		}
	}
	str = line+ofs;
	delete[] line;	
}

void CEvalView::PeekLine(int lineno, CString& str,
						 BOOL remove_prompt)
{
	CEdit& edit = GetEditCtrl();
	int p = edit.LineIndex(lineno);
	if (p != -1) {
		int p2 = edit.LineIndex(lineno+1);
		if (p2 == -1)
			p2 = edit.GetWindowTextLength();
		else
			p2 -= 2; // CR/LF
		PeekLine(lineno, p2-p, str, remove_prompt);
	} else
		str = "";
}

void CEvalView::GotoLine(int lineno)
{
	CEdit& edit = GetEditCtrl();
	int p = edit.LineIndex(lineno);
	if (p != -1)
		edit.SetSel(p, p);
}

void CEvalView::MarkLine(int lineno)
{
	CEdit& edit = GetEditCtrl();
	int p = edit.LineIndex(lineno);
	if (p != -1) {
		int p2 = edit.LineIndex(lineno+1);
		if (p2 == -1)
			p2 = edit.GetWindowTextLength();
		else
			p2 -= 2; // CR/LF
		edit.SetSel(p2, p);
	}
}

static BOOL match_ws(LPCTSTR& p)
{
	while (*p && isspace(*p)) p++;
	return TRUE;
}

static BOOL match_num(LPCTSTR& p, int& num)
{
	LPCTSTR p1 = p;
	while (*p1 && isdigit(*p1)) p1++;
	if (p1 > p) {
		LPTSTR numstr = new TCHAR[p1-p+1];
		strncpy(numstr, p, p1-p);
		numstr[p1-p] = '\0';
		num = atoi(numstr);
		p = p1;
		delete[] numstr;
		return TRUE;
	} else
		return FALSE;
}

static BOOL match_fname(LPCTSTR& p, LPTSTR fname)
{
	LPCTSTR p1 = p;
	while (*p1 && *p1 != ',') p1++;
	if (p1 > p && fname) {
		strncpy(fname, p, p1-p);
		fname[p1-p] = '\0';
		p = p1;
		return TRUE;
	} else
		return FALSE;
}

static BOOL match(LPCTSTR s, LPCTSTR& p)
{
	if (strncmp(s, p, strlen(s)) == 0) {
		p += strlen(s);
		return TRUE;
	} else
		return FALSE;
}

static BOOL match_error(LPCTSTR line, LPTSTR fname, LPCTSTR& msg,
						int& lineno)
{
	LPCTSTR p = line;
#if 0
	LPCTSTR p1;
	int stacknum;
	match_ws(p); p1 = p;
	if (!((match("Error", p) || match("Warning", p))
		&& match_ws(p) ||
		match_num(p, stacknum) && match(">", p) && match_ws(p)))
		p = p1;
#endif
	if (!(match_fname(p, fname) && match(", line ", p) &&
		match_num(p, lineno) &&
		match(":", p) && match_ws(p) &&
		strcmp(fname, "<stdin>")))
		return FALSE;
	else {
		msg = p;
		return TRUE;
	}
}

BOOL CEvalView::GetErr(CString& fname, int& lineno, int &ref,
					   CString& msg)
{
	CString line;
	PeekLine(ref, line);
	LPCTSTR msgstr;
	LPTSTR fnamestr = new TCHAR[line.GetLength()+1];
	ASSERT(fnamestr != NULL);
	if (match_error(line, fnamestr, msgstr, lineno)) {
		msg = msgstr;
		fname = fnamestr;
		delete[] fnamestr;
		return TRUE;
	} else {
		delete[] fnamestr;
		return FALSE;
	}
}

BOOL CEvalView::GetFirstErr(CString& fname, int& lineno, int &ref,
							CString& msg, int ref0)
{
	CString fname1, msg1;
	int lineno1, ref1;
	BOOL res = FALSE;;
	CEdit& edit = GetEditCtrl();
	if (ref0 < 0) ref0 = edit.LineFromChar();
	for (ref1 = ref0; ref1 >= 0; ref1--)
		if (GetErr(fname1, lineno1, ref1, msg1)) {
			res = TRUE;
			fname = fname1; lineno = lineno1;
			ref = ref1; msg = msg1;
		} else
			return res;
	return res;
}

BOOL CEvalView::GetLastErr(CString& fname, int& lineno, int &ref,
						   CString& msg, int ref0)
{
	CString fname1, msg1;
	int lineno1, ref1;
	BOOL res = FALSE;;
	CEdit& edit = GetEditCtrl();
	if (ref0 < 0) ref0 = edit.LineFromChar();
	for (ref1 = ref0; ref1 >= 0 &&
		ref1 < edit.GetLineCount(); ref1++)
		if (GetErr(fname1, lineno1, ref1, msg1)) {
			res = TRUE;
			fname = fname1; lineno = lineno1;
			ref = ref1; msg = msg1;
		} else
			return res;
	return res;
}

BOOL CEvalView::GetPrevErr(CString& fname, int& lineno, int &ref,
						   CString& msg, int ref0)
{
	CEdit& edit = GetEditCtrl();
	if (ref0 < 0) ref0 = edit.LineFromChar();
	for (ref = ref0-1; ref >= 0; ref--)
		if (GetErr(fname, lineno, ref, msg))
			return TRUE;
	return FALSE;
}

BOOL CEvalView::GetNextErr(CString& fname, int& lineno, int &ref,
						   CString& msg, int ref0)
{
	CEdit& edit = GetEditCtrl();
	if (ref0 < 0) ref0 = edit.LineFromChar();
	for (ref = ref0+1; ref > 0 && ref < edit.GetLineCount(); ref++)
		if (GetErr(fname, lineno, ref, msg))
			return TRUE;
	return FALSE;
}

void CEvalView::OnChar(UINT nChar, UINT nRepCnt, UINT nFlags) 
{
	if (nChar == '\n' || nChar == '\r' && IsCmd()) {
		CEdit& edit = GetEditCtrl();
		// looks like we can assume this
		ASSERT(nRepCnt == 1);
		// get the current line
		edit.ReplaceSel("");
		int lineno = edit.LineFromChar();
		CString line;
		PeekLine(lineno, line, TRUE);
		if (edit.LineIndex(lineno+1) != -1)
			// copy the line down to the end of the buffer
			SetCmd(line);
		Append("\r\n");
		CQpadDoc* pDoc = GetDocument();
		// send the line to the interpreter process
		if (IsRunning()) {
			m_pipe.Write(line);
			m_pipe.Write("\n");
		}
		// add the line to the command history
		m_hist.Append(line);
		// reset the process marker
		m_strPMark = "";
	} else
		CEditView::OnChar(nChar, nRepCnt, nFlags);
}

// public attributes and operations

BOOL CEvalView::IsRunning()
{
	return m_pipe.IsRunning();
}


BOOL CEvalView::Run(LPCTSTR name, LPCTSTR pname)
{
	if (IsRunning())
		AppendNewLine();
	m_strPMark = "";
	return m_pipe.Run(name, pname);
}

BOOL CEvalView::Debug(LPCTSTR name, LPCTSTR pname)
{
	if (IsRunning())
		AppendNewLine();
	m_strPMark = "";
	return m_pipe.Debug(name, pname);
}

void CEvalView::Break()
{
	if (IsRunning()) {
		AppendNewLine();
		m_strPMark = "";
		m_pipe.Break();
	}
}

void CEvalView::Kill()
{
	if (IsRunning()) {
		AppendNewLine();
		m_strPMark = "";
		m_pipe.Kill();
	}
}

void CEvalView::ProcessInput()
{
	LPTSTR s = m_pipe.Read();
	Append(s, TRUE);
	delete[] s;
}

void CEvalView::OnViewFont() 
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
			CMainFrame::m_lfLogFont = lf;
		}
	}
}

void CEvalView::OnViewTabstops() 
{
	CTabStopDlg dlg;
	dlg.m_nTabStops = m_nTabStops/4;
	if (dlg.DoModal() == IDOK)
	{
		CWaitCursor wait;
		SetTabStops(dlg.m_nTabStops*4);
		CMainFrame::m_nLogTabStops = m_nTabStops;
	}
}

int CEvalView::OnCreate(LPCREATESTRUCT lpCreateStruct) 
{
	if (CEditView::OnCreate(lpCreateStruct) == -1)
		return -1;
	
	if (CMainFrame::m_lfLogFont.lfHeight != 0)
	{
		m_font.CreateFontIndirect(&CMainFrame::m_lfLogFont);
		SetFont(&m_font);
	}
	
	return 0;
}

void CEvalView::OnHistFirst() 
{
	CString str;
	if (IsCmd() && m_hist.GetFirst(str))
		SetCmd(str);
	else
		MessageBeep(MB_OK);
}

void CEvalView::OnHistLast() 
{
	CString str;
	if (IsCmd() && m_hist.GetLast(str))
		SetCmd(str);
	else
		MessageBeep(MB_OK);
}

void CEvalView::OnHistNext() 
{
	CString str;
	if (IsCmd() && m_hist.GetNext(str))
		SetCmd(str);
	else
		MessageBeep(MB_OK);
}

void CEvalView::OnHistPrev() 
{
	CString str;
	if (IsCmd() && m_hist.GetPrev(str))
		SetCmd(str);
	else
		MessageBeep(MB_OK);
}

void CEvalView::GetSearchStr(CString& str)
{
	CEdit& edit = GetEditCtrl();
	int lineno = edit.LineFromChar(), n = edit.LineIndex();
	int n1, n2;
	edit.GetSel(n1, n2);
	int len = n2-n;
	PeekLine(lineno, len, str, TRUE);
}

void CEvalView::OnHistSearchNext() 
{
	if (IsCmd() && m_hist.HasNext()) {
		CString str, search;
		GetSearchStr(search);
		if (m_hist.SearchNext(str, search))
			SetCmd(str);
		else
			MessageBeep(MB_OK);
	} else
		MessageBeep(MB_OK);
}

void CEvalView::OnHistSearchPrev() 
{
	if (IsCmd() && m_hist.HasPrev()) {
		CString str, search;
		GetSearchStr(search);
		if (m_hist.SearchPrev(str, search))
			SetCmd(str);
		else
			MessageBeep(MB_OK);
	} else
		MessageBeep(MB_OK);
}
