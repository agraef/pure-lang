// TabStopDlg.cpp : implementation file
//

#include "stdafx.h"
#include "qpad.h"
#include "TabStopDlg.h"

#ifdef _DEBUG
#define new DEBUG_NEW
#undef THIS_FILE
static char THIS_FILE[] = __FILE__;
#endif

/////////////////////////////////////////////////////////////////////////////
// CTabStopDlg dialog


CTabStopDlg::CTabStopDlg(CWnd* pParent /*=NULL*/)
	: CDialog(CTabStopDlg::IDD, pParent)
{
	//{{AFX_DATA_INIT(CTabStopDlg)
	m_nTabStops = 8;
	//}}AFX_DATA_INIT
}


void CTabStopDlg::DoDataExchange(CDataExchange* pDX)
{
	CDialog::DoDataExchange(pDX);
	//{{AFX_DATA_MAP(CTabStopDlg)
	DDX_Text(pDX, IDC_TABSTOPS, m_nTabStops);
	//}}AFX_DATA_MAP
}


BEGIN_MESSAGE_MAP(CTabStopDlg, CDialog)
	//{{AFX_MSG_MAP(CTabStopDlg)
	//}}AFX_MSG_MAP
END_MESSAGE_MAP()

/////////////////////////////////////////////////////////////////////////////
// CTabStopDlg message handlers
