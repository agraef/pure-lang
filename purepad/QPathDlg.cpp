// QPathDlg.cpp : implementation file
//

#include "stdafx.h"
#include "qpad.h"
#include "QPathDlg.h"

#ifdef _DEBUG
#define new DEBUG_NEW
#undef THIS_FILE
static char THIS_FILE[] = __FILE__;
#endif

/////////////////////////////////////////////////////////////////////////////
// CQPathDlg dialog


CQPathDlg::CQPathDlg(CWnd* pParent /*=NULL*/)
	: CDialog(CQPathDlg::IDD, pParent)
{
	//{{AFX_DATA_INIT(CQPathDlg)
	m_strPath = _T("");
	//}}AFX_DATA_INIT
}


void CQPathDlg::DoDataExchange(CDataExchange* pDX)
{
	CDialog::DoDataExchange(pDX);
	//{{AFX_DATA_MAP(CQPathDlg)
	DDX_Text(pDX, IDC_QPATH, m_strPath);
	//}}AFX_DATA_MAP
}


BEGIN_MESSAGE_MAP(CQPathDlg, CDialog)
	//{{AFX_MSG_MAP(CQPathDlg)
		// NOTE: the ClassWizard will add message map macros here
	//}}AFX_MSG_MAP
END_MESSAGE_MAP()

/////////////////////////////////////////////////////////////////////////////
// CQPathDlg message handlers


