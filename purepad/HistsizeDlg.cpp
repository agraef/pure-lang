// HistsizeDlg.cpp: Implementierungsdatei
//

#include "stdafx.h"
#include "qpad.h"
#include "HistsizeDlg.h"

#ifdef _DEBUG
#define new DEBUG_NEW
#undef THIS_FILE
static char THIS_FILE[] = __FILE__;
#endif

/////////////////////////////////////////////////////////////////////////////
// Dialogfeld CHistsizeDlg 


CHistsizeDlg::CHistsizeDlg(CWnd* pParent /*=NULL*/)
	: CDialog(CHistsizeDlg::IDD, pParent)
{
	//{{AFX_DATA_INIT(CHistsizeDlg)
	m_nHistSize = 0;
	//}}AFX_DATA_INIT
}


void CHistsizeDlg::DoDataExchange(CDataExchange* pDX)
{
	CDialog::DoDataExchange(pDX);
	//{{AFX_DATA_MAP(CHistsizeDlg)
	DDX_Text(pDX, IDC_HISTSIZE, m_nHistSize);
	DDV_MinMaxInt(pDX, m_nHistSize, 0, 10000);
	//}}AFX_DATA_MAP
}


BEGIN_MESSAGE_MAP(CHistsizeDlg, CDialog)
	//{{AFX_MSG_MAP(CHistsizeDlg)
		// HINWEIS: Der Klassen-Assistent fügt hier Zuordnungsmakros für Nachrichten ein
	//}}AFX_MSG_MAP
END_MESSAGE_MAP()

/////////////////////////////////////////////////////////////////////////////
// Behandlungsroutinen für Nachrichten CHistsizeDlg 
