// HistfileDlg.cpp: Implementierungsdatei
//

#include "stdafx.h"
#include "qpad.h"
#include "HistfileDlg.h"

#ifdef _DEBUG
#define new DEBUG_NEW
#undef THIS_FILE
static char THIS_FILE[] = __FILE__;
#endif

/////////////////////////////////////////////////////////////////////////////
// Dialogfeld CHistfileDlg 


CHistfileDlg::CHistfileDlg(CWnd* pParent /*=NULL*/)
	: CDialog(CHistfileDlg::IDD, pParent)
{
	//{{AFX_DATA_INIT(CHistfileDlg)
	m_strHistFile = _T("");
	//}}AFX_DATA_INIT
}


void CHistfileDlg::DoDataExchange(CDataExchange* pDX)
{
	CDialog::DoDataExchange(pDX);
	//{{AFX_DATA_MAP(CHistfileDlg)
	DDX_Text(pDX, IDC_HISTFILE, m_strHistFile);
	//}}AFX_DATA_MAP
}


BEGIN_MESSAGE_MAP(CHistfileDlg, CDialog)
	//{{AFX_MSG_MAP(CHistfileDlg)
		// HINWEIS: Der Klassen-Assistent fügt hier Zuordnungsmakros für Nachrichten ein
	//}}AFX_MSG_MAP
END_MESSAGE_MAP()

/////////////////////////////////////////////////////////////////////////////
// Behandlungsroutinen für Nachrichten CHistfileDlg 
