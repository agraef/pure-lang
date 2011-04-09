#if !defined(AFX_PROMPTDLG_H__0B0D0B20_B09B_11D3_AAB7_F4C9F573BE2D__INCLUDED_)
#define AFX_PROMPTDLG_H__0B0D0B20_B09B_11D3_AAB7_F4C9F573BE2D__INCLUDED_

#if _MSC_VER > 1000
#pragma once
#endif // _MSC_VER > 1000
// PromptDlg.h : Header-Datei
//

/////////////////////////////////////////////////////////////////////////////
// Dialogfeld CPromptDlg 

class CPromptDlg : public CDialog
{
// Konstruktion
public:
	CPromptDlg(CWnd* pParent = NULL);   // Standardkonstruktor

// Dialogfelddaten
	//{{AFX_DATA(CPromptDlg)
	enum { IDD = IDD_PROMPT };
	CString	m_strQPS;
	//}}AFX_DATA


// Überschreibungen
	// Vom Klassen-Assistenten generierte virtuelle Funktionsüberschreibungen
	//{{AFX_VIRTUAL(CPromptDlg)
	protected:
	virtual void DoDataExchange(CDataExchange* pDX);    // DDX/DDV-Unterstützung
	//}}AFX_VIRTUAL

// Implementierung
protected:

	// Generierte Nachrichtenzuordnungsfunktionen
	//{{AFX_MSG(CPromptDlg)
		// HINWEIS: Der Klassen-Assistent fügt hier Member-Funktionen ein
	//}}AFX_MSG
	DECLARE_MESSAGE_MAP()
};

//{{AFX_INSERT_LOCATION}}
// Microsoft Visual C++ fügt unmittelbar vor der vorhergehenden Zeile zusätzliche Deklarationen ein.

#endif // AFX_PROMPTDLG_H__0B0D0B20_B09B_11D3_AAB7_F4C9F573BE2D__INCLUDED_
