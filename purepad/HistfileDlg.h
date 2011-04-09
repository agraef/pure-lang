#if !defined(AFX_HISTFILEDLG_H__0B0D0B21_B09B_11D3_AAB7_F4C9F573BE2D__INCLUDED_)
#define AFX_HISTFILEDLG_H__0B0D0B21_B09B_11D3_AAB7_F4C9F573BE2D__INCLUDED_

#if _MSC_VER > 1000
#pragma once
#endif // _MSC_VER > 1000
// HistfileDlg.h : Header-Datei
//

/////////////////////////////////////////////////////////////////////////////
// Dialogfeld CHistfileDlg 

class CHistfileDlg : public CDialog
{
// Konstruktion
public:
	CHistfileDlg(CWnd* pParent = NULL);   // Standardkonstruktor

// Dialogfelddaten
	//{{AFX_DATA(CHistfileDlg)
	enum { IDD = IDD_HISTFILE };
	CString	m_strHistFile;
	//}}AFX_DATA


// Überschreibungen
	// Vom Klassen-Assistenten generierte virtuelle Funktionsüberschreibungen
	//{{AFX_VIRTUAL(CHistfileDlg)
	protected:
	virtual void DoDataExchange(CDataExchange* pDX);    // DDX/DDV-Unterstützung
	//}}AFX_VIRTUAL

// Implementierung
protected:

	// Generierte Nachrichtenzuordnungsfunktionen
	//{{AFX_MSG(CHistfileDlg)
		// HINWEIS: Der Klassen-Assistent fügt hier Member-Funktionen ein
	//}}AFX_MSG
	DECLARE_MESSAGE_MAP()
};

//{{AFX_INSERT_LOCATION}}
// Microsoft Visual C++ fügt unmittelbar vor der vorhergehenden Zeile zusätzliche Deklarationen ein.

#endif // AFX_HISTFILEDLG_H__0B0D0B21_B09B_11D3_AAB7_F4C9F573BE2D__INCLUDED_
