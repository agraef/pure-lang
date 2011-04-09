// qpad.cpp : Defines the class behaviors for the application.
//

#include "stdafx.h"
#include "qpad.h"
#include "pipe.h"

#include "MainFrm.h"
#include "qpadDoc.h"

#ifdef _DEBUG
#define new DEBUG_NEW
#undef THIS_FILE
static char THIS_FILE[] = __FILE__;
#endif

/////////////////////////////////////////////////////////////////////////////
// CQpadApp

BEGIN_MESSAGE_MAP(CQpadApp, CWinApp)
	//{{AFX_MSG_MAP(CQpadApp)
	ON_COMMAND(ID_APP_ABOUT, OnAppAbout)
	ON_COMMAND(ID_HELP_FINDER, OnHelpFinder)
	//}}AFX_MSG_MAP
	// Standard file based document commands
	ON_COMMAND(ID_FILE_NEW, CWinApp::OnFileNew)
	ON_COMMAND(ID_FILE_OPEN, CWinApp::OnFileOpen)
	// Standard print setup command
	ON_COMMAND(ID_FILE_PRINT_SETUP, CWinApp::OnFilePrintSetup)
END_MESSAGE_MAP()

/////////////////////////////////////////////////////////////////////////////
// CQpadApp construction

CQpadApp::CQpadApp()
{
	// TODO: add construction code here,
	// Place all significant initialization in InitInstance
}

/////////////////////////////////////////////////////////////////////////////
// The one and only CQpadApp object

CQpadApp theApp;

/////////////////////////////////////////////////////////////////////////////
// CQpadApp initialization

BOOL CQpadApp::InitInstance()
{
	AfxEnableControlContainer();

	// Standard initialization
	// If you are not using these features and wish to reduce the size
	//  of your final executable, you should remove from the following
	//  the specific initialization routines you do not need.

#ifdef _AFXDLL
	Enable3dControls();			// Call this when using MFC in a shared DLL
#else
	Enable3dControlsStatic();	// Call this when linking to MFC statically
#endif

	// Change the registry key under which our settings are stored.
	// TODO: You should modify this string to be something appropriate
	// such as the name of your company or organization.
	SetRegistryKey(_T("Pure Software"));

	// Get our settings

	LoadStdProfileSettings(8);  // Load standard INI file options (including MRU)
	CMainFrame::Initialize();

	// Register the application's document templates.  Document templates
	//  serve as the connection between documents, frame windows and views.

	CSingleDocTemplate* pDocTemplate;
	pDocTemplate = new CSingleDocTemplate(
		IDR_MAINFRAME,
		RUNTIME_CLASS(CQpadDoc),
		RUNTIME_CLASS(CMainFrame),       // main SDI frame window
		RUNTIME_CLASS(CEvalView));
	AddDocTemplate(pDocTemplate);

	// Enable DDE Execute open
	EnableShellOpen();
	RegisterShellFileTypes(TRUE);

	// Parse command line for standard shell commands, DDE, file open
	CCommandLineInfo cmdInfo;
	ParseCommandLine(cmdInfo);

	// Dispatch commands specified on the command line
	if (!ProcessShellCommand(cmdInfo))
		return FALSE;

	// The one and only window has been initialized, so show and update it.
//	m_pMainWnd->ShowWindow(SW_SHOW);
	((CMainFrame*)m_pMainWnd)->InitialShowWindow(SW_SHOW);
	m_pMainWnd->UpdateWindow();

	// Enable drag/drop open
	m_pMainWnd->DragAcceptFiles();

	return TRUE;
}

int CQpadApp::ExitInstance() 
{
	CMainFrame::Terminate();	
	return CWinApp::ExitInstance();
}

/////////////////////////////////////////////////////////////////////////////
// CAboutDlg dialog used for App About

class CAboutDlg : public CDialog
{
public:
	CAboutDlg();

// Dialog Data
	//{{AFX_DATA(CAboutDlg)
	enum { IDD = IDD_ABOUTBOX };
	//}}AFX_DATA

	// ClassWizard generated virtual function overrides
	//{{AFX_VIRTUAL(CAboutDlg)
	protected:
	virtual void DoDataExchange(CDataExchange* pDX);    // DDX/DDV support
	//}}AFX_VIRTUAL

// Implementation
protected:
	//{{AFX_MSG(CAboutDlg)
		// No message handlers
	//}}AFX_MSG
	DECLARE_MESSAGE_MAP()
};

CAboutDlg::CAboutDlg() : CDialog(CAboutDlg::IDD)
{
	//{{AFX_DATA_INIT(CAboutDlg)
	//}}AFX_DATA_INIT
}

void CAboutDlg::DoDataExchange(CDataExchange* pDX)
{
	CDialog::DoDataExchange(pDX);
	//{{AFX_DATA_MAP(CAboutDlg)
	//}}AFX_DATA_MAP
}

BEGIN_MESSAGE_MAP(CAboutDlg, CDialog)
	//{{AFX_MSG_MAP(CAboutDlg)
		// No message handlers
	//}}AFX_MSG_MAP
END_MESSAGE_MAP()

// App command to run the dialog
void CQpadApp::OnAppAbout()
{
	CAboutDlg aboutDlg;
	aboutDlg.DoModal();
}

void CQpadApp::OnHelpFinder() 
{
	HtmlHelp(NULL, CMainFrame::m_strAppPath+"\\puredoc.chm",
		HH_DISPLAY_TOPIC, 0);	
}

/////////////////////////////////////////////////////////////////////////////
// CQpadApp message handlers

BOOL CQpadApp::OnIdle(LONG lCount) 
{
	CMainFrame* pFrame = (CMainFrame*) AfxGetMainWnd();
	CStatusBar* pStatusBar = (CStatusBar*) pFrame->GetDescendantWindow(AFX_IDW_STATUS_BAR);

	if(pStatusBar)
	{
		CEdit &edit = ((CEditView*)pFrame->GetActiveView())->GetEditCtrl();
		CString s1;
		UINT i = edit.LineFromChar();
		CString ind;
		ind.LoadString(ID_INDICATOR_LINE);
		int p = ind.Find(' ');
		s1.Format(_T("%s %u"), ind.Left(p), ++i);
		pStatusBar->SetPaneText(pStatusBar->CommandToIndex(ID_INDICATOR_LINE), s1);
	}

	pFrame->UpdateTitle();

	return CWinApp::OnIdle(lCount);
}

