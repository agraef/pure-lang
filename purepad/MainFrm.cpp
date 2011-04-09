// MainFrm.cpp : implementation of the CMainFrame class
//

#include "stdafx.h"
#include "qpad.h"

#include "MainFrm.h"
#include "EvalView.h"
#include "QpadView.h"
#include "QpadDoc.h"
#include "QPathDlg.h"
#include "PromptDlg.h"
#include "HistfileDlg.h"
#include "HistsizeDlg.h"

#include <direct.h>

#ifdef _DEBUG
#define new DEBUG_NEW
#undef THIS_FILE
static char THIS_FILE[] = __FILE__;
#endif

/////////////////////////////////////////////////////////////////////////////
// CMainFrame

IMPLEMENT_DYNCREATE(CMainFrame, CFrameWnd)

BEGIN_MESSAGE_MAP(CMainFrame, CFrameWnd)
	//{{AFX_MSG_MAP(CMainFrame)
	ON_WM_CREATE()
	ON_WM_CLOSE()
	ON_COMMAND(ID_SCRIPT_RUN, OnScriptRun)
	ON_COMMAND(ID_SCRIPT_DEBUG, OnScriptDebug)
	ON_COMMAND(ID_SCRIPT_BREAK, OnScriptBreak)
	ON_COMMAND(ID_SCRIPT_KILL, OnScriptKill)
	ON_UPDATE_COMMAND_UI(ID_SCRIPT_BREAK, OnUpdateScriptBreak)
	ON_UPDATE_COMMAND_UI(ID_SCRIPT_KILL, OnUpdateScriptKill)
	ON_COMMAND(ID_SCRIPT_RESET, OnScriptReset)
	ON_UPDATE_COMMAND_UI(ID_SCRIPT_RESET, OnUpdateScriptReset)
	ON_COMMAND(ID_NEXT_ERR, OnNextErr)
	ON_UPDATE_COMMAND_UI(ID_NEXT_ERR, OnUpdateNextErr)
	ON_COMMAND(ID_PREV_ERR, OnPrevErr)
	ON_UPDATE_COMMAND_UI(ID_PREV_ERR, OnUpdatePrevErr)
	ON_COMMAND(ID_FIRST_ERR, OnFirstErr)
	ON_UPDATE_COMMAND_UI(ID_FIRST_ERR, OnUpdateFirstErr)
	ON_COMMAND(ID_LAST_ERR, OnLastErr)
	ON_UPDATE_COMMAND_UI(ID_LAST_ERR, OnUpdateLastErr)
	ON_COMMAND(ID_INPUT_LINE, OnInputLine)
	ON_UPDATE_COMMAND_UI(ID_INPUT_LINE, OnUpdateInputLine)
	ON_COMMAND(ID_SCRIPT_OPEN, OnScriptOpen)
	ON_UPDATE_COMMAND_UI(ID_SCRIPT_OPEN, OnUpdateScriptOpen)
	ON_COMMAND(ID_SCRIPT_PATH, OnScriptPath)
	ON_COMMAND(ID_SCRIPT_PROMPT, OnScriptPrompt)
	ON_COMMAND(ID_SCRIPT_HISTFILE, OnScriptHistfile)
	ON_MESSAGE(WM_USER_INPUT, OnInput)
	ON_MESSAGE(WM_USER_COMPILE, OnCompile)
	ON_COMMAND(ID_SCRIPT_HISTSIZE, OnScriptHistsize)
	//}}AFX_MSG_MAP
	ON_UPDATE_COMMAND_UI(ID_INDICATOR_LINE, OnUpdateLineNumber)
END_MESSAGE_MAP()

static UINT indicators[] =
{
	ID_SEPARATOR,           // status line indicator
	ID_INDICATOR_LINE,
	ID_INDICATOR_CAPS,
	ID_INDICATOR_NUM,
	ID_INDICATOR_SCRL,
};

BOOL CMainFrame::m_bLogReset;
int CMainFrame::m_nTabStops, CMainFrame::m_nLogTabStops;
LOGFONT CMainFrame::m_lfFont, CMainFrame::m_lfLogFont;
int CMainFrame::m_nHistSize, CMainFrame::m_nLogMaxLines;
CString CMainFrame::m_strHistFile;
#if 0
CString CMainFrame::m_strCompileCommand;
#endif
CString CMainFrame::m_strRunCommand;
CString CMainFrame::m_strDebugCommand;
CString CMainFrame::m_strAppPath;
CString CMainFrame::m_strVersion;
CString CMainFrame::m_strQPATH;
CString CMainFrame::m_strQPS;
CString CMainFrame::m_strQPSX;
CString CMainFrame::m_strQPS2 = _T("> ");
CString CMainFrame::m_strDPS = _T(": ");

/////////////////////////////////////////////////////////////////////////////
// CMainFrame construction/destruction

CMainFrame::CMainFrame()
{
	// TODO: add member initialization code here
}

CMainFrame::~CMainFrame()
{
}

static TCHAR BASED_CODE szSetup[] = _T("Software\\Microsoft\\Windows\\CurrentVersion\\App Paths\\purepad.exe");
static TCHAR BASED_CODE szPath[] = _T("Path");

#if 0
static TCHAR BASED_CODE szTclSetup[] = _T("Software\\ActiveState\\ActiveTcl");
static TCHAR BASED_CODE szCurrentVersion[] = _T("CurrentVersion");
#endif

static TCHAR BASED_CODE szSettings[] = _T("Settings");

static TCHAR BASED_CODE szVersion[] = _T("Version");

static TCHAR BASED_CODE szLogReset[] = _T("LogReset");
static TCHAR BASED_CODE szPaneSplit[] = _T("PaneSplit");
static TCHAR BASED_CODE szTabStops[] = _T("TabStops");
static TCHAR BASED_CODE szLogTabStops[] = _T("LogTabStops");
static TCHAR BASED_CODE szHistSize[] = _T("HistSize");
static TCHAR BASED_CODE szHistFile[] = _T("HistFile");
static TCHAR BASED_CODE szLogMaxLines[] = _T("LogMaxLines");

#if 0
static TCHAR BASED_CODE szCompileCommand[] = _T("CompileCommand");
#endif
static TCHAR BASED_CODE szRunCommand[] = _T("RunCommand");
static TCHAR BASED_CODE szDebugCommand[] = _T("DebugCommand");
static TCHAR BASED_CODE szAppPath[] = _T("AppPath");
#if 0
static TCHAR BASED_CODE szQPATH[] = _T("QPATH");
#endif
static TCHAR BASED_CODE szQPS[] = _T("PURE_PS");

static TCHAR BASED_CODE szToolbars[] = _T("Toolbars");
static TCHAR BASED_CODE szWindowPos[] = _T("WindowPos");
static TCHAR BASED_CODE szFont[] = _T("Font");
static TCHAR BASED_CODE szLogFont[] = _T("LogFont");
static TCHAR BASED_CODE szHeight[] = _T("Height");
static TCHAR BASED_CODE szWeight[] = _T("Weight");
static TCHAR BASED_CODE szItalic[] = _T("Italic");
static TCHAR BASED_CODE szUnderline[] = _T("Underline");
static TCHAR BASED_CODE szPitchAndFamily[] = _T("PitchAndFamily");
static TCHAR BASED_CODE szCharSet[] = _T("CharSet");
static TCHAR BASED_CODE szFaceName[] = _T("FaceName");
static TCHAR BASED_CODE szSystem[] = _T("System");

static void GetProfileFont(LPCTSTR szSec, LOGFONT* plf)
{
	CWinApp* pApp = AfxGetApp();
	plf->lfHeight = pApp->GetProfileInt(szSec, szHeight, 0);
	if (plf->lfHeight != 0)
	{
		plf->lfWeight = pApp->GetProfileInt(szSec, szWeight, 0);
		plf->lfItalic = (BYTE)pApp->GetProfileInt(szSec, szItalic, 0);
		plf->lfUnderline = (BYTE)pApp->GetProfileInt(szSec, szUnderline, 0);
		plf->lfPitchAndFamily = (BYTE)pApp->GetProfileInt(szSec, szPitchAndFamily, 0);
		plf->lfCharSet = (BYTE)pApp->GetProfileInt(szSec, szCharSet, DEFAULT_CHARSET);
		CString strFont = pApp->GetProfileString(szSec, szFaceName, szSystem);
		lstrcpyn((TCHAR*)plf->lfFaceName, strFont, sizeof plf->lfFaceName);
		plf->lfFaceName[sizeof plf->lfFaceName-1] = 0;
	}
}

static void WriteProfileFont(LPCTSTR szSec, const LOGFONT* plf)
{
	CWinApp* pApp = AfxGetApp();

	pApp->WriteProfileInt(szSec, szHeight, plf->lfHeight);
	if (plf->lfHeight != 0)
	{
		pApp->WriteProfileInt(szSec, szHeight, plf->lfHeight);
		pApp->WriteProfileInt(szSec, szWeight, plf->lfWeight);
		pApp->WriteProfileInt(szSec, szItalic, plf->lfItalic);
		pApp->WriteProfileInt(szSec, szUnderline, plf->lfUnderline);
		pApp->WriteProfileInt(szSec, szPitchAndFamily, plf->lfPitchAndFamily);
		pApp->WriteProfileInt(szSec, szCharSet, plf->lfCharSet);
		pApp->WriteProfileString(szSec, szFaceName, (LPCTSTR)plf->lfFaceName);
	}
}

static TCHAR szFormat[] = _T("%u,%u,%d,%d,%d,%d,%d,%d,%d,%d");

static BOOL ReadWindowPlacement(LPWINDOWPLACEMENT pwp)
{
	CString strBuffer = AfxGetApp()->GetProfileString(
		szSettings, szWindowPos);
	if (strBuffer.IsEmpty())
		return FALSE;

	WINDOWPLACEMENT wp;
	int nRead = _stscanf(strBuffer, szFormat,
		&wp.flags, &wp.showCmd,
		&wp.ptMinPosition.x, &wp.ptMinPosition.y,
		&wp.ptMaxPosition.x, &wp.ptMaxPosition.y,
		&wp.rcNormalPosition.left, &wp.rcNormalPosition.top,
		&wp.rcNormalPosition.right, &wp.rcNormalPosition.bottom);

	if (nRead != 10)
		return FALSE;

	wp.length = sizeof wp;
	*pwp = wp;
	return TRUE;
}

static void WriteWindowPlacement(LPWINDOWPLACEMENT pwp)
	// write a window placement to settings section of app's ini file
{
	TCHAR szBuffer[sizeof("-32767")*8 + sizeof("65535")*2];

	wsprintf(szBuffer, szFormat,
		pwp->flags, pwp->showCmd,
		pwp->ptMinPosition.x, pwp->ptMinPosition.y,
		pwp->ptMaxPosition.x, pwp->ptMaxPosition.y,
		pwp->rcNormalPosition.left, pwp->rcNormalPosition.top,
		pwp->rcNormalPosition.right, pwp->rcNormalPosition.bottom);
	AfxGetApp()->WriteProfileString(szSettings, szWindowPos,
		szBuffer);
}

static void GetAppPath(CString& app_path)
{
	app_path = "";
	HKEY hSetup;
	DWORD dwType, dwSize;
	if (RegOpenKeyEx(HKEY_LOCAL_MACHINE, szSetup,
		0, KEY_READ, &hSetup) == ERROR_SUCCESS &&
		RegQueryValueEx(hSetup, szPath, NULL, &dwType, NULL,
			&dwSize) == ERROR_SUCCESS && dwType == REG_SZ) {
		unsigned char *path = new unsigned char[dwSize];
		if (RegQueryValueEx(hSetup, "Path", NULL, &dwType, path,
			&dwSize) == ERROR_SUCCESS) {
			app_path = path;
			delete[] path;
		}
		RegCloseKey(hSetup);
	}
}

#if 0
static void GetTclPathAndVersion(CString& tcl_path, CString& tcl_version)
{
	tcl_path = "";
	tcl_version = "";
	HKEY hTclSetup, hTclVersion;
	DWORD dwType, dwSize;
	if (RegOpenKeyEx(HKEY_LOCAL_MACHINE, szTclSetup,
		0, KEY_READ, &hTclSetup) == ERROR_SUCCESS &&
		RegQueryValueEx(hTclSetup, szCurrentVersion, NULL, &dwType, NULL,
			&dwSize) == ERROR_SUCCESS && dwType == REG_SZ) {
		unsigned char *version = new unsigned char[dwSize];
		if (RegQueryValueEx(hTclSetup, szCurrentVersion, NULL, &dwType, version,
			&dwSize) == ERROR_SUCCESS) {
			tcl_version = version;
			delete[] version;
			CString key;
			key.Format("%s\\%s", szTclSetup, tcl_version);
			if (RegOpenKeyEx(HKEY_LOCAL_MACHINE, (LPCTSTR)key,
				0, KEY_READ, &hTclVersion) == ERROR_SUCCESS &&
				RegQueryValueEx(hTclVersion, _T(""), NULL, &dwType, NULL,
				&dwSize) == ERROR_SUCCESS && dwType == REG_SZ) {
				unsigned char *path = new unsigned char[dwSize];
				if (RegQueryValueEx(hTclVersion, _T(""), NULL, &dwType, path,
					&dwSize) == ERROR_SUCCESS) {
					tcl_path = path;
					delete[] path;
				}
				RegCloseKey(hTclVersion);

			}
		}
		RegCloseKey(hTclSetup);
	}
}
#endif

static CString LastLine(CString str)
{
	int pos = str.ReverseFind('\n');
	if (pos >= 0)
		return str.Right(str.GetLength()-pos-1);
	else
		return str;
}

void CMainFrame::Initialize()
{
	// get our settings
	m_strVersion = AfxGetApp()->GetProfileString(szSettings,
		szVersion, "");
	m_bLogReset = AfxGetApp()->GetProfileInt(szSettings,
		szLogReset, FALSE);
	m_nTabStops = AfxGetApp()->GetProfileInt(szSettings,
		szTabStops, 8*4);
	m_nLogTabStops = AfxGetApp()->GetProfileInt(szSettings,
		szLogTabStops, 8*4);
	m_nHistSize = AfxGetApp()->GetProfileInt(szSettings,
		szHistSize, 100);
	m_nLogMaxLines = AfxGetApp()->GetProfileInt(szSettings,
		szLogMaxLines, 500);
#if 0
	m_strHistFile = AfxGetApp()->GetProfileString(szSettings,
		szHistFile, "PurePadHistory");
#else
	char *home = getenv("HOME");
	if (home) {
		char *p = strchr(home, '=');
		if (p) home = p+1;
		m_strHistFile = AfxGetApp()->GetProfileString(szSettings,
			szHistFile, CString(home) + "\\PurePadHistory");
	} else
		m_strHistFile = AfxGetApp()->GetProfileString(szSettings,
			szHistFile, "PurePadHistory");
#endif
	if (m_strVersion == "1.1") {
		m_strQPS = AfxGetApp()->GetProfileString(szSettings,
			szQPS, "> ");
#if 0
		m_strCompileCommand = AfxGetApp()->GetProfileString(szSettings,
			szCompileCommand, "qc -n -o %s %s");
#endif
		m_strRunCommand = AfxGetApp()->GetProfileString(szSettings,
			szRunCommand, "pure -i -q %s");
		m_strDebugCommand = AfxGetApp()->GetProfileString(szSettings,
			szDebugCommand, "pure -i -q -g %s");
	} else {
		// update registry entries to current version
		m_strQPS = "> ";
#if 0
		m_strCompileCommand = "qc -n -o %s %s";
#endif
		m_strRunCommand = "pure -i -q %s";
		m_strDebugCommand = "pure -i -q -g %s";
	}
	CMainFrame::m_strQPSX = LastLine(m_strQPS);
	m_strVersion = "1.1";
	CString	app_path;
	GetAppPath(app_path);
	m_strAppPath = AfxGetApp()->GetProfileString(szSettings,
		szAppPath, app_path);
	CString	tcl_path, tcl_version;
#if 0
	GetTclPathAndVersion(tcl_path, tcl_version);
#endif
	if (app_path.IsEmpty()) {
		// no install yet; assume help path of application
		// (should be same location as executable)
		char drive[_MAX_DRIVE], dir[_MAX_DIR], fname[_MAX_FNAME],
			ext[_MAX_EXT], path[_MAX_PATH];
		_splitpath(AfxGetApp()->m_pszHelpFilePath, drive, dir, fname, ext);
		strcat(strcpy(path, drive), dir);
		int l = strlen(path);
		if (l > 1 && (path[l-1] == '\\' || path[l-1] == '/'))
			path[l-1] = 0;
		app_path = _T(path);
	}
#if 0
	// NOTE: As of version 4.6, the directory layout has changed:
	// The executable is now in the bin subdir. We want the actual
	// app_path to be the Qpad dir, so move one level up.
	if (!app_path.IsEmpty()) {
		char drive[_MAX_DRIVE], dir[_MAX_DIR], fname[_MAX_FNAME],
			ext[_MAX_EXT], path[_MAX_PATH];
		_splitpath(app_path, drive, dir, fname, ext);
		strcat(strcpy(path, drive), dir);
		int l = strlen(path);
		if (l > 1 && (path[l-1] == '\\' || path[l-1] == '/'))
			path[l-1] = 0;
		app_path = _T(path);
	}
#endif
	CString def_qpath = ".";
	if (!app_path.IsEmpty())
		def_qpath.Format(".;%s\\lib", app_path);
	if (m_strAppPath != app_path) {
		// looks like Qpad has been reinstalled in a different
		// location; reset QPATH to its default
		//AfxMessageBox(IDS_WARN_QPATH);
#if 0
		m_strQPATH = def_qpath;
#endif
		m_strAppPath = app_path;
	}
#if 0
	else
		m_strQPATH = AfxGetApp()->GetProfileString(szSettings,
			szQPATH, def_qpath);
#else
	char *purelib = getenv("PURELIB");
	if (purelib) {
		char *p = strchr(purelib, '=');
		if (p) purelib = p+1;
	}
#endif
	if (!purelib) purelib = "/progra~1/pure/lib";
	m_strQPATH = purelib;
	GetProfileFont(szFont, &m_lfFont);
	if (m_lfFont.lfHeight == 0)
		::GetObject(GetStockObject(SYSTEM_FIXED_FONT),
			sizeof(LOGFONT), &m_lfFont);
	GetProfileFont(szLogFont, &m_lfLogFont);
	if (m_lfLogFont.lfHeight == 0)
		::GetObject(GetStockObject(SYSTEM_FIXED_FONT),
			sizeof(LOGFONT), &m_lfLogFont);
#if 0
	CString set_qpath;
	set_qpath.Format("QPATH=%s", m_strQPATH);
	if (_putenv(set_qpath))
		AfxMessageBox(IDS_ENV_FULL);
	// add application and tcl path to PATH so that the interpreter
	// can find the compiler and the tcl dlls
	// FIXME: we should only do this when necessary
	char *path = getenv("PATH");
	CString set_path = path?path:".";
	if (!tcl_path.IsEmpty())
		set_path = tcl_path+"\\bin;"+set_path;
	if (!m_strAppPath.IsEmpty())
		set_path = m_strAppPath+";"+set_path;
	if (set_path != path) {
		set_path = "PATH="+set_path;
		if (_putenv(set_path))
			AfxMessageBox(IDS_ENV_FULL);
	}
	// set TCL_LIBRARY and TK_LIBRARY so that Tcl/Tk can find its
	// support files
	// FIXME: we should only do this when necessary
	if (!tcl_path.IsEmpty()) {
		if (!tcl_version.IsEmpty()) {
			int p = tcl_version.Find('.');
			if (p>=0) {
				int q = tcl_version.Find('.', p+1);
				if (q>=0) p=q;
			}
			if (p>=0) tcl_version = tcl_version.Left(p);
		}
		CString set_lib;
		set_lib.Format("TCL_LIBRARY=%s\\lib\\tcl%s", tcl_path, tcl_version);
		_putenv(set_lib);
		set_lib.Format("TK_LIBRARY=%s\\lib\\tk%s", tcl_path, tcl_version);
		_putenv(set_lib);
	}
	// set GGI_CONFDIR variable if necessary
	if (!getenv("GGI_CONFDIR")) {
		CString set_conf;
		set_conf.Format("GGI_CONFDIR=%s\\ggi\\etc\\ggi", app_path);
		_putenv(set_conf);
	}
#endif
}

void CMainFrame::Terminate()
{
	// save our settings
	AfxGetApp()->WriteProfileString(szSettings,
		szVersion, m_strVersion);
	AfxGetApp()->WriteProfileInt(szSettings,
		szLogReset, m_bLogReset);
	AfxGetApp()->WriteProfileInt(szSettings,
		szTabStops, m_nTabStops);
	AfxGetApp()->WriteProfileInt(szSettings,
		szLogTabStops, m_nLogTabStops);
	AfxGetApp()->WriteProfileInt(szSettings,
		szLogMaxLines, m_nLogMaxLines);
	AfxGetApp()->WriteProfileInt(szSettings,
		szHistSize, m_nHistSize);
	AfxGetApp()->WriteProfileString(szSettings,
		szHistFile, m_strHistFile);
	AfxGetApp()->WriteProfileString(szSettings,
		szQPS, m_strQPS);
#if 0
	AfxGetApp()->WriteProfileString(szSettings,
		szCompileCommand, m_strCompileCommand);
#endif
	AfxGetApp()->WriteProfileString(szSettings,
		szRunCommand, m_strRunCommand);
#if 0
	AfxGetApp()->WriteProfileString(szSettings,
		szDebugCommand, m_strDebugCommand);
	AfxGetApp()->WriteProfileString(szSettings,
		szQPATH, m_strQPATH);
#endif
	AfxGetApp()->WriteProfileString(szSettings,
		szAppPath, m_strAppPath);
	WriteProfileFont(szFont, &m_lfFont);
	WriteProfileFont(szLogFont, &m_lfLogFont);
}

int CMainFrame::OnCreate(LPCREATESTRUCT lpCreateStruct)
{
	if (CFrameWnd::OnCreate(lpCreateStruct) == -1)
		return -1;
	
	if (!m_wndToolBar.CreateEx(this, TBSTYLE_FLAT, WS_CHILD | WS_VISIBLE | CBRS_TOP
		| CBRS_GRIPPER | CBRS_TOOLTIPS | CBRS_FLYBY | CBRS_SIZE_DYNAMIC) ||
		!m_wndToolBar.LoadToolBar(IDR_MAINFRAME))
	{
		TRACE0("Failed to create toolbar\n");
		return -1;      // fail to create
	}

	if (!m_wndStatusBar.Create(this) ||
		!m_wndStatusBar.SetIndicators(indicators,
		  sizeof(indicators)/sizeof(UINT)))
	{
		TRACE0("Failed to create status bar\n");
		return -1;      // fail to create
	}

	// TODO: Delete these three lines if you don't want the toolbar to
	//  be dockable
	m_wndToolBar.EnableDocking(CBRS_ALIGN_ANY);
	EnableDocking(CBRS_ALIGN_ANY);
	DockControlBar(&m_wndToolBar);
	CString tb_title;
	tb_title.LoadString(IDS_TOOLBAR_TITLE);
	m_wndToolBar.SetWindowText(tb_title);

	LoadBarState(szToolbars);
	WINDOWPLACEMENT wp;
	if (ReadWindowPlacement(&wp)) {
		wp.showCmd = SW_SHOWNORMAL;
		SetWindowPlacement(&wp);
	}

	return 0;
}

void CMainFrame::OnClose() 
{
	GetEvalView()->Kill();
	int cyCur, cyMin;
	m_wndSplitter.GetRowInfo(0, cyCur, cyMin);
	AfxGetApp()->WriteProfileInt(szSettings, szPaneSplit,
		cyCur);
	SaveBarState(szToolbars);
	WINDOWPLACEMENT wp;
	wp.length = sizeof wp;
	if (GetWindowPlacement(&wp)) {
		wp.flags = 0;
		if (IsZoomed())
			wp.flags |= WPF_RESTORETOMAXIMIZED;
		WriteWindowPlacement(&wp);
	}
	CFrameWnd::OnClose();
}

BOOL CMainFrame::OnCreateClient(LPCREATESTRUCT /*lpcs*/,
	CCreateContext* pContext)
{
	// create splitter window
	m_nPaneSplit = AfxGetApp()->GetProfileInt(szSettings,
		szPaneSplit, 230);
	if (!m_wndSplitter.CreateStatic(this, 2, 1))
		return FALSE;

	if (!m_wndSplitter.CreateView(0, 0, RUNTIME_CLASS(CQpadView), CSize(100, m_nPaneSplit), pContext) ||
		!m_wndSplitter.CreateView(1, 0, RUNTIME_CLASS(CEvalView), CSize(100, 100), pContext))
	{
		m_wndSplitter.DestroyWindow();
		return FALSE;
	}

	return TRUE;
}

LRESULT CMainFrame::OnInput(WPARAM, LPARAM)
{
	// process input signalled by the WM_USER_INPUT message
	GetEvalView()->ProcessInput();
	return 0;
}

LRESULT CMainFrame::OnCompile(WPARAM, LPARAM res)
{
	// compiler has finished (WM_USER_COMPILE message); on failure,
	// display an error message in the status line
	if (!res)
		SetMessageText(IDS_COMPILE_ERR);
	return 0;
}

/////////////////////////////////////////////////////////////////////////////
// CMainFrame diagnostics

#ifdef _DEBUG
void CMainFrame::AssertValid() const
{
	CFrameWnd::AssertValid();
}

void CMainFrame::Dump(CDumpContext& dc) const
{
	CFrameWnd::Dump(dc);
}

#endif //_DEBUG

/////////////////////////////////////////////////////////////////////////////
// CMainFrame message handlers

void CMainFrame::OnUpdateLineNumber(CCmdUI* pCmdUI)
{
	pCmdUI->Enable(TRUE);
}

void CMainFrame::OnScriptRun() 
{
	// save current script if modified
	if (!AfxGetApp()->SaveAllModified())
		return;
	// run the script
	CQpadDoc* pDoc = (CQpadDoc*)GetActiveDocument();
	LPCTSTR name = pDoc->GetPathName();
	ASSERT(name != NULL);
	if (!GetEvalView()->Run(name, *name?name:pDoc->GetTitle())) {
		CString msg;
		msg.LoadString(IDS_RUN_FAILED);
		AfxMessageBox(msg);
		return;
	}
	// update status info
	if (!*name || name != m_strRunPathName) SetStatusInfo();
	UpdateTitle();
	// change to eval pane
	ActivateEvalView();
	ResetEvalView();
}

void CMainFrame::OnScriptDebug() 
{
	// save current script if modified
	if (!AfxGetApp()->SaveAllModified())
		return;
	// run the (main) script
	// run the script
	CQpadDoc* pDoc = (CQpadDoc*)GetActiveDocument();
	LPCTSTR name = pDoc->GetPathName();
	ASSERT(name != NULL);
	if (!GetEvalView()->Debug(name, *name?name:pDoc->GetTitle())) {
		CString msg;
		msg.LoadString(IDS_RUN_FAILED);
		AfxMessageBox(msg);
		return;
	}
	// update status info
	if (!*name || name != m_strRunPathName) SetStatusInfo();
	UpdateTitle();
	// change to eval pane
	ActivateEvalView();
	ResetEvalView();
}

void CMainFrame::OnScriptBreak() 
{
	GetEvalView()->Break();
	ActivateEvalView();
}

void CMainFrame::OnScriptKill()
{
	GetEvalView()->Kill();
	ActivateEvalView();
}

void CMainFrame::OnUpdateScriptBreak(CCmdUI* pCmdUI) 
{
	pCmdUI->Enable(IsRunning());
}

void CMainFrame::OnUpdateScriptKill(CCmdUI* pCmdUI) 
{
	pCmdUI->Enable(IsRunning());
}

void CMainFrame::OnScriptReset() 
{
	m_bLogReset = !m_bLogReset;
}

void CMainFrame::OnUpdateScriptReset(CCmdUI* pCmdUI) 
{
	pCmdUI->SetCheck(m_bLogReset);
}

// path searching subroutines from qbase.c in the Q distribution

static char *dirstr = "/\\:", *volstr = ":";
#define PATHDELIM ';'

static char	*home()
{
	static char *homedir = NULL;
	if (!homedir && !(homedir = getenv("HOME"))) {
		homedir = "/";
		*homedir = *dirstr;
	}
	return homedir;
}

#define tilde(s) (s[0] == '~' && (!s[1] || strchr(dirstr, s[1]) && !strchr(volstr, s[1])))

static int absolute(const char *s)
{
  const char *t = s;
  if (!s || !*s)
    return 0;
  else if (tilde(s))
    return 1;
  else {
    while (*s && !strchr(dirstr, *s)) ++s;
    return *s && (s == t || strchr(volstr, *s));
  }
}

static char *expand(char *s1, const char *s2)
{
  if (tilde(s2)) {
    char *h = home();
    int l = strlen(h);
    strcpy(s1, h);
    if (l > 1 && strchr(dirstr, h[l-1]))
      strcpy(s1+l, s2+2);
    else
      strcpy(s1+l, s2+1);
  } else
    strcpy(s1, s2);
  return s1;
}

static char *searchlib(char *s1, const char *s2, const char *qpath)
{
  const char *s;
  char *t;

  if (tilde(s2))
    return expand(s1, s2);
  else if (absolute(s2))
    return strcpy(s1, s2);
  for (s = qpath; *s; s = t) {
    int l;
    FILE *fp;
    char p[_MAX_PATH];
    if (!(t = strchr(s, PATHDELIM)))
      t = strchr(s, 0);
    if (s == t) goto next;
    if (s[0] == '.')
      if (t == s+1)
	s = t;
      else if (strchr(dirstr, s[1]) &&
	       !strchr(volstr, s[1]))
	s += 2;
    l = t-s;
    strncpy(p, s, l);
    p[l] = 0;
    expand(s1, p);
    l = strlen(s1);
    if (l > 0 && (!strchr(dirstr, s1[l-1]) || 
		  strchr(volstr, s1[l-1])))
      s1[l] = *dirstr, l++;
    strcpy(s1+l, s2);
    if (fp = fopen(s1, "rb")) {
      fclose(fp);
      return s1;
    }
  next:
    if (*t) t++;
  }
  return strcpy(s1, s2);
}

static void MakePathName(CString path, CString& name)
{
	char *realname = new char[_MAX_PATH+CMainFrame::m_strQPATH.GetLength()];
	ASSERT(realname != NULL);
	searchlib(realname, name, CMainFrame::m_strQPATH);
	name = realname;
	delete[] realname;
	char drive[_MAX_DRIVE];
	char dir[_MAX_DIR];
	char fname[_MAX_FNAME];
	char ext[_MAX_EXT];
	_splitpath(name, drive, dir, fname, ext);
	if (!*drive && !*dir) name = path+name;
}

static void Trim(CString& str, int len)
{
	if (str.GetLength() > len) str = str.Left(len-3) + "...";
}

void CMainFrame::OnErr(CString& fname, int& lineno,
					   int& ref, CString& msg)
{
	Trim(msg, 80);
	CDocument* pDoc = GetQpadView()->GetDocument();
	CString act_fname = pDoc->GetPathName();
	MakePathName(m_strRunPath, fname);
	ActivateEvalView();
	GetEvalView()->MarkLine(ref);
	if (act_fname == fname ||
		AfxGetApp()->OpenDocumentFile(fname)) {
		ActivateQpadView();
		GetQpadView()->GotoLine(lineno-1);
		UpdateTitle();
		// show fname, line instead of actual text of message
		msg.Format("%s, line %d", fname, lineno);
		if (!msg.IsEmpty()) SetMessageText(msg);
	}
}

void CMainFrame::OnFirstErr()
{
	CString fname, msg;
	int lineno, ref;
	if (GetEvalView()->GetFirstErr(fname, lineno, ref, msg) ||
		// back up to most recent error
		GetEvalView()->GetPrevErr(fname, lineno, ref, msg) &&
		GetEvalView()->GetFirstErr(fname, lineno, ref, msg, ref))
		OnErr(fname, lineno, ref, msg);
	else {
		SetMessageText(IDS_NOMSG);
		MessageBeep(MB_OK);
	}
}

void CMainFrame::OnLastErr()
{
	CString fname, msg;
	int lineno, ref;
	if (GetEvalView()->GetLastErr(fname, lineno, ref, msg) ||
		GetEvalView()->GetPrevErr(fname, lineno, ref, msg))
		OnErr(fname, lineno, ref, msg);
	else {
		SetMessageText(IDS_NOMSG);
		MessageBeep(MB_OK);
	}
}

void CMainFrame::OnPrevErr()
{
	CString fname, msg;
	int lineno, ref;
	if (GetEvalView()->GetPrevErr(fname, lineno, ref, msg))
		OnErr(fname, lineno, ref, msg);
	else {
		SetMessageText(IDS_NOMSG);
		MessageBeep(MB_OK);
	}
}

void CMainFrame::OnNextErr()
{
	CString fname, msg;
	int lineno, ref;
	if (GetEvalView()->GetNextErr(fname, lineno, ref, msg))
		OnErr(fname, lineno, ref, msg);
	else {
		SetMessageText(IDS_NOMSG);
		MessageBeep(MB_OK);
	}
}

void CMainFrame::OnUpdateFirstErr(CCmdUI* pCmdUI) 
{
	pCmdUI->Enable(GetEvalView()->GetEditCtrl().GetWindowTextLength());
}

void CMainFrame::OnUpdateLastErr(CCmdUI* pCmdUI) 
{
	pCmdUI->Enable(GetEvalView()->GetEditCtrl().GetWindowTextLength());
}

void CMainFrame::OnUpdatePrevErr(CCmdUI* pCmdUI) 
{
	pCmdUI->Enable(GetEvalView()->GetEditCtrl().GetWindowTextLength());
}

void CMainFrame::OnUpdateNextErr(CCmdUI* pCmdUI) 
{
	pCmdUI->Enable(GetEvalView()->GetEditCtrl().GetWindowTextLength());
}

void CMainFrame::OnInputLine() 
{
	CEdit& edit = GetEvalView()->GetEditCtrl();
	ActivateEvalView();
	int n = edit.GetWindowTextLength();
	edit.SetSel(n, n);
}

void CMainFrame::OnUpdateInputLine(CCmdUI* pCmdUI) 
{
	pCmdUI->Enable(GetEvalView()->GetEditCtrl().GetWindowTextLength());
}

void CMainFrame::OnScriptOpen() 
{
	CDocument* pDoc = GetQpadView()->GetDocument();
	CString act_fname = pDoc->GetPathName();
	if (act_fname == m_strRunPathName ||
		AfxGetApp()->OpenDocumentFile(m_strRunPathName))
		if (act_fname != m_strRunPathName) UpdateTitle();
}

void CMainFrame::OnUpdateScriptOpen(CCmdUI* pCmdUI) 
{
	CDocument* pDoc = GetQpadView()->GetDocument();
	pCmdUI->Enable(m_strRunPathName != "" &&
		m_strRunPathName != pDoc->GetPathName());
}

void CMainFrame::OnScriptPath() 
{
	CQPathDlg dlg;
	dlg.m_strPath = m_strQPATH;
	if (dlg.DoModal() == IDOK)
	{
		m_strQPATH = dlg.m_strPath;
		CString set_qpath;
		set_qpath.Format("QPATH=%s", m_strQPATH);
		if (_putenv(set_qpath))
			AfxMessageBox(IDS_ENV_FULL);
	}
}

void CMainFrame::OnScriptPrompt() 
{
	CPromptDlg dlg;
	dlg.m_strQPS = CMainFrame::m_strQPS;
	if (dlg.DoModal() == IDOK)
	{
		CMainFrame::m_strQPS = dlg.m_strQPS;
		CMainFrame::m_strQPSX = LastLine(CMainFrame::m_strQPS);
	}
}

void CMainFrame::OnScriptHistfile() 
{
	CHistfileDlg dlg;
	dlg.m_strHistFile = CMainFrame::m_strHistFile;
	if (dlg.DoModal() == IDOK)
	{
		GetEvalView()->m_hist.Write(CMainFrame::m_strHistFile);
		CMainFrame::m_strHistFile = dlg.m_strHistFile;
		GetEvalView()->m_hist.Read(CMainFrame::m_strHistFile);
	}
}

void CMainFrame::OnScriptHistsize() 
{
	CHistsizeDlg dlg;
	dlg.m_nHistSize = CMainFrame::m_nHistSize;
	if (dlg.DoModal() == IDOK)
	{
		CMainFrame::m_nHistSize = dlg.m_nHistSize;
		GetEvalView()->m_hist.SetMaxSize(CMainFrame::m_nHistSize);
	}
}

BOOL CMainFrame::OnCommand(WPARAM wParam, LPARAM lParam) 
{
	// reset any previous message (compilation, On{Prev,Next}Err)
	SetMessageText(AFX_IDS_IDLEMESSAGE);
	return CFrameWnd::OnCommand(wParam, lParam);
}

// public member functions

void CMainFrame::InitialShowWindow(int nCmdShow)
{
	WINDOWPLACEMENT wp;
	if (ReadWindowPlacement(&wp))
		SetWindowPlacement(&wp);
	ShowWindow(nCmdShow);
}

BOOL CMainFrame::IsRunning()
{
	return GetEvalView()->IsRunning();
}

CEvalView* CMainFrame::GetEvalView()
{
	CWnd* pWnd = m_wndSplitter.GetPane(1, 0);
	CEvalView* pView = DYNAMIC_DOWNCAST(CEvalView, pWnd);
	return pView;
}

CQpadView* CMainFrame::GetQpadView()
{
	CWnd* pWnd = m_wndSplitter.GetPane(0, 0);
	CQpadView* pView = DYNAMIC_DOWNCAST(CQpadView, pWnd);
	return pView;
}

void CMainFrame::ActivateEvalView()
{
	m_wndSplitter.SetActivePane(1, 0);
}

void CMainFrame::ActivateQpadView()
{
	m_wndSplitter.SetActivePane(0, 0);
}

void CMainFrame::ResetEvalView()
{
	if (m_bLogReset)
		GetEvalView()->Reset();
}

void CMainFrame::SetStatusInfo()
{
	CQpadDoc* pDoc = (CQpadDoc*)GetActiveDocument();
	m_strRunTitle = pDoc->GetTitle();
	m_strRunPathName = pDoc->GetPathName();
	if (m_strRunPathName.IsEmpty()) {
		m_strRunPath = "";
		m_strRunFile = m_strRunTitle;
	} else {
		char drive[_MAX_DRIVE], dir[_MAX_DIR];
		char fname[_MAX_FNAME], ext[_MAX_EXT];
		_splitpath(m_strRunPathName, drive, dir, fname, ext);
		m_strRunPath = drive; m_strRunPath += dir;
		m_strRunFile = fname; m_strRunFile += ext;
	}
}

void CMainFrame::UpdateTitle()
{
	CString str;
	CQpadDoc* pDoc = (CQpadDoc*)GetActiveDocument();
	if (m_strRunTitle != "") {
		CString termmsg;
		termmsg.LoadString(IDS_TERMINATED);
		str.Format(_T("%s%s - purepad [%s%s]"), pDoc->GetTitle(),
			_T(pDoc->IsModified()?"*":""),
			IsRunning()?"":termmsg, m_strRunTitle);
	} else {
		str.Format(_T("%s%s - purepad"), pDoc->GetTitle(),
			_T(pDoc->IsModified()?"*":""));
	}
	CString old;
	GetWindowText(old);
	if (old != str)
		SetWindowText(str);
}

