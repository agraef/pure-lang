// Pipe.cpp: implementation of the CPipe class.
//
//////////////////////////////////////////////////////////////////////

#include "stdafx.h"
#include "qpad.h"
#include "Pipe.h"
#include "Buffer.h"
#include "MainFrm.h"
#include <stdlib.h> // for _split_path()

#ifdef _DEBUG
#undef THIS_FILE
static char THIS_FILE[]=__FILE__;
#define new DEBUG_NEW
#endif

//////////////////////////////////////////////////////////////////////
// Construction/Destruction
//////////////////////////////////////////////////////////////////////

CPipe::CPipe()
{
	m_bRunning = FALSE;
	hChildStdinRd = hChildStdinWr = INVALID_HANDLE_VALUE;
	hChildStdoutRd = hChildStdoutWr = INVALID_HANDLE_VALUE;
	hReader = hWriter = hCompileRun = NULL;
	hOutput = CreateEvent(NULL, FALSE, FALSE, NULL);
	hKillThreads = CreateEvent(NULL, TRUE, FALSE, NULL);
	hMutex = CreateMutex(NULL, FALSE, NULL);
	ASSERT(hOutput != NULL && hKillThreads != NULL && hMutex != NULL);
}

CPipe::~CPipe()
{
	Kill();
	CloseHandle(hOutput);
}

// public member functions

BOOL CPipe::Run(LPCTSTR name, LPCTSTR pname)
{
	return Run2(CMainFrame::m_strRunCommand, name, pname);
}

BOOL CPipe::Debug(LPCTSTR name, LPCTSTR pname)
{
	return Run2(CMainFrame::m_strDebugCommand, name, pname);
}

void CPipe::Write(LPCTSTR lpszBuf)
{
	if (m_bufOutput.Write(lpszBuf))
		SetEvent(hOutput);
}

LPTSTR CPipe::Read()
{
	return m_bufInput.Read();
}

LPTSTR CPipe::Peek()
{
	return m_bufInput.Peek();
}

void CPipe::Empty()
{
	m_bufInput.Empty();
}

int CPipe::GetLength()
{
	return m_bufInput.GetLength();
}

BOOL CPipe::IsEmpty()
{
	return m_bufInput.IsEmpty();
}

/*

Now comes the hairy part. Quite a bit of synchronization is needed
here to make the main thread and the background (worker) threads
work peacefully together. Also, we need to manufacture our own
"kill" and "break" events to synchronize with the compiler and
interpreter child processes. This is necessary because brain-
damaged Windows does not have kill() and the console control
events only work if a console is attached.

*/

// reader and writer thread routines

// save to let these run without gaining access to the thread info
// mutex since all accesses are mutually exclusive (or are
// thread-save CBuffer accesses)

static DWORD WINAPI Reader(LPVOID pParam)
{
	ThreadInfo* ti = (ThreadInfo*)pParam;
	char buf[1025];
	DWORD n;
	while (WaitForSingleObject(ti->hKillThreads, 0) !=
		WAIT_OBJECT_0 &&
		ReadFile(ti->hChildStdoutRd, buf, 1024, &n, NULL)) {
		if (n > 0) {
			buf[n] = 0;
			if (ti->pInput->Write(buf))
				PostMessage(ti->hWnd, WM_USER_INPUT, 0, 0);
		}
	}
	return 0;
}

static DWORD WINAPI Writer(LPVOID pParam)
{
	ThreadInfo* ti = (ThreadInfo*)pParam;
	HANDLE hEvents[2] = { ti->hOutput, ti->hKillThreads };

	while (WaitForMultipleObjects(2, hEvents, FALSE, INFINITE) ==
		WAIT_OBJECT_0) {
		BOOL success = TRUE;
		LPTSTR buf = ti->pOutput->Read();
		if (*buf) {
			DWORD n, len = strlen(buf);
			success =
				WriteFile(ti->hChildStdinWr, buf, len, &n, NULL)
				&& n == len;
		}
		delete[] buf;
		if (!success) break;
	}
	return 0;
}

static void MakeEvents(ThreadInfo* ti)
{
	SECURITY_ATTRIBUTES saAttr;

	saAttr.nLength = sizeof(SECURITY_ATTRIBUTES);
	saAttr.bInheritHandle = TRUE;
	saAttr.lpSecurityDescriptor = NULL; 

	sprintf(ti->szBreak, "PURE_SIGINT-%u", ti->pi.dwProcessId);
	sprintf(ti->szKill, "PURE_SIGTERM-%u", ti->pi.dwProcessId);
	ti->hBreak = CreateEvent(&saAttr, TRUE, FALSE, ti->szBreak);
	ti->hKill = CreateEvent(&saAttr, TRUE, FALSE, ti->szKill);
}

static BOOL MakeTempName(CString& name)
{
	DWORD len = GetTempPath(0, NULL);
	LPTSTR tmppath = new TCHAR[len];
	DWORD res = GetTempPath(len, tmppath);
	if (res == 0 || res > len) {
		// this can't happen??
		delete[] tmppath;
		return FALSE;
	}
	LPTSTR tmpname = new TCHAR[MAX_PATH];
	UINT res2 = GetTempFileName(tmppath, "Q", 0, tmpname);
	delete[] tmppath;
	if (res2) name = tmpname;
	delete[] tmpname;
	return res2;
}

static void QuoteArg(CString& s)
{
	if (s.FindOneOf(" \t") >= 0) {
		s.Replace("\"", "\\\"");
		s = "\"" + s + "\"";
	}
}

static DWORD WINAPI CompileRun(LPVOID pParam)
{
	ThreadInfo* ti = (ThreadInfo*)pParam;

	// run compiler and interpreter in the background

	// initialize process startup info
    STARTUPINFO si;

    ZeroMemory(&si, sizeof(si));
	si.cb = sizeof(si);
	si.dwFlags = STARTF_USESTDHANDLES|STARTF_USESHOWWINDOW;
	si.wShowWindow = SW_HIDE;
	si.hStdInput = ti->hChildStdinRd;
	si.hStdOutput = ti->hChildStdoutWr;
	si.hStdError = ti->hChildStdoutWr;

	// wait until we can get access to the thread info structure
	HANDLE hMutexEvents[2] = { ti->hMutex, ti->hKillThreads };
	if (WaitForMultipleObjects(2, hMutexEvents, FALSE, INFINITE)
		!= WAIT_OBJECT_0)
		return 1;

#if 0
	// spawn the compiler process (syntax checking)
	CString cmd, codearg = ti->code, filearg = ti->file;
	QuoteArg(codearg); QuoteArg(filearg);
	cmd.Format(ti->compile, codearg, filearg);
	BOOL res = CreateProcess(NULL, cmd.GetBuffer(cmd.GetLength()+1),
			NULL, NULL, TRUE,
			CREATE_NEW_CONSOLE,
			NULL, *ti->path?ti->path:NULL, &si, &ti->pi);
	cmd.ReleaseBuffer();
	if (!res) { ReleaseMutex(ti->hMutex); return 1; }
	ASSERT(ti->pi.hProcess != NULL);
	MakeEvents(ti);

	// now save to let the main thread take over until the
	// compiler has finished
	ReleaseMutex(ti->hMutex);

	// wait for the compiler to finish
	DWORD code;
	HANDLE hProcessEvents[2] = { ti->pi.hProcess, ti->hKillThreads };
	res = WaitForMultipleObjects(2, hProcessEvents, FALSE, INFINITE)
			== WAIT_OBJECT_0 &&
			GetExitCodeProcess(ti->pi.hProcess, &code) &&
			code == 0;
	// scratch any code file left over by compiler (shouldn't happen
	// unless compiler has died)
	DeleteFile(ti->code);

	// regain access
	if (WaitForMultipleObjects(2, hMutexEvents, FALSE, INFINITE)
		!= WAIT_OBJECT_0)
		return 1;
	CloseHandle(ti->pi.hProcess); ti->pi.hProcess = NULL;
	CloseHandle(ti->hBreak);
	CloseHandle(ti->hKill);
	PostMessage(ti->hWnd, WM_USER_COMPILE, 0, res);
	if (!res) { ReleaseMutex(ti->hMutex); return 1; }
#endif

	// spawn the interpreter process

#if 0
	CString prompt = CMainFrame::m_strQPS;
	QuoteArg(prompt);
	cmd.Format(ti->run, codearg, filearg);
	cmd += " --prompt ";
	cmd += prompt;
#else
	BOOL res;
	CString cmd, filearg = ti->file;
	cmd.Format(ti->run, filearg);
	CString prompt = CMainFrame::m_strQPS;
	CString set_prompt;
	set_prompt.Format("PURE_PS=%s", prompt);
	_putenv(set_prompt);
#endif
	res = CreateProcess(NULL, cmd.GetBuffer(cmd.GetLength()+1),
			NULL, NULL, TRUE,
			CREATE_NEW_CONSOLE,
			NULL, *ti->path?ti->path:NULL, &si, &ti->pi);
	cmd.ReleaseBuffer();
	ASSERT(ti->pi.hProcess != NULL);
	if (!res) { ReleaseMutex(ti->hMutex); return 1; }
	MakeEvents(ti);

	ReleaseMutex(ti->hMutex);
	return 0;
}

BOOL CPipe::Run2(LPCTSTR command, LPCTSTR name, LPCTSTR pname)
{
	// kill running process if it exists
	Kill();

	// show script name
 	if (!CMainFrame::m_bLogReset) {
 		CString msg;
 		msg.Format("// %s\r\n", pname);
		m_bufInput.Write(msg);
		PostMessage(AfxGetMainWnd()->m_hWnd, WM_USER_INPUT, 0, 0);
 	}

	// get filename/directory info
	char drive[_MAX_DRIVE], dir[_MAX_DIR], fname[_MAX_FNAME],
		ext[_MAX_EXT];
	_splitpath(name, drive, dir, fname, ext);
	strcat(strcpy(path, drive), dir);
	strcat(strcpy(file, fname), ext);

	// create a temporary code file name
	CString tmpname;
	if (!MakeTempName(tmpname) || tmpname.GetLength()+1 > _MAX_PATH)
		return FALSE;
	strcpy(code, tmpname);

	// set up the pipes

	// set attributes s.t. pipe handles are inherited
	SECURITY_ATTRIBUTES saAttr;

	saAttr.nLength = sizeof(SECURITY_ATTRIBUTES);
	saAttr.bInheritHandle = TRUE;
	saAttr.lpSecurityDescriptor = NULL; 

	// initialize handles
	HANDLE hChildStdinWrDup = INVALID_HANDLE_VALUE,
		hChildStdoutRdDup = INVALID_HANDLE_VALUE;

	// create pipes for the child's stdio streams
	if (!CreatePipe(&hChildStdinRd, &hChildStdinWr, &saAttr, 0)) 
		goto fail;
	if (!CreatePipe(&hChildStdoutRd, &hChildStdoutWr, &saAttr, 0)) 
		goto fail;

	// duplicate and close handles which should not be inherited by
	// child
	if (!DuplicateHandle(GetCurrentProcess(), hChildStdoutRd,
			GetCurrentProcess(), &hChildStdoutRdDup, 0, FALSE,
			DUPLICATE_SAME_ACCESS))
		goto fail;
	CloseHandle(hChildStdoutRd);
	hChildStdoutRd = hChildStdoutRdDup;
	hChildStdoutRdDup = INVALID_HANDLE_VALUE;
	if (!DuplicateHandle(GetCurrentProcess(), hChildStdinWr,
			GetCurrentProcess(), &hChildStdinWrDup, 0, FALSE,
			DUPLICATE_SAME_ACCESS))
		goto fail;
	CloseHandle(hChildStdinWr);
	hChildStdinWr = hChildStdinWrDup;
	hChildStdinWrDup = INVALID_HANDLE_VALUE;

	// child's file handles
	m_ti.hChildStdinRd = hChildStdinRd;
	m_ti.hChildStdoutWr = hChildStdoutWr;
	// parent's file handles
	m_ti.hChildStdinWr = hChildStdinWr;
	m_ti.hChildStdoutRd = hChildStdoutRd;
	
	// spawn the worker threads which perform asynchronous I/O
	m_ti.hWnd = AfxGetMainWnd()->m_hWnd;
	m_ti.hMutex = hMutex;
	m_ti.hKillThreads = hKillThreads;
	m_ti.hOutput = hOutput;
	m_ti.pInput = &m_bufInput;
	m_ti.pOutput = &m_bufOutput;
	m_ti.path = path; m_ti.file = file; m_ti.code = code;
#if 0
	m_ti.compile = CMainFrame::m_strCompileCommand;
#endif
	m_ti.run = command;
	hReader = CreateThread(NULL, 0, Reader, &m_ti, 0, &idReader);
	hWriter = CreateThread(NULL, 0, Writer, &m_ti, 0, &idWriter);
	ASSERT(hReader != NULL && hWriter != NULL);

	// spawn the compile/run thread
	m_ti.pi.hProcess = NULL;
	m_ti.hBreak = m_ti.hKill = NULL;
	hCompileRun = CreateThread(NULL, 0, CompileRun, &m_ti, 0,
		&idCompileRun);
	ASSERT(hCompileRun != NULL);
	m_bRunning = TRUE;

	return TRUE;

fail:
	// clean up
	if (hChildStdinRd != INVALID_HANDLE_VALUE)
		CloseHandle(hChildStdinRd);
	if (hChildStdinWr != INVALID_HANDLE_VALUE)
		CloseHandle(hChildStdinWr);
	if (hChildStdinWrDup != INVALID_HANDLE_VALUE)
		CloseHandle(hChildStdinWrDup);
	if (hChildStdoutRd != INVALID_HANDLE_VALUE)
		CloseHandle(hChildStdoutRd);
	if (hChildStdoutWr != INVALID_HANDLE_VALUE)
		CloseHandle(hChildStdoutWr);
	if (hChildStdoutRdDup != INVALID_HANDLE_VALUE)
		CloseHandle(hChildStdoutRdDup);
	hChildStdinRd = hChildStdinWr = INVALID_HANDLE_VALUE;
	hChildStdoutRd = hChildStdoutWr = INVALID_HANDLE_VALUE;
	return FALSE;
}

void CPipe::Break()
{
	if (IsRunning() &&
		WaitForSingleObject(m_ti.hMutex, INFINITE) ==
		WAIT_OBJECT_0 &&
		m_ti.pi.hProcess != NULL) {
		ASSERT(m_ti.hBreak != NULL);
		PulseEvent(m_ti.hBreak);
		ResetEvent(hOutput);
		ReleaseMutex(hMutex);
	}
}

void CPipe::Kill()
{
	if (IsRunning() &&
		WaitForSingleObject(m_ti.hMutex, INFINITE) ==
		WAIT_OBJECT_0 &&
		m_ti.pi.hProcess != NULL) {
		// signal the process
		ASSERT(m_ti.hKill != NULL);
		PulseEvent(m_ti.hKill);
		// give it some time to arrive
		Sleep(100);
		// close our side of the stdin handle (in case
		// the interpreter sits waiting in an input loop)
		if (hChildStdinWr != INVALID_HANDLE_VALUE)
			CloseHandle(hChildStdinWr);
		hChildStdinWr = INVALID_HANDLE_VALUE;
		// give the process some time to terminate, then
		// kill it
		CWaitCursor wait;
		if (WaitForSingleObject(m_ti.pi.hProcess, 1000) !=
			WAIT_OBJECT_0) {
			/*
			CString msg;
			msg.LoadString(IDS_KILL_FAILED);
			AfxMessageBox(msg);
			*/
			// asta la vista, baby
			TerminateProcess(m_ti.pi.hProcess, 0);
		}
		KillThreads();
		Clean();
		m_bRunning = FALSE;
		ReleaseMutex(hMutex);
	}
}

BOOL CPipe::IsRunning()
{
	if (m_bRunning) {
		// inspect process to see whether child and associated
		// threads are still up
		DWORD code;
		if (GetExitCodeThread(hCompileRun, &code) &&
			code == STILL_ACTIVE) {
			// still preparing interpreter process
			return TRUE;
		// save to access thread info now since compile/run thread
		// has finished
		} else if (m_ti.pi.hProcess == NULL ||
			!GetExitCodeProcess(m_ti.pi.hProcess, &code) ||
			code != STILL_ACTIVE) {
			KillThreads();
			Clean();
			m_bRunning = FALSE;
		} else if (!GetExitCodeThread(hReader, &code) ||
			code != STILL_ACTIVE) {
			CloseHandle(hReader); hReader = NULL;
			KillThreads();
			KillChild();
			Clean();
			m_bRunning = FALSE;
		} else if (!GetExitCodeThread(hWriter, &code) ||
			code != STILL_ACTIVE) {
			CloseHandle(hWriter); hWriter = NULL;
			KillThreads();
			KillChild();
			Clean();
			m_bRunning = FALSE;
		}
		return m_bRunning;
	} else
		return FALSE;
}

void CPipe::KillThreads()
{
	// kill the worker threads
	PulseEvent(hKillThreads);
	if (hCompileRun != NULL) {
		if (WaitForSingleObject(hCompileRun, 100) !=
			WAIT_OBJECT_0)
			TerminateThread(hCompileRun, 0);
		CloseHandle(hCompileRun);
	}
	if (hReader != NULL) {
		if (WaitForSingleObject(hReader, 100) !=
			WAIT_OBJECT_0)
			// last resort
			TerminateThread(hReader, 0);
		CloseHandle(hReader);
	}
	if (hWriter != NULL) {
		if (WaitForSingleObject(hWriter, 100) !=
			WAIT_OBJECT_0)
			TerminateThread(hWriter, 0);
		CloseHandle(hWriter);
	}
	hReader = hWriter = hCompileRun = NULL;
	ResetEvent(hOutput);
	// close all open handles
	if (hChildStdinRd != INVALID_HANDLE_VALUE)
		CloseHandle(hChildStdinRd);
	if (hChildStdinWr != INVALID_HANDLE_VALUE)
		CloseHandle(hChildStdinWr);
	if (hChildStdoutRd != INVALID_HANDLE_VALUE)
		CloseHandle(hChildStdoutRd);
	if (hChildStdoutWr != INVALID_HANDLE_VALUE)
		CloseHandle(hChildStdoutWr);
	hChildStdinRd = hChildStdinWr = INVALID_HANDLE_VALUE;
	hChildStdoutRd = hChildStdoutWr = INVALID_HANDLE_VALUE;
	// empty buffers
	//m_bufInput.Empty();
	m_bufOutput.Empty();
}

void CPipe::KillChild()
{
	ASSERT(m_ti.pi.hProcess != NULL);
	// give the process some time to terminate gracefully
	// after the pipes have been closed
	CWaitCursor wait;
	if (WaitForSingleObject(m_ti.pi.hProcess, 1000) ==
		WAIT_OBJECT_0)
		return;
	// process seems to be dead, get rid of it
	/*
	CString msg;
	msg.LoadString(IDS_KILL_FAILED);
	AfxMessageBox(msg);
	*/
	// asta la vista, baby
	TerminateProcess(m_ti.pi.hProcess, 0);
}

void CPipe::Clean()
{
	// close process and event handles
	if (m_ti.pi.hProcess != NULL) {
		ASSERT(m_ti.hBreak != NULL && m_ti.hKill != NULL);
		CloseHandle(m_ti.pi.hProcess); m_ti.pi.hProcess = NULL;
		CloseHandle(m_ti.hBreak); m_ti.hBreak = NULL;
		CloseHandle(m_ti.hKill); m_ti.hKill = NULL;
	}
#if 0
	// delete the code file
	DeleteFile(code);
#endif
}

