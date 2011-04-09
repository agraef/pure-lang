// Pipe.h: interface for the CPipe class.
//
//////////////////////////////////////////////////////////////////////

#if !defined(AFX_PIPE_H__87EB6D20_9FD1_11D2_BCEB_AE52699C7164__INCLUDED_)
#define AFX_PIPE_H__87EB6D20_9FD1_11D2_BCEB_AE52699C7164__INCLUDED_

#include "Buffer.h"	// Added by ClassView
#if _MSC_VER > 1000
#pragma once
#endif // _MSC_VER > 1000

/*
  The CPipe class implements a bidirectional pipe to an interpreter
  child process. The member functions allow to create a new child
  process, signal it a break, stop the process, feed input into the
  child's stdin, and obtain its response on the child's stdout. Only
  a single child process can be active at any time. The application
  is notified when new input from the interpreter's stdout becomes
  available by means of the WM_USER_INPUT message, which is passed
  to the main window. Furthermore, the main window is notfied by
  a WM_USER_COMPILE message when the compiler has finished, giving
  its result in the LPARAM parameter.
*/

struct ThreadInfo {
	HANDLE hMutex;
	HWND hWnd;
	HANDLE hChildStdinRd, hChildStdoutWr;
	HANDLE hChildStdinWr, hChildStdoutRd;
	HANDLE hOutput, hKillThreads;
	CBuffer *pInput, *pOutput;
	LPCTSTR file, path, compile, run, code;
	PROCESS_INFORMATION pi;
	HANDLE hBreak, hKill;
	TCHAR szBreak[MAX_PATH], szKill[MAX_PATH];
};

class CPipe  
{
public:

	// construction

	CPipe();
	virtual ~CPipe();

	BOOL Run(LPCTSTR name, LPCTSTR pname);
	// create a new interpreter process for script name (full path
	// or empty string if none) kill off an existing child process
	// if present; returns TRUE iff interpreter was started
	// successfully

	BOOL Debug(LPCTSTR name, LPCTSTR pname);
	// same as Run(), but invokes the debugger

	void Break();
	// signal a break (Ctl-C)

	void Kill();
	// kill the child process (called automatically when the instance
	// is destroyed); also removes the code file

	BOOL IsRunning();
	// checks whether the child process is still up and running

	void Write(LPCTSTR lpszBuf);
	// send a string to the child's stdin

	LPTSTR Read();
	// get the currently available output from the child's stdout
	// and empty the buffer; the string is allocated dynamically
	// using new and should be deallocated by the caller using
	// delete

	LPTSTR Peek();
	// like Read(), but does not empty the buffer

	void Empty();
	// empty the input buffer

	int GetLength();
	// get the current length of the input buffer

	BOOL IsEmpty();
	// check whether the input buffer is empty

private:
	void KillThreads();
	void KillChild();
	void Clean();
	BOOL Run2(LPCTSTR command, LPCTSTR name, LPCTSTR pname);
	char path[_MAX_PATH], file[_MAX_PATH], code[_MAX_FNAME];
	CBuffer m_bufInput, m_bufOutput;
	ThreadInfo m_ti;
	BOOL m_bRunning;
	HANDLE hChildStdinRd, hChildStdinWr;
	HANDLE hChildStdoutRd, hChildStdoutWr;
	HANDLE hReader, hWriter, hCompileRun;
	DWORD idReader, idWriter, idCompileRun;
	HANDLE hMutex, hOutput, hKillThreads;
};

#endif // !defined(AFX_PIPE_H__87EB6D20_9FD1_11D2_BCEB_AE52699C7164__INCLUDED_)
