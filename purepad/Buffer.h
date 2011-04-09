// Buffer.h: interface for the CBuffer class.
//
//////////////////////////////////////////////////////////////////////

#if !defined(AFX_BUFFER_H__C5D9D0C0_A0A5_11D2_BCEB_B94DC0861264__INCLUDED_)
#define AFX_BUFFER_H__C5D9D0C0_A0A5_11D2_BCEB_B94DC0861264__INCLUDED_

#if _MSC_VER > 1000
#pragma once
#endif // _MSC_VER > 1000

#include <afxmt.h>

// implements a thread-save I/O buffer

class CBuffer  
{
public:
	CBuffer();
	virtual ~CBuffer();

	BOOL Write(LPCTSTR lpszStr);
	// append a string to the buffer (return TRUE if buffer is
	// currently empty)
	LPTSTR Read();
	// get the current buffer contents and empty the buffer
	// (the result is allocated dynamically using new and should
	// be deallocated using delete by the caller)
	LPTSTR Peek();
	// same as ReadBuffer, but retains the buffer contents
	void Empty();
	// empty the buffer
	int GetLength();
	// return the current buffer length
	BOOL IsEmpty();
	// check whether the buffer is empty

private:
	CString m_strBuf;
	CMutex m_mutex;
};

#endif // !defined(AFX_BUFFER_H__C5D9D0C0_A0A5_11D2_BCEB_B94DC0861264__INCLUDED_)
