// History.h: interface for the CHistory class.
//
//////////////////////////////////////////////////////////////////////

#if !defined(AFX_HISTORY_H__A46E79E1_A3D9_11D2_BCEB_AA366DD80C62__INCLUDED_)
#define AFX_HISTORY_H__A46E79E1_A3D9_11D2_BCEB_AA366DD80C62__INCLUDED_

#if _MSC_VER > 1000
#pragma once
#endif // _MSC_VER > 1000

class CHistory  
{
public:
	CHistory();
	CHistory(int nMaxSize);
	virtual ~CHistory();

	BOOL IsEmpty();
	BOOL HasPrev();
	BOOL HasNext();
	int Size();

	BOOL GetFirst(CString& strLine);
	BOOL GetLast(CString& strLine);
	BOOL GetPrev(CString& strLine);
	BOOL GetNext(CString& strLine);
	BOOL SearchPrev(CString& strLine, CString strSearch);
	BOOL SearchNext(CString& strLine, CString strSearch);

	void Clear();
	void Append(CString strLine, BOOL allow_dups = FALSE);
	void SetMaxSize(int nMaxSize);
	BOOL Read(CString strFile);
	BOOL Write(CString strFile);

private:
	int m_nMaxSize;
	int m_nSize;
	CStringList m_lLine;
	POSITION m_rPos;
};

#endif // !defined(AFX_HISTORY_H__A46E79E1_A3D9_11D2_BCEB_AA366DD80C62__INCLUDED_)
