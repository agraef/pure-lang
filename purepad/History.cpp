// History.cpp: implementation of the CHistory class.
//
//////////////////////////////////////////////////////////////////////

#include "stdafx.h"
#include "qpad.h"
#include "History.h"

#ifdef _DEBUG
#undef THIS_FILE
static char THIS_FILE[]=__FILE__;
#define new DEBUG_NEW
#endif

//////////////////////////////////////////////////////////////////////
// Construction/Destruction
//////////////////////////////////////////////////////////////////////

CHistory::CHistory()
{
	m_nMaxSize = 100;
	m_nSize = 0;
}


CHistory::CHistory(int nMaxSize)
{
	m_nMaxSize = nMaxSize;
	m_nSize = 0;
}

CHistory::~CHistory()
{
}

BOOL CHistory::IsEmpty()
{
	return m_nSize == 0;
}

BOOL CHistory::HasPrev()
{
	return m_rPos != m_lLine.GetHeadPosition();
}

BOOL CHistory::HasNext()
{
	return m_rPos != NULL;
}

int CHistory::Size()
{
	return m_nSize;
}

BOOL CHistory::GetFirst(CString &strLine)
{
	if (m_nSize) {
		m_rPos = m_lLine.GetHeadPosition();
		strLine = m_lLine.GetAt(m_rPos);
		return TRUE;
	} else
		return FALSE;
}

BOOL CHistory::GetLast(CString &strLine)
{
	if (m_nSize) {
		m_rPos = NULL;
		strLine = "";
		return TRUE;
	} else
		return FALSE;
}

BOOL CHistory::GetPrev(CString &strLine)
{
	if (!m_nSize || m_rPos == m_lLine.GetHeadPosition())
		return FALSE;
	else {
		if (m_rPos)
			m_lLine.GetPrev(m_rPos);
		else
			m_rPos = m_lLine.GetTailPosition();
		strLine = m_lLine.GetAt(m_rPos);
		return TRUE;
	}
}

BOOL CHistory::GetNext(CString &strLine)
{
	if (m_nSize && m_rPos) {
		m_lLine.GetNext(m_rPos);
		if (m_rPos)
			strLine = m_lLine.GetAt(m_rPos);
		else
			strLine = "";
		return TRUE;
	} else
		return FALSE;
}

BOOL CHistory::SearchPrev(CString &strLine, CString strSearch)
{
	POSITION pos = m_rPos;
	CString str;
	while (GetPrev(str))
		if (strncmp(str, strSearch, strSearch.GetLength()) == 0) {
			strLine = str;
			return TRUE;
		}
	m_rPos = pos;
	return FALSE;
}

BOOL CHistory::SearchNext(CString &strLine, CString strSearch)
{
	POSITION pos = m_rPos;
	CString str;
	while (GetNext(str))
		if (strncmp(str, strSearch, strSearch.GetLength()) == 0) {
			strLine = str;
			return TRUE;
		}
	m_rPos = pos;
	return FALSE;
}

void CHistory::Clear()
{
	m_lLine.RemoveAll();
	m_nSize = 0;
	m_rPos = NULL;
}

void CHistory::Append(CString strLine, BOOL allow_dups)
{
	m_rPos = NULL;
	if (strLine.IsEmpty() || !m_nMaxSize) return;
	if (m_nSize && !allow_dups) {
		POSITION pos = m_lLine.GetTailPosition();
		if (strLine == m_lLine.GetAt(pos)) return;
	}
	if (m_nSize == m_nMaxSize)
		m_lLine.RemoveHead();
	else
		m_nSize++;
	m_lLine.AddTail(strLine);
}

void CHistory::SetMaxSize(int nMaxSize)
{
	if (nMaxSize < 0)
		nMaxSize = 0;
	m_nMaxSize = nMaxSize;
	while (m_nSize > m_nMaxSize) {
		m_lLine.RemoveHead();
		m_nSize--;
	}
}

BOOL CHistory::Read(CString strFile)
{
	CStdioFile f;
	Clear();
	if (!m_nMaxSize)
		return TRUE;
	else if (f.Open(strFile, CFile::modeRead)) {
		CString s;
		try {
			while (f.ReadString(s)) Append(s, TRUE);
			return TRUE;
		}
		catch (CFileException*) {
			return FALSE;
		}
	} else
		return FALSE;
}

BOOL CHistory::Write(CString strFile)
{
	CStdioFile f;
	if (m_nMaxSize <= 0)
		return TRUE;
	else if (f.Open(strFile, CFile::modeCreate | CFile::modeWrite)) {
		try {
			POSITION pos;
			for (pos = m_lLine.GetHeadPosition(); pos != NULL;) {
				f.WriteString(m_lLine.GetNext(pos) + "\n");
			}
			return TRUE;
		}
		catch (CFileException*) {
			return FALSE;
		}
	} else
		return FALSE;
}
