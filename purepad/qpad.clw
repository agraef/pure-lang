; CLW file contains information for the MFC ClassWizard

[General Info]
Version=1
LastClass=CHistsizeDlg
LastTemplate=CDialog
NewFileInclude1=#include "stdafx.h"
NewFileInclude2=#include "qpad.h"
LastPage=0

ClassCount=11
Class1=CQpadApp
Class2=CQpadDoc
Class3=CQpadView
Class4=CMainFrame

ResourceCount=43
Resource1=IDD_GOTO (English (U.S.))
Class5=CAboutDlg
Resource2=IDD_PROPPAGE_SMALL (English (U.S.))
Class6=CEvalView
Resource3=IDD_GOTO (Neutral (Standard))
Resource4="IDD_TABSTOPS"
Class7=CTabStopDlg
Resource5=IDD_PROPPAGE_MEDIUM (English (U.S.))
Resource6=IDD_DIALOGBAR (English (U.S.))
Resource7=IDD_FORMVIEW (English (U.S.))
Resource8=IDD_PROMPT (Englisch (USA))
Class8=CQPathDlg
Resource9=IDD_HISTSIZE (Neutral (Default))
Resource10=IDD_ABOUTBOX (Neutral (Default))
Resource11=IDD_TABSTOPS (Neutral (Default))
Resource12=IDR_MAINFRAME (Neutral (Default))
Resource13=IDD_HISTSIZE (Englisch (USA))
Resource14=IDD_PROMPT
Resource15=IDD_TABSTOPS (Englisch (USA))
Resource16=IDD_ABOUTBOX (English (U.S.))
Resource17=IDD_TABSTOPS (English (U.S.))
Resource18=IDD_QPATH (Neutral (Default))
Resource19=IDD_GOTO (Englisch (USA))
Resource20=IDD_HISTSIZE (English (U.S.))
Resource21=IDD_QPATH (Englisch (USA))
Resource22=IDR_MAINFRAME (Englisch (USA))
Resource23=IDD_HISTFILE (Englisch (USA))
Resource24=IDD_PROMPT (Neutral (Standard))
Resource25=IDD_ABOUTBOX (Englisch (USA))
Resource26=IDD_HISTFILE (English (U.S.))
Class9=CPromptDlg
Class10=CHistfileDlg
Class11=CHistsizeDlg
Resource27=IDD_HISTFILE (Neutral (Standard))
Resource28=IDD_GOTO (Neutral (Default))
Resource29=IDD_QPATH (English (U.S.))
Resource30=IDD_PROMPT (English (U.S.))
Resource31=IDD_ABOUTBOX
Resource32=IDD_PROMPT (Neutral (Default))
Resource33=IDD_TABSTOPS (Neutral (Standard))
Resource34=IDD_HISTFILE (Neutral (Default))
Resource35=IDD_HISTSIZE (Neutral (Standard))
Resource36=IDD_GOTO
Resource37=IDD_TABSTOPS
Resource38=IDD_HISTFILE
Resource39=IDR_MAINFRAME (Neutral)
Resource40=IDR_MAINFRAME (Neutral (Standard))
Resource41=IDR_MAINFRAME
Resource42=IDD_ABOUTBOX (Neutral (Standard))
Resource43=IDD_HISTSIZE

[CLS:CQpadApp]
Type=0
HeaderFile=qpad.h
ImplementationFile=qpad.cpp
Filter=N
BaseClass=CWinApp
VirtualFilter=AC
LastObject=CQpadApp

[CLS:CQpadDoc]
Type=0
HeaderFile=qpadDoc.h
ImplementationFile=qpadDoc.cpp
Filter=D
BaseClass=CDocument
VirtualFilter=DC
LastObject=ID_SCRIPT_KILL

[CLS:CQpadView]
Type=0
HeaderFile=qpadView.h
ImplementationFile=qpadView.cpp
Filter=C
BaseClass=CEditView
VirtualFilter=VWC
LastObject=CQpadView


[CLS:CMainFrame]
Type=0
HeaderFile=MainFrm.h
ImplementationFile=MainFrm.cpp
Filter=T
BaseClass=CFrameWnd
VirtualFilter=fWC
LastObject=CMainFrame



[CLS:CAboutDlg]
Type=0
HeaderFile=qpad.cpp
ImplementationFile=qpad.cpp
Filter=D
LastObject=CAboutDlg

[DLG:IDD_ABOUTBOX]
Type=1
Class=CAboutDlg
ControlCount=4
Control1=IDC_STATIC,static,1342177283
Control2=IDC_STATIC,static,1342308480
Control3=IDC_STATIC,static,1342308352
Control4=IDOK,button,1342373889

[MNU:IDR_MAINFRAME]
Type=1
Class=CMainFrame
Command1=ID_FILE_NEW
Command2=ID_FILE_OPEN
Command3=ID_FILE_SAVE
Command4=ID_FILE_SAVE_AS
Command5=ID_FILE_PRINT
Command6=ID_FILE_PRINT_PREVIEW
Command7=ID_FILE_PRINT_SETUP
Command8=ID_FILE_MRU_FILE1
Command9=ID_APP_EXIT
Command10=ID_EDIT_UNDO
Command11=ID_EDIT_CUT
Command12=ID_EDIT_COPY
Command13=ID_EDIT_PASTE
Command14=ID_EDIT_SELECT_ALL
Command15=ID_EDIT_FIND
Command16=ID_EDIT_REPLACE
Command17=ID_EDIT_GOTO
Command18=ID_SCRIPT_RUN
Command19=ID_SCRIPT_DEBUG
Command20=ID_SCRIPT_BREAK
Command21=ID_SCRIPT_KILL
Command22=ID_SCRIPT_OPEN
Command23=ID_SCRIPT_PROMPT
Command24=ID_SCRIPT_HISTFILE
Command25=ID_SCRIPT_HISTSIZE
Command26=ID_SCRIPT_RESET
Command27=ID_NEXT_PANE
Command28=ID_PREV_PANE
Command29=ID_INPUT_LINE
Command30=ID_VIEW_TOOLBAR
Command31=ID_VIEW_STATUS_BAR
Command32=ID_VIEW_TABSTOPS
Command33=ID_VIEW_FONT
Command34=ID_HELP_FINDER
Command35=ID_APP_ABOUT
CommandCount=35

[TB:IDR_MAINFRAME]
Type=1
Class=?
Command1=ID_FILE_NEW
Command2=ID_FILE_OPEN
Command3=ID_FILE_SAVE
Command4=ID_EDIT_CUT
Command5=ID_EDIT_COPY
Command6=ID_EDIT_PASTE
Command7=ID_EDIT_FIND
Command8=ID_FILE_PRINT
Command9=ID_SCRIPT_RUN
Command10=ID_SCRIPT_DEBUG
Command11=ID_SCRIPT_BREAK
Command12=ID_SCRIPT_KILL
Command13=ID_FIRST_ERR
Command14=ID_PREV_ERR
Command15=ID_NEXT_ERR
Command16=ID_LAST_ERR
Command17=ID_HELP_FINDER
CommandCount=17

[DLG:IDD_GOTO]
Type=1
Class=?
ControlCount=4
Control1=IDC_GOTO_LINE,edit,1350631552
Control2=IDOK,button,1342242817
Control3=IDCANCEL,button,1342242816
Control4=IDC_STATIC,static,1342308352

[CLS:CEvalView]
Type=0
HeaderFile=evalview.h
ImplementationFile=evalview.cpp
BaseClass=CEditView
LastObject=CEvalView
Filter=C
VirtualFilter=VWC

[DLG:"IDD_TABSTOPS"]
Type=1
Class=?
ControlCount=4
Control1=IDC_TABSTOPS,edit,1350631552
Control2=IDOK,button,1342242817
Control3=IDCANCEL,button,1342242816
Control4=IDC_STATIC,static,1342308352

[DLG:IDD_TABSTOPS]
Type=1
Class=CTabStopDlg
ControlCount=4
Control1=IDC_TABSTOPS,edit,1350631552
Control2=IDOK,button,1342242817
Control3=IDCANCEL,button,1342242816
Control4=IDC_STATIC,static,1342308352

[CLS:CTabStopDlg]
Type=0
HeaderFile=TabStopDlg.h
ImplementationFile=TabStopDlg.cpp
BaseClass=CDialog
Filter=D
LastObject=CTabStopDlg

[DLG:IDD_DIALOGBAR (English (U.S.))]
Type=1
Class=?
ControlCount=1
Control1=IDC_STATIC,static,1342308352

[DLG:IDD_FORMVIEW (English (U.S.))]
Type=1
Class=?
ControlCount=1
Control1=IDC_STATIC,static,1342308352

[DLG:IDD_PROPPAGE_SMALL (English (U.S.))]
Type=1
Class=?
ControlCount=1
Control1=IDC_STATIC,static,1342308352

[CLS:CQPathDlg]
Type=0
HeaderFile=QPathDlg.h
ImplementationFile=QPathDlg.cpp
BaseClass=CDialog
Filter=D
LastObject=CQPathDlg
VirtualFilter=dWC

[DLG:IDD_PROPPAGE_MEDIUM (English (U.S.))]
Type=1
Class=?
ControlCount=2
Control1=IDC_STATIC,static,1342308352
Control2=IDC_TAB1,SysTabControl32,1342177280

[DLG:IDD_ABOUTBOX (English (U.S.))]
Type=1
Class=CAboutDlg
ControlCount=4
Control1=IDC_STATIC,static,1342177283
Control2=IDC_STATIC,static,1342308480
Control3=IDC_STATIC,static,1342308352
Control4=IDOK,button,1342373889

[DLG:IDD_GOTO (English (U.S.))]
Type=1
Class=?
ControlCount=4
Control1=IDC_GOTO_LINE,edit,1350631552
Control2=IDOK,button,1342242817
Control3=IDCANCEL,button,1342242816
Control4=IDC_STATIC,static,1342308352

[DLG:IDD_QPATH (English (U.S.))]
Type=1
Class=CQPathDlg
ControlCount=4
Control1=IDC_QPATH,edit,1350631552
Control2=IDOK,button,1342242817
Control3=IDCANCEL,button,1342242816
Control4=IDC_STATIC,static,1342308352

[DLG:IDD_TABSTOPS (English (U.S.))]
Type=1
Class=CTabStopDlg
ControlCount=4
Control1=IDC_TABSTOPS,edit,1350631552
Control2=IDOK,button,1342242817
Control3=IDCANCEL,button,1342242816
Control4=IDC_STATIC,static,1342308352

[ACL:IDR_MAINFRAME (Neutral)]
Type=1
Class=CMainFrame
Command1=ID_EDIT_SELECT_ALL
Command2=ID_EDIT_COPY
Command3=ID_EDIT_FIND
Command4=ID_EDIT_GOTO
Command5=ID_EDIT_REPLACE
Command6=ID_FILE_NEW
Command7=ID_FILE_OPEN
Command8=ID_SCRIPT_OPEN
Command9=ID_FILE_PRINT
Command10=ID_APP_EXIT
Command11=ID_FILE_SAVE
Command12=ID_EDIT_PASTE
Command13=ID_EDIT_UNDO
Command14=ID_EDIT_CUT
Command15=ID_HIST_NEXT
Command16=ID_HIST_SEARCH_NEXT
Command17=ID_INPUT_LINE
Command18=ID_HELP_FINDER
Command19=ID_NEXT_ERR
Command20=ID_LAST_ERR
Command21=ID_PREV_ERR
Command22=ID_FIRST_ERR
Command23=ID_NEXT_PANE
Command24=ID_PREV_PANE
Command25=ID_SCRIPT_RUN
Command26=ID_SCRIPT_KILL
Command27=ID_SCRIPT_DEBUG
Command28=ID_EDIT_COPY
Command29=ID_EDIT_PASTE
Command30=ID_HIST_LAST
Command31=ID_SCRIPT_BREAK
Command32=ID_HIST_FIRST
Command33=ID_HIST_PREV
Command34=ID_HIST_SEARCH_PREV
Command35=ID_EDIT_CUT
Command36=ID_EDIT_UNDO
CommandCount=36

[TB:IDR_MAINFRAME (Englisch (USA))]
Type=1
Class=?
Command1=ID_FILE_NEW
Command2=ID_FILE_OPEN
Command3=ID_FILE_SAVE
Command4=ID_EDIT_CUT
Command5=ID_EDIT_COPY
Command6=ID_EDIT_PASTE
Command7=ID_EDIT_FIND
Command8=ID_FILE_PRINT
Command9=ID_SCRIPT_RUN
Command10=ID_SCRIPT_DEBUG
Command11=ID_SCRIPT_BREAK
Command12=ID_SCRIPT_KILL
Command13=ID_FIRST_ERR
Command14=ID_PREV_ERR
Command15=ID_NEXT_ERR
Command16=ID_LAST_ERR
Command17=ID_HELP_FINDER
CommandCount=17

[MNU:IDR_MAINFRAME (Englisch (USA))]
Type=1
Class=?
Command1=ID_FILE_NEW
Command2=ID_FILE_OPEN
Command3=ID_FILE_SAVE
Command4=ID_FILE_SAVE_AS
Command5=ID_FILE_PRINT
Command6=ID_FILE_PRINT_PREVIEW
Command7=ID_FILE_PRINT_SETUP
Command8=ID_FILE_MRU_FILE1
Command9=ID_APP_EXIT
Command10=ID_EDIT_UNDO
Command11=ID_EDIT_CUT
Command12=ID_EDIT_COPY
Command13=ID_EDIT_PASTE
Command14=ID_EDIT_SELECT_ALL
Command15=ID_EDIT_FIND
Command16=ID_EDIT_REPLACE
Command17=ID_EDIT_GOTO
Command18=ID_SCRIPT_RUN
Command19=ID_SCRIPT_DEBUG
Command20=ID_SCRIPT_BREAK
Command21=ID_SCRIPT_KILL
Command22=ID_SCRIPT_OPEN
Command23=ID_SCRIPT_PATH
Command24=ID_SCRIPT_PROMPT
Command25=ID_SCRIPT_HISTFILE
Command26=ID_SCRIPT_HISTSIZE
Command27=ID_SCRIPT_RESET
Command28=ID_NEXT_PANE
Command29=ID_PREV_PANE
Command30=ID_INPUT_LINE
Command31=ID_VIEW_TOOLBAR
Command32=ID_VIEW_STATUS_BAR
Command33=ID_VIEW_TABSTOPS
Command34=ID_VIEW_FONT
Command35=ID_HELP_FINDER
Command36=ID_APP_ABOUT
CommandCount=36

[DLG:IDD_ABOUTBOX (Englisch (USA))]
Type=1
Class=CAboutDlg
ControlCount=4
Control1=IDC_STATIC,static,1342177283
Control2=IDC_STATIC,static,1342308480
Control3=IDC_STATIC,static,1342308352
Control4=IDOK,button,1342373889

[DLG:IDD_GOTO (Englisch (USA))]
Type=1
Class=?
ControlCount=4
Control1=IDC_GOTO_LINE,edit,1350631552
Control2=IDOK,button,1342242817
Control3=IDCANCEL,button,1342242816
Control4=IDC_STATIC,static,1342308352

[DLG:IDD_QPATH (Englisch (USA))]
Type=1
Class=CQPathDlg
ControlCount=4
Control1=IDC_QPATH,edit,1350631552
Control2=IDOK,button,1342242817
Control3=IDCANCEL,button,1342242816
Control4=IDC_STATIC,static,1342308352

[DLG:IDD_TABSTOPS (Englisch (USA))]
Type=1
Class=CTabStopDlg
ControlCount=4
Control1=IDC_TABSTOPS,edit,1350631552
Control2=IDOK,button,1342242817
Control3=IDCANCEL,button,1342242816
Control4=IDC_STATIC,static,1342308352

[DLG:IDD_PROMPT]
Type=1
Class=CPromptDlg
ControlCount=4
Control1=IDC_PROMPT,edit,1350631552
Control2=IDOK,button,1342242817
Control3=IDCANCEL,button,1342242816
Control4=IDC_STATIC,static,1342308352

[DLG:IDD_HISTFILE]
Type=1
Class=CHistfileDlg
ControlCount=4
Control1=IDC_HISTFILE,edit,1350631552
Control2=IDOK,button,1342242817
Control3=IDCANCEL,button,1342242816
Control4=IDC_STATIC,static,1342308352

[DLG:IDD_HISTSIZE]
Type=1
Class=CHistsizeDlg
ControlCount=4
Control1=IDC_HISTSIZE,edit,1350631552
Control2=IDOK,button,1342242817
Control3=IDCANCEL,button,1342242816
Control4=IDC_STATIC,static,1342308352

[DLG:IDD_HISTFILE (Englisch (USA))]
Type=1
Class=CHistfileDlg
ControlCount=4
Control1=IDC_HISTFILE,edit,1350631552
Control2=IDOK,button,1342242817
Control3=IDCANCEL,button,1342242816
Control4=IDC_STATIC,static,1342308352

[DLG:IDD_HISTSIZE (Englisch (USA))]
Type=1
Class=CHistsizeDlg
ControlCount=4
Control1=IDC_HISTSIZE,edit,1350631552
Control2=IDOK,button,1342242817
Control3=IDCANCEL,button,1342242816
Control4=IDC_STATIC,static,1342308352

[DLG:IDD_PROMPT (Englisch (USA))]
Type=1
Class=CPromptDlg
ControlCount=4
Control1=IDC_PROMPT,edit,1350631552
Control2=IDOK,button,1342242817
Control3=IDCANCEL,button,1342242816
Control4=IDC_STATIC,static,1342308352

[CLS:CPromptDlg]
Type=0
HeaderFile=PromptDlg.h
ImplementationFile=PromptDlg.cpp
BaseClass=CDialog
Filter=D
VirtualFilter=dWC
LastObject=CPromptDlg

[CLS:CHistfileDlg]
Type=0
HeaderFile=HistfileDlg.h
ImplementationFile=HistfileDlg.cpp
BaseClass=CDialog
Filter=D
VirtualFilter=dWC
LastObject=CHistfileDlg

[CLS:CHistsizeDlg]
Type=0
HeaderFile=HistsizeDlg.h
ImplementationFile=HistsizeDlg.cpp
BaseClass=CDialog
Filter=D
VirtualFilter=dWC

[DLG:IDD_HISTFILE (English (U.S.))]
Type=1
Class=?
ControlCount=4
Control1=IDC_HISTFILE,edit,1350631552
Control2=IDOK,button,1342242817
Control3=IDCANCEL,button,1342242816
Control4=IDC_STATIC,static,1342308352

[DLG:IDD_HISTSIZE (English (U.S.))]
Type=1
Class=?
ControlCount=4
Control1=IDC_HISTSIZE,edit,1350631552
Control2=IDOK,button,1342242817
Control3=IDCANCEL,button,1342242816
Control4=IDC_STATIC,static,1342308352

[DLG:IDD_PROMPT (English (U.S.))]
Type=1
Class=?
ControlCount=4
Control1=IDC_PROMPT,edit,1350631552
Control2=IDOK,button,1342242817
Control3=IDCANCEL,button,1342242816
Control4=IDC_STATIC,static,1342308352

[DLG:IDD_ABOUTBOX (Neutral (Default))]
Type=1
Class=?
ControlCount=4
Control1=IDC_STATIC,static,1342177283
Control2=IDC_STATIC,static,1342308480
Control3=IDC_STATIC,static,1342308352
Control4=IDOK,button,1342373889

[DLG:IDD_GOTO (Neutral (Default))]
Type=1
Class=?
ControlCount=4
Control1=IDC_GOTO_LINE,edit,1350631552
Control2=IDOK,button,1342242817
Control3=IDCANCEL,button,1342242816
Control4=IDC_STATIC,static,1342308352

[DLG:IDD_QPATH (Neutral (Default))]
Type=1
Class=?
ControlCount=4
Control1=IDC_QPATH,edit,1350631552
Control2=IDOK,button,1342242817
Control3=IDCANCEL,button,1342242816
Control4=IDC_STATIC,static,1342308352

[DLG:IDD_TABSTOPS (Neutral (Default))]
Type=1
Class=?
ControlCount=4
Control1=IDC_TABSTOPS,edit,1350631552
Control2=IDOK,button,1342242817
Control3=IDCANCEL,button,1342242816
Control4=IDC_STATIC,static,1342308352

[DLG:IDD_HISTFILE (Neutral (Default))]
Type=1
Class=?
ControlCount=4
Control1=IDC_HISTFILE,edit,1350631552
Control2=IDOK,button,1342242817
Control3=IDCANCEL,button,1342242816
Control4=IDC_STATIC,static,1342308352

[DLG:IDD_HISTSIZE (Neutral (Default))]
Type=1
Class=?
ControlCount=4
Control1=IDC_HISTSIZE,edit,1350631552
Control2=IDOK,button,1342242817
Control3=IDCANCEL,button,1342242816
Control4=IDC_STATIC,static,1342308352

[DLG:IDD_PROMPT (Neutral (Default))]
Type=1
Class=?
ControlCount=4
Control1=IDC_PROMPT,edit,1350631552
Control2=IDOK,button,1342242817
Control3=IDCANCEL,button,1342242816
Control4=IDC_STATIC,static,1342308352

[MNU:IDR_MAINFRAME (Neutral (Default))]
Type=1
Class=?
Command1=ID_FILE_NEW
Command2=ID_FILE_OPEN
Command3=ID_FILE_SAVE
Command4=ID_FILE_SAVE_AS
Command5=ID_FILE_PRINT
Command6=ID_FILE_PRINT_PREVIEW
Command7=ID_FILE_PRINT_SETUP
Command8=ID_FILE_MRU_FILE1
Command9=ID_APP_EXIT
Command10=ID_EDIT_UNDO
Command11=ID_EDIT_CUT
Command12=ID_EDIT_COPY
Command13=ID_EDIT_PASTE
Command14=ID_EDIT_SELECT_ALL
Command15=ID_EDIT_FIND
Command16=ID_EDIT_REPLACE
Command17=ID_EDIT_GOTO
Command18=ID_SCRIPT_RUN
Command19=ID_SCRIPT_DEBUG
Command20=ID_SCRIPT_BREAK
Command21=ID_SCRIPT_KILL
Command22=ID_SCRIPT_OPEN
Command23=ID_SCRIPT_PROMPT
Command24=ID_SCRIPT_HISTFILE
Command25=ID_SCRIPT_HISTSIZE
Command26=ID_SCRIPT_RESET
Command27=ID_NEXT_PANE
Command28=ID_PREV_PANE
Command29=ID_INPUT_LINE
Command30=ID_VIEW_TOOLBAR
Command31=ID_VIEW_STATUS_BAR
Command32=ID_VIEW_TABSTOPS
Command33=ID_VIEW_FONT
Command34=ID_HELP_FINDER
Command35=ID_APP_ABOUT
CommandCount=35

[TB:IDR_MAINFRAME (Neutral (Default))]
Type=1
Class=?
Command1=ID_FILE_NEW
Command2=ID_FILE_OPEN
Command3=ID_FILE_SAVE
Command4=ID_EDIT_CUT
Command5=ID_EDIT_COPY
Command6=ID_EDIT_PASTE
Command7=ID_EDIT_FIND
Command8=ID_FILE_PRINT
Command9=ID_SCRIPT_RUN
Command10=ID_SCRIPT_DEBUG
Command11=ID_SCRIPT_BREAK
Command12=ID_SCRIPT_KILL
Command13=ID_FIRST_ERR
Command14=ID_PREV_ERR
Command15=ID_NEXT_ERR
Command16=ID_LAST_ERR
Command17=ID_HELP_FINDER
CommandCount=17

[TB:IDR_MAINFRAME (Neutral (Standard))]
Type=1
Class=?
Command1=ID_FILE_NEW
Command2=ID_FILE_OPEN
Command3=ID_FILE_SAVE
Command4=ID_EDIT_CUT
Command5=ID_EDIT_COPY
Command6=ID_EDIT_PASTE
Command7=ID_EDIT_FIND
Command8=ID_FILE_PRINT
Command9=ID_SCRIPT_RUN
Command10=ID_SCRIPT_DEBUG
Command11=ID_SCRIPT_BREAK
Command12=ID_SCRIPT_KILL
Command13=ID_FIRST_ERR
Command14=ID_PREV_ERR
Command15=ID_NEXT_ERR
Command16=ID_LAST_ERR
Command17=ID_HELP_FINDER
CommandCount=17

[MNU:IDR_MAINFRAME (Neutral (Standard))]
Type=1
Class=?
Command1=ID_FILE_NEW
Command2=ID_FILE_OPEN
Command3=ID_FILE_SAVE
Command4=ID_FILE_SAVE_AS
Command5=ID_FILE_PRINT
Command6=ID_FILE_PRINT_PREVIEW
Command7=ID_FILE_PRINT_SETUP
Command8=ID_FILE_MRU_FILE1
Command9=ID_APP_EXIT
Command10=ID_EDIT_UNDO
Command11=ID_EDIT_CUT
Command12=ID_EDIT_COPY
Command13=ID_EDIT_PASTE
Command14=ID_EDIT_SELECT_ALL
Command15=ID_EDIT_FIND
Command16=ID_EDIT_REPLACE
Command17=ID_EDIT_GOTO
Command18=ID_SCRIPT_RUN
Command19=ID_SCRIPT_DEBUG
Command20=ID_SCRIPT_BREAK
Command21=ID_SCRIPT_KILL
Command22=ID_SCRIPT_OPEN
Command23=ID_SCRIPT_PROMPT
Command24=ID_SCRIPT_HISTFILE
Command25=ID_SCRIPT_HISTSIZE
Command26=ID_SCRIPT_RESET
Command27=ID_NEXT_PANE
Command28=ID_PREV_PANE
Command29=ID_INPUT_LINE
Command30=ID_VIEW_TOOLBAR
Command31=ID_VIEW_STATUS_BAR
Command32=ID_VIEW_TABSTOPS
Command33=ID_VIEW_FONT
Command34=ID_HELP_FINDER
Command35=ID_APP_ABOUT
CommandCount=35

[DLG:IDD_ABOUTBOX (Neutral (Standard))]
Type=1
Class=?
ControlCount=4
Control1=IDC_STATIC,static,1342177283
Control2=IDC_STATIC,static,1342308480
Control3=IDC_STATIC,static,1342308352
Control4=IDOK,button,1342373889

[DLG:IDD_GOTO (Neutral (Standard))]
Type=1
Class=?
ControlCount=4
Control1=IDC_GOTO_LINE,edit,1350631552
Control2=IDOK,button,1342242817
Control3=IDCANCEL,button,1342242816
Control4=IDC_STATIC,static,1342308352

[DLG:IDD_TABSTOPS (Neutral (Standard))]
Type=1
Class=?
ControlCount=4
Control1=IDC_TABSTOPS,edit,1350631552
Control2=IDOK,button,1342242817
Control3=IDCANCEL,button,1342242816
Control4=IDC_STATIC,static,1342308352

[DLG:IDD_HISTFILE (Neutral (Standard))]
Type=1
Class=?
ControlCount=4
Control1=IDC_HISTFILE,edit,1350631552
Control2=IDOK,button,1342242817
Control3=IDCANCEL,button,1342242816
Control4=IDC_STATIC,static,1342308352

[DLG:IDD_HISTSIZE (Neutral (Standard))]
Type=1
Class=?
ControlCount=4
Control1=IDC_HISTSIZE,edit,1350631552
Control2=IDOK,button,1342242817
Control3=IDCANCEL,button,1342242816
Control4=IDC_STATIC,static,1342308352

[DLG:IDD_PROMPT (Neutral (Standard))]
Type=1
Class=?
ControlCount=4
Control1=IDC_PROMPT,edit,1350631552
Control2=IDOK,button,1342242817
Control3=IDCANCEL,button,1342242816
Control4=IDC_STATIC,static,1342308352

