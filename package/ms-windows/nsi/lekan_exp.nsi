;NSIS Modern User Interface
;Header Bitmap Example Script
;Written by Joost Verburg
;edited par Vincent Cloarec

Unicode True

!define LEKAN_VERSION "$%LEKAN_EXPERIMENTAL_VERSION%"
!define PACKAGE_SOURCE "$%NSI_DESTINATION%"
!define PATH_TO_FILES "$%REOS_INSTALL%"

;--------------------------------
;Include Modern UI

  !include "MUI2.nsh"
  !include "WinVer.nsh"

;--------------------------------
;General

  SetCompressor /SOLID lzma

  ;Name and file
  Name "Lekan exp"
  OutFile "${PACKAGE_SOURCE}\Lekan-${LEKAN_VERSION}-install-win64.exe"

  ;Default installation folder
  InstallDir "$PROGRAMFILES64\ReosProject-exp"
  
  ;Get installation folder from registry if available
  ;InstallDirRegKey HKCU "Software\Modern UI Test" ""

  ;Request application privileges for Windows Vista/7 and later
  RequestExecutionLevel none

;--------------------------------
;Interface Configuration
  !define MUI_ICON "lekan.ico"
  !define MUI_UNICON "lekan_remove.ico"
  !define MUI_HEADERIMAGE
  !define MUI_HEADERIMAGE_BITMAP "bannerLekan.bmp"
  !define MUI_ABORTWARNING

  
  ;Finish page action
Function finishpageaction
CreateShortcut "$desktop\Lekan exp.lnk" "$instdir\bin\Lekan.exe"
FunctionEnd

!define MUI_FINISHPAGE_SHOWREADME ""
!define MUI_FINISHPAGE_SHOWREADME_NOTCHECKED
!define MUI_FINISHPAGE_SHOWREADME_TEXT "Create shortcut on the desktop"
!define MUI_FINISHPAGE_SHOWREADME_FUNCTION finishpageaction 

;--------------------------------
;Pages

  Var SMDir
  !insertmacro MUI_PAGE_DIRECTORY
  !insertmacro MUI_PAGE_STARTMENU 0 $SMDir
  !insertmacro MUI_PAGE_LICENSE "..\..\..\LICENSE_LEKAN.txt"
  !insertmacro MUI_PAGE_LICENSE "..\..\..\external\telemac\LICENSE.txt"
  !insertmacro MUI_PAGE_INSTFILES
  !insertmacro MUI_PAGE_FINISH
  
  !insertmacro MUI_UNPAGE_CONFIRM
  !insertmacro MUI_UNPAGE_INSTFILES
  !insertmacro MUI_UNPAGE_FINISH
  
;-------------------------
;  Languages Files
  !insertmacro MUI_LANGUAGE "English"
  !insertmacro MUI_LANGUAGE "French"

;--------------------------------
;Installer Sections

Section "Install" SecInstall

  SetOutPath "$INSTDIR"
  
  ;Path to file
  File /r "${PATH_TO_FILES}\*.*"

  ;Store installation folder
  WriteRegStr HKLM "Lekan exp 2" "" $INSTDIR
  
  ;Create uninstaller
  WriteUninstaller "$INSTDIR\Uninstall.exe"
  
  WriteRegStr HKLM "Software\Microsoft\Windows\CurrentVersion\Uninstall\LekanExp2" \
                 "DisplayName" "Lekan exp 2"
  WriteRegStr HKLM "Software\Microsoft\Windows\CurrentVersion\Uninstall\LekanExp2" \
                 "UninstallString" "$\"$INSTDIR\uninstall.exe$\""
  
  
!insertmacro MUI_STARTMENU_WRITE_BEGIN 0 ;This macro sets $SMDir and skips to MUI_STARTMENU_WRITE_END if the "Don't create shortcuts" checkbox is checked... 
CreateDirectory "$SMPROGRAMS\$SMDir"
SetOutPath "$INSTDIR\bin"
CreateShortCut "$SMPROGRAMS\$SMDir\Lekan exp.lnk" "$INSTDIR\bin\Lekan.exe"
!insertmacro MUI_STARTMENU_WRITE_END

SectionEnd


;Uninstaller Section

Section "Uninstall"

  ;ADD YOUR OWN FILES HERE...
  SetShellVarContext all
  
  RMDir /r "$INSTDIR"

  Delete "$INSTDIR\Uninstall.exe"
  
  RMDir "$INSTDIR"
    
  Delete "$SMPROGRAMS\$SMDir\Lekan exp.lnk"
  RMDir "$SMPROGRAMS\$SMDir"

  DeleteRegKey /ifempty HKCU "Lekan"

SectionEnd
