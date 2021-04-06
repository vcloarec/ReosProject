;NSIS Modern User Interface
;Header Bitmap Example Script
;Written by Joost Verburg
;edited par Vincent Cloarec

Unicode True

!define LEKAN_VERSION "1.99.3"
!define PACKAGE_SOURCE "D:\ReosProject-release"

;--------------------------------
;Include Modern UI

  !include "MUI2.nsh"
  !include "WinVer.nsh"

;--------------------------------
;General

  SetCompressor /SOLID lzma

  ;Name and file
  Name "Lekan"
  OutFile "${PACKAGE_SOURCE}\Lekan-${LEKAN_VERSION}-install-win64.exe"

  ;Default installation folder
  InstallDir "$PROGRAMFILES64\ReosProject"
  
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
CreateShortcut "$desktop\Lekan.lnk" "$instdir\bin\Lekan.exe"
FunctionEnd

!define MUI_FINISHPAGE_SHOWREADME ""
!define MUI_FINISHPAGE_SHOWREADME_NOTCHECKED
!define MUI_FINISHPAGE_SHOWREADME_TEXT "Créer un raccourci sur le bureau"
!define MUI_FINISHPAGE_SHOWREADME_FUNCTION finishpageaction 

;--------------------------------
;Pages

  Var SMDir
  !insertmacro MUI_PAGE_LICENSE "..\..\..\LICENSE_LEKAN.txt"
  !insertmacro MUI_PAGE_DIRECTORY
  !insertmacro MUI_PAGE_STARTMENU 0 $SMDir
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
  File /r "${PACKAGE_SOURCE}\${LEKAN_VERSION}\*.*"

  ;Store installation folder
  WriteRegStr HKLM "Lekan 2" "" $INSTDIR
  
  ;Create uninstaller
  WriteUninstaller "$INSTDIR\Uninstall.exe"
  
  WriteRegStr HKLM "Software\Microsoft\Windows\CurrentVersion\Uninstall\Lekan2" \
                 "DisplayName" "Lekan 2"
  WriteRegStr HKLM "Software\Microsoft\Windows\CurrentVersion\Uninstall\Lekan2" \
                 "UninstallString" "$\"$INSTDIR\uninstall.exe$\""
  
  
!insertmacro MUI_STARTMENU_WRITE_BEGIN 0 ;This macro sets $SMDir and skips to MUI_STARTMENU_WRITE_END if the "Don't create shortcuts" checkbox is checked... 
CreateDirectory "$SMPROGRAMS\$SMDir"
SetOutPath "$INSTDIR\bin"
CreateShortCut "$SMPROGRAMS\$SMDir\Lekan.lnk" "$INSTDIR\bin\Lekan.exe"
!insertmacro MUI_STARTMENU_WRITE_END

SectionEnd


;Uninstaller Section

Section "Uninstall"

  ;ADD YOUR OWN FILES HERE...
  SetShellVarContext all
  
  RMDir /r "$INSTDIR"

  Delete "$INSTDIR\Uninstall.exe"
  
  RMDir "$INSTDIR"
    
  Delete "$SMPROGRAMS\$SMDir\Lekan.lnk"
  RMDir "$SMPROGRAMS\$SMDir"

  DeleteRegKey /ifempty HKCU "Lekan"

SectionEnd
