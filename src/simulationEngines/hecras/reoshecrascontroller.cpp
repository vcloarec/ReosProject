/***************************************************************************
  reoshecrascontroller.cpp - ReosHecRasController

 ---------------------
 begin                : 03.10.2022
 copyright            : (C) 2022 by Vincent Cloarec
 email                : vcloarec at gmail dot com
 ***************************************************************************
 *                                                                         *
 *   This program is free software; you can redistribute it and/or modify  *
 *   it under the terms of the GNU General Public License as published by  *
 *   the Free Software Foundation; either version 2 of the License, or     *
 *   (at your option) any later version.                                   *
 *                                                                         *
 ***************************************************************************/

#include <QFileInfo>
#include <QDebug>

#include "reoshecrascontroller.h"

#define MAX_KEY_LENGTH 255
#define MAX_VALUE_NAME 16383

#ifndef UNICODE
typedef std::string String;
#else
typedef std::wstring String;
#endif

#ifdef _WIN32
static QString BSTRToQString( const BSTR &bstr )
{
  int wslen = SysStringLen( bstr );

  int len = WideCharToMultiByte( CP_ACP, 0, static_cast<wchar_t *>( bstr ), wslen, NULL, 0, NULL, NULL );
  std::string dblstr( len, '\0' );

  len = WideCharToMultiByte( CP_ACP, 0, bstr, wslen, &dblstr[0], len, NULL, NULL );
  return QString::fromStdString( dblstr );
}

static std::string WideStringToString( const std::wstring &wstr )
{
  int len = WideCharToMultiByte( CP_ACP, 0, wstr.c_str(), -1, nullptr, 0, nullptr, nullptr );
  std::string ret;
  ret.resize( static_cast<size_t>( len ) + 1 );

  WideCharToMultiByte( CP_ACP, 0, wstr.c_str(), -1, ret.data(), ret.size(), nullptr, nullptr );
  return ret;
}

static std::wstring qStringToWideString( const QString &qstr )
{
  std::string str = qstr.toStdString();
  int len = MultiByteToWideChar( CP_UTF8, 0, str.c_str(), -1, nullptr, 0 );
  std::wstring ret;
  ret.resize( static_cast<size_t>( len ) - 1 );
  MultiByteToWideChar( CP_UTF8, 0, str.c_str(), -1, ret.data(), len );
  return ret;
}

static QString systemStringToQString( const String &sysStr )
{
#ifndef UNICODE
  return QString::fromStdString( sysStr );
#else
  return QString::fromStdString( WideStringToString( sysStr ) );
#endif
}
#endif

QStringList ReosHecrasController::availableVersion()
{
  ///learn.microsoft.com/en-us/windows/win32/sysinfo/enumerating-registry-subkeys
  QStringList ret;
#ifdef _WIN32
  TCHAR    achClass[MAX_PATH] = TEXT( "" );
  DWORD    cchClassName = MAX_PATH;
  DWORD    cSubKeys = 0;
  DWORD    cbMaxSubKey;
  DWORD    cchMaxClass;
  DWORD    cValues;
  DWORD    cchMaxValue;
  DWORD    cbMaxValueData;
  DWORD    cbSecurityDescriptor;
  FILETIME ftLastWriteTime;

  DWORD res = RegQueryInfoKey( HKEY_CLASSES_ROOT,
                               achClass,
                               &cchClassName,
                               nullptr,
                               &cSubKeys,
                               &cbMaxSubKey,
                               &cchClassName,
                               &cValues,
                               &cchMaxValue,
                               &cbMaxValueData,
                               &cbSecurityDescriptor,
                               &ftLastWriteTime );

  if ( cSubKeys )
  {
    DWORD    cbName;
    TCHAR    achKey[MAX_KEY_LENGTH];
    for ( DWORD i = 0; i < cSubKeys; i++ )
    {
      cbName = MAX_KEY_LENGTH;
      res = RegEnumKeyEx( HKEY_CLASSES_ROOT,
                          i,
                          achKey,
                          &cbName,
                          NULL,
                          NULL,
                          NULL,
                          &ftLastWriteTime );
      if ( res == ERROR_SUCCESS )
      {
        String str( achKey );
        ret.append( systemStringToQString( str ) );
      }
    }
  }

  int i = 0;
  while ( i < ret.size() )
  {
    if ( !ret.at( i ).contains( QStringLiteral( ".HECRASController" ) ) )
      ret.removeAt( i );
    else
    {
      ret[i] = ret.at( i ).split( '.' ).first();
      ++i;
    }
  }
#endif
  return ret;
}

ReosHecrasController::ReosHecrasController( const QString &version )
{
#ifdef _WIN32
  if ( !SUCCEEDED( CoInitializeEx( nullptr, COINIT_APARTMENTTHREADED ) ) )
    return;

  CLSID ClassID;
  QString controllerName = version + QStringLiteral( ".HECRASController" );

  std::wstring controllerString = qStringToWideString( controllerName );

  if ( !SUCCEEDED( CLSIDFromProgID( controllerString.c_str(), &ClassID ) ) )
    return;

  if ( !SUCCEEDED( CoCreateInstance( ClassID, nullptr, CLSCTX_LOCAL_SERVER, IID_IDispatch, ( void ** )&mDispatch ) ) )
    return;

  ITypeInfo *typeInfo;
  if ( !SUCCEEDED( mDispatch->GetTypeInfo( 0, 0, &typeInfo ) ) )
    return;

  TYPEATTR *pTatt = nullptr;
  if ( !SUCCEEDED( typeInfo->GetTypeAttr( &pTatt ) ) )
    return;

  FUNCDESC *fd = nullptr;
  for ( int i = 0; i < pTatt->cFuncs; ++i )
  {
    typeInfo->GetFuncDesc( i, &fd );
    DWORD funcName;
    BSTR pBstrName;
    BSTR doc;
    typeInfo->GetDocumentation( fd->memid, &pBstrName, nullptr, nullptr, nullptr );

    if ( pBstrName )
      mFunctionNames.insert( BSTRToQString( pBstrName ), fd->memid );

    QList<VARTYPE> varType;

    TYPEDESC tdesc;
    for ( SHORT pai = 0; pai < fd->cParams; ++pai )
    {
      ELEMDESC *elemDesc = &fd->lprgelemdescParam[pai];
      tdesc = elemDesc->tdesc;
      varType.append( tdesc.vt );
    }

    qDebug() << BSTRToQString( pBstrName ) << fd->elemdescFunc.tdesc.vt << varType;

    typeInfo->ReleaseFuncDesc( fd );

    SysFreeString( pBstrName );
  }

  typeInfo->ReleaseTypeAttr( pTatt );
  typeInfo->Release();

  mIsValid = mFunctionNames.contains( QStringLiteral( "HECRASVersion" ) );
#endif
}

ReosHecrasController::~ReosHecrasController()
{
#ifdef _WIN32
  CoUninitialize();
  if ( mDispatch )
  {
    exitRas();
    mDispatch->Release();
  }
#endif
}

bool ReosHecrasController::isValid() const
{
  return mIsValid;
}

QString ReosHecrasController::version() const
{
#ifdef _WIN32
  DISPID id = mFunctionNames.value( QStringLiteral( "HECRASVersion" ) );

  DISPPARAMS par = {nullptr, nullptr, 0, 0};

  VARIANT result;
  EXCEPINFO excepInfo;
  UINT puArgErr;

  HRESULT res = mDispatch->Invoke( id, IID_NULL, 0, DISPATCH_METHOD, &par, &result, &excepInfo, &puArgErr );

  if ( SUCCEEDED( res ) )
    return BSTRToQString( result.bstrVal );
  else
#endif
    return QString();

}

bool ReosHecrasController::openHecrasProject( const QString &projFileName )
{
  QFileInfo fileInfo( projFileName );
  if ( !fileInfo.exists() )
    return false;
  bool ok = false;
#ifdef _WIN32
  DISPID id = mFunctionNames.value( QStringLiteral( "Project_Open" ) );

  DISPPARAMS par;
  std::vector<VARIANTARG> args;

  //*************************************** Arguments
  VARIANTARG fileNameV;
  VariantInit( &fileNameV );
  std::wstring ws = qStringToWideString( projFileName );
  V_VT( &fileNameV ) = VT_BSTR;
  fileNameV.bstrVal = SysAllocStringLen( ws.c_str(), ws.length() );

  //***************************************
  args.push_back( fileNameV );

  par.cArgs = args.size();
  par.rgvarg = args.data();
  par.cNamedArgs = 0;

  VARIANT result;
  EXCEPINFO excepInfo;
  UINT puArgErr;

  HRESULT res = mDispatch->Invoke( id, IID_NULL, 0, DISPATCH_METHOD, &par, &result, &excepInfo, &puArgErr );

  ok = SUCCEEDED( res );
#endif
  return ok;
}

QStringList ReosHecrasController::planNames() const
{
  QStringList ret;
#ifdef _WIN32
  DISPID id = mFunctionNames.value( QStringLiteral( "Plan_Names" ) );

  VARIANT result;
  EXCEPINFO excepInfo;
  UINT puArgErr;

  DISPPARAMS par;
  std::vector<VARIANTARG> args;

  //*************************************** Arguments
  VARIANTARG pCount;
  VariantInit( &pCount );
  VARIANTARG pNames;
  VariantInit( &pNames );
  VARIANTARG pBool;
  VariantInit( &pBool );

  SAFEARRAYBOUND saBound;
  saBound.lLbound = 0;
  saBound.cElements = 0;

  SAFEARRAY *array = SafeArrayCreate( VT_BSTR, 1, &saBound );
  V_VT( &pNames ) = VT_BSTR | VT_ARRAY | VT_BYREF;
  pNames.pparray = &array;

  LONG count = 0;
  V_VT( &pCount ) = VT_I4 | VT_BYREF;
  V_I4REF( &pCount ) = &count;

  V_VT( &pBool ) = VT_BOOL;
  V_BOOL( &pBool ) = true;

  // Must be in reverse order
  args.push_back( pBool );
  args.push_back( pNames );
  args.push_back( pCount );

  //***************************************

  par.cArgs = args.size();
  par.rgvarg = args.data();
  par.cNamedArgs = 0;

  HRESULT res = mDispatch->Invoke( id, IID_NULL, 0, DISPATCH_METHOD, &par, &result, &excepInfo, &puArgErr );

  if ( SUCCEEDED( res ) )
  {
    res = SafeArrayLock( array );
    if ( SUCCEEDED( res ) )
    {
      BSTR *pData = static_cast<BSTR *>( array->pvData );
      for ( LONG i = 0; i < count; ++i )
      {
        ret.append( BSTRToQString( pData[static_cast<size_t>( i )] ) );
      }
      SafeArrayUnlock( array );
    }

  }

  SafeArrayDestroy( array );
#endif
  return ret;
}

bool ReosHecrasController::exitRas() const
{
#ifdef _WIN32
  DISPID id = mFunctionNames.value( QStringLiteral( "QuitRas" ) );

  VARIANT result;
  EXCEPINFO excepInfo;
  UINT puArgErr;

  DISPPARAMS par = { nullptr, nullptr, 0, 0 };

  HRESULT res = mDispatch->Invoke( id, IID_NULL, 0, DISPATCH_METHOD, &par, &result, &excepInfo, &puArgErr );

  return SUCCEEDED( res );
#else
  return false;
#endif
}

QStringList ReosHecrasController::flowAreas2D() const
{
  QStringList ret;
#ifdef _WIN32
  DISPID id = mFunctionNames.value( QStringLiteral( "Geometry_Get2DFlowAreas" ) );

  VARIANT result;
  EXCEPINFO excepInfo;
  UINT puArgErr;

  DISPPARAMS par;
  std::vector<VARIANTARG> args;

  //*************************************** Arguments
  VARIANTARG pCount;
  VariantInit( &pCount );
  VARIANTARG pNames;
  VariantInit( &pNames );

  SAFEARRAYBOUND saBound;
  saBound.lLbound = 0;
  saBound.cElements = 0;

  SAFEARRAY *array = SafeArrayCreate( VT_BSTR, 1, &saBound );
  V_VT( &pNames ) = VT_BSTR | VT_ARRAY | VT_BYREF;
  pNames.pparray = &array;

  LONG count = 0;
  V_VT( &pCount ) = VT_I4 | VT_BYREF;
  V_I4REF( &pCount ) = &count;

  // Must be in reverse order
  args.push_back( pNames );
  args.push_back( pCount );

  //***************************************

  par.cArgs = args.size();
  par.rgvarg = args.data();
  par.cNamedArgs = 0;

  HRESULT res = mDispatch->Invoke( id, IID_NULL, 0, DISPATCH_METHOD, &par, &result, &excepInfo, &puArgErr );

  if ( SUCCEEDED( res ) )
  {
    res = SafeArrayLock( array );
    if ( SUCCEEDED( res ) )
    {
      BSTR *pData = static_cast<BSTR *>( array->pvData );
      for ( LONG i = 0; i < count; ++i )
      {
        ret.append( BSTRToQString( pData[static_cast<size_t>( i )] ) );
      }
      SafeArrayUnlock( array );
    }
  }

  SafeArrayDestroy( array );
#endif
  return ret;
}

QPolygonF ReosHecrasController::flow2DAreasDomain( const QString &areaName ) const
{
  QPolygonF ret;
#ifdef _WIN32
  DISPID id = mFunctionNames.value( QStringLiteral( "Schematic_D2FlowAreaPolygon" ) );

  VARIANT result;
  EXCEPINFO excepInfo;
  UINT puArgErr;

  DISPPARAMS par;
  std::vector<VARIANTARG> args;

  //*************************************** Arguments
  VARIANTARG pCount;
  VariantInit( &pCount );
  VARIANTARG pX;
  VariantInit( &pX );
  VARIANTARG pY;
  VariantInit( &pY );

  VARIANTARG areaNameV;
  VariantInit( &areaNameV );

  SAFEARRAYBOUND saBound;
  saBound.lLbound = 0;
  saBound.cElements = 0;

  SAFEARRAY *arrayY = SafeArrayCreate( VT_BSTR, 1, &saBound );
  V_VT( &pY ) = VT_R8 | VT_ARRAY | VT_BYREF;
  pY.pparray = &arrayY;

  SAFEARRAY *arrayX = SafeArrayCreate( VT_BSTR, 1, &saBound );
  V_VT( &pX ) = VT_R8 | VT_ARRAY | VT_BYREF;
  pX.pparray = &arrayX;

  LONG count = 0;
  V_VT( &pCount ) = VT_I4 | VT_BYREF;
  V_I4REF( &pCount ) = &count;

  std::wstring ws = qStringToWideString( areaName );
  V_VT( &areaNameV ) = VT_BSTR;
  areaNameV.bstrVal = SysAllocStringLen( ws.c_str(), ws.length() );

  // Must be in reverse order
  args.push_back( pY );
  args.push_back( pX );
  args.push_back( pCount );
  args.push_back( areaNameV );
  //***************************************

  par.cArgs = args.size();
  par.rgvarg = args.data();
  par.cNamedArgs = 0;

  HRESULT res = mDispatch->Invoke( id, IID_NULL, 0, DISPATCH_METHOD, &par, &result, &excepInfo, &puArgErr );

  if ( SUCCEEDED( res ) )
  {
    HRESULT resX = SafeArrayLock( arrayX );
    HRESULT resY = SafeArrayLock( arrayY );
    if ( SUCCEEDED( res ) )
    {
      double *pDataX = static_cast<double *>( arrayX->pvData );
      double *pDataY = static_cast<double *>( arrayY->pvData );
      for ( LONG i = 0; i < count; ++i )
      {
        double x = pDataX[static_cast<size_t>( i )];
        double y = pDataY[static_cast<size_t>( i )];
        ret.append( QPointF( x, y ) );
      }
      SafeArrayUnlock( arrayX );
      SafeArrayUnlock( arrayY );
    }
  }

  SafeArrayDestroy( arrayX );
  SafeArrayDestroy( arrayY );

  if (ret.count() > 0)
      ret.removeLast();
#endif
  return ret;
}
