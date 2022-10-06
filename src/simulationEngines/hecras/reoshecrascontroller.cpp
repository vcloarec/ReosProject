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

#include <qDebug>
#include <QFileInfo>

#include "reoshecrascontroller.h"

#define MAX_KEY_LENGTH 255
#define MAX_VALUE_NAME 16383

#ifndef UNICODE  
typedef std::string String;
#else
typedef std::wstring String;
#endif


static QString BSTRToQString(BSTR bstr)
{
    int wslen = SysStringLen(bstr);

    int len = WideCharToMultiByte(CP_ACP, 0, static_cast<wchar_t*>(bstr), wslen, NULL, 0, NULL, NULL);
    std::string dblstr(len, '\0');

    len = WideCharToMultiByte(CP_ACP, 0,bstr, wslen,&dblstr[0], len, NULL, NULL);
    return QString::fromStdString(dblstr);
}

static std::string WideStringToString(const std::wstring &wstr)
{
    int len = WideCharToMultiByte(CP_ACP, 0, wstr.c_str(), -1, nullptr, 0, nullptr, nullptr);
    std::string ret;
    ret.resize(static_cast<size_t>(len) + 1);

    WideCharToMultiByte(CP_ACP, 0, wstr.c_str(), -1, ret.data(), ret.size(), nullptr, nullptr);
    return ret;
}

static std::wstring qStringToWideString(const QString &qstr)
{
    std::string str = qstr.toStdString();
    int len = MultiByteToWideChar(CP_UTF8, 0, str.c_str(), -1, nullptr, 0);
    std::wstring ret;
    ret.resize(static_cast<size_t>(len) + 1);

    MultiByteToWideChar(CP_UTF8, 0, str.c_str(), -1, ret.data(), ret.size());
    return ret;
}

static QString systemStringToQString(const String& sysStr)
{
#ifndef UNICODE  
    return QString::fromStdString(sysStr);
#else
    return QString::fromStdString(WideStringToString(sysStr));
#endif
}

QStringList ReosHecrasController::availableVersion()
{
    ///learn.microsoft.com/en-us/windows/win32/sysinfo/enumerating-registry-subkeys
    QStringList ret;

    TCHAR    achClass[MAX_PATH] = TEXT("");
    DWORD    cchClassName = MAX_PATH;
    DWORD    cSubKeys = 0;
    DWORD    cbMaxSubKey;
    DWORD    cchMaxClass;
    DWORD    cValues;
    DWORD    cchMaxValue;
    DWORD    cbMaxValueData;
    DWORD    cbSecurityDescriptor;
    FILETIME ftLastWriteTime;

    DWORD res = RegQueryInfoKey(HKEY_CLASSES_ROOT,
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
        &ftLastWriteTime);

    qDebug() <<"------------------- keys"<< cSubKeys;

    if (cSubKeys)
    {
        DWORD    cbName;
        TCHAR    achKey[MAX_KEY_LENGTH];
        for (DWORD i = 0; i < cSubKeys; i++)
        {
            cbName = MAX_KEY_LENGTH;
            res = RegEnumKeyEx(HKEY_CLASSES_ROOT,
                i,
                achKey,
                &cbName,
                NULL,
                NULL,
                NULL,
                &ftLastWriteTime);
            if (res == ERROR_SUCCESS)
            {
                String str(achKey);
                ret.append(systemStringToQString(str));
            }
        }
    }

    int i = 0;
    while (i < ret.size())
    {
        if (!ret.at(i).contains(QStringLiteral(".HECRASController")))
            ret.removeAt(i);
        else
        {
            ret[i] = ret.at(i).split('.').first();
            ++i;
        }
    }
    return ret;
}

ReosHecrasController::ReosHecrasController(const QString& version)
{
    if (!SUCCEEDED(CoInitializeEx(nullptr, COINIT_APARTMENTTHREADED)))
        return;

    CLSID ClassID;
    QString controllerName = version + QStringLiteral(".HECRASController");

    std::wstring controllerString = qStringToWideString(controllerName);
    
    if (!SUCCEEDED(CLSIDFromProgID(controllerString.c_str(), &ClassID)))
        return;

    if (!SUCCEEDED(CoCreateInstance(ClassID, nullptr, CLSCTX_LOCAL_SERVER, IID_IDispatch, (void**)&mDispatch)))
        return;

    ITypeInfo* typeInfo;
    if (!SUCCEEDED(mDispatch->GetTypeInfo(0, 0, &typeInfo)))
        return;
     
    TYPEATTR* pTatt = nullptr;
    if (!SUCCEEDED(typeInfo->GetTypeAttr(&pTatt)))
            return;

    FUNCDESC* fd = nullptr;
    for (int i = 0; i < pTatt->cFuncs; ++i)
    {
        typeInfo->GetFuncDesc(i, &fd);
        DWORD funcName;
        BSTR pBstrName;
        BSTR doc;
        typeInfo->GetDocumentation(fd->memid, &pBstrName, nullptr, nullptr,nullptr);
      
        if (pBstrName)
            mFunctionNames.insert(BSTRToQString(pBstrName), fd->memid);

        QList<VARTYPE> varType;

        TYPEDESC tdesc;
        for (SHORT pai = 0; pai < fd->cParams; ++pai)
        {
            ELEMDESC* elemDesc = &fd->lprgelemdescParam[pai];
            tdesc = elemDesc->tdesc;
            varType.append(tdesc.vt);
        }

        qDebug() << BSTRToQString(pBstrName) << fd->elemdescFunc.tdesc.vt<< varType;

        typeInfo->ReleaseFuncDesc(fd);

        SysFreeString(pBstrName);
    }

    typeInfo->ReleaseTypeAttr(pTatt);
    typeInfo->Release();

    mIsValid = mFunctionNames.contains(QStringLiteral("HECRASVersion"));
}

ReosHecrasController::~ReosHecrasController()
{
    CoUninitialize();
    if (mDispatch)
        mDispatch->Release();
}

bool ReosHecrasController::isValid() const
{
    return mIsValid;
}

VARIANT ReosHecrasController::invokeFunction(DISPID id, const Parameters& params, bool &ok) const
{
    VARIANT result;
    EXCEPINFO excepInfo;
    UINT puArgErr;

    HRESULT res = mDispatch->Invoke(id, IID_NULL, 0, DISPATCH_METHOD, params.funcParameters(), &result, &excepInfo, &puArgErr);
    ok=SUCCEEDED(res);
    return result;
}

QString ReosHecrasController::version() const
{
    DISPID id=mFunctionNames.value(QStringLiteral("HECRASVersion"));
    
    bool ok = false;
    return BSTRToQString(invokeFunction(id, Parameters(),ok).bstrVal);
}

bool ReosHecrasController::openHecrasProject(const QString& projFileName)
{
    QFileInfo fileInfo(projFileName);
    if (!fileInfo.exists())
        return false;

    DISPID id = mFunctionNames.value(QStringLiteral("Project_Open"));

    Parameters params;
    params.addStringParameter(projFileName);
    bool ok = false;
    invokeFunction(id, params,ok);
    return ok;
}

QStringList ReosHecrasController::flowAreas2D(bool &ok) const
{

    DISPID dispid[5];
    dispid[1] = 125;
    OLECHAR* szMember = L"newProjectName ";

    Parameters params;
    LONG count;
    //params.prepareReturnLong(count);
    size_t strIndex=params.prepareReturnString();
    DISPID id = mFunctionNames.value(QStringLiteral("Geometry_Get2DFlowAreas"));

    VARIANT result;
    EXCEPINFO excepInfo;
    UINT puArgErr;

    DISPPARAMS par;
    std::vector<VARIANTARG> args;
    VARIANTARG p1;
    VariantInit(&p1);
    VARIANTARG p2;
    VariantInit(&p2);
    BSTR nam= SysAllocString(L"C:/dev/sources/ReosProject/TestsHecRas/testData/simple/simple_v2.prj");

    V_VT(&p1) = VT_BSTR | VT_BYREF;
    V_BSTRREF(&p1) = &nam;

    V_VT(&p2) = VT_I4 | VT_BYREF;
    V_I4REF(&p2) = &count;


    args.push_back(p1);
    //args.push_back(p2);
    par.cArgs = args.size();
    par.rgvarg = args.data();

    std::vector<DISPID> argsId;

    DISPPARAMS* pdispparams;

    VARIANTARG varg0;

    HRESULT res = mDispatch->Invoke(id, IID_NULL, 0, DISPATCH_METHOD, &par, &result, &excepInfo, &puArgErr);

    ok = SUCCEEDED(res);

    invokeFunction(id, params, ok);
    return QStringList();
}

inline ReosHecrasController::Parameters::~Parameters()
{
    for (BSTR bstr : mStringParams)
        SysFreeString(bstr);
}

DISPPARAMS* ReosHecrasController::Parameters::funcParameters() const
{
    mParams =DISPPARAMS();
    mParams.rgvarg = args.data();
    mParams.rgdispidNamedArgs = argsId.data();
    mParams.cArgs = args.size();
    mParams.cNamedArgs = argsId.size();

    return &mParams;
}

void ReosHecrasController::Parameters::addStringParameter(const QString& string)
{
    std::wstring ws = qStringToWideString(string);
    VARIANT var;
    VariantInit(&var);
    args.push_back(var);
    args[args.size() - 1].bstrVal = SysAllocStringLen(ws.data(), ws.size());
    mStringParams.push_back(args[args.size() - 1].bstrVal);
}

size_t ReosHecrasController::Parameters::prepareReturnString()
{
    VARIANT var;
    VariantInit(&var);
    var.vt = 26;
    args.push_back(var);
    return args.size() - 1;
}

void ReosHecrasController::Parameters::freeString(size_t index)
{
    SysFreeString(*args.at(index).pbstrVal);
}

void ReosHecrasController::Parameters::prepareReturnLong(LONG& value)
{
    VARIANT var;
    VariantInit(&var);
    V_VT(&var) = 26;
    V_INT(&var) = value;
    args.push_back(var);
    args[args.size() - 1].plVal = &value;
}
