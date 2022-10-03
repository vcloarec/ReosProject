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

#include <combaseapi.h>
#include <qDebug>

#include "reoshecrascontroller.h"

ReosHecrasController::ReosHecrasController()
{

    if (!SUCCEEDED(CoInitializeEx(nullptr, COINIT_APARTMENTTHREADED)))
        return;

    CLSID ClassID;
    if (!SUCCEEDED(CLSIDFromProgID(OLESTR("RAS507.HECRASController"), &ClassID)))
        return;

    mIsValid = SUCCEEDED(CoCreateInstance(ClassID, nullptr, CLSCTX_ALL, IID_IDispatch, (void**)&mDispatch));

    ITypeInfo* typeInfo;

    mDispatch->GetTypeInfo(0, 0, &typeInfo);
    TYPEATTR* pTatt = nullptr;
    typeInfo->GetTypeAttr(&pTatt);
    FUNCDESC* fd = nullptr;
    std::vector<std::wstring> functions;
    for (int i = 0; i < pTatt->cFuncs; ++i)
    {
        typeInfo->GetFuncDesc(i, &fd);
        DWORD funcName;
        BSTR* pBstrName;
        typeInfo->GetDocumentation(fd->memid, pBstrName, nullptr, nullptr, nullptr);

        if (pBstrName)
        {
            std::wstring ws(*pBstrName, SysStringLen(*pBstrName));
            functions.push_back(ws);
        }

        qDebug() << "**";
    }

}

ReosHecrasController::~ReosHecrasController()
{
    CoUninitialize();
    mDispatch->Release();
}

bool ReosHecrasController::isValid() const
{
    return mIsValid;
}
