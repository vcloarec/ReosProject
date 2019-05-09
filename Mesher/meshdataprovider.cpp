/***************************************************************************
                      meshdataprovider.cpp
                     --------------------------------------
Date                 : 01-04-2019
Copyright            : (C) 2018 by Vincent Cloarec
email                : vcloarec at gmail dot com   /  projetreos at gmail dot com
 ***************************************************************************
 *                                                                         *
 *   This program is free software; you can redistribute it and/or modify  *
 *   it under the terms of the GNU General Public License as published by  *
 *   the Free Software Foundation; either version 2 of the License, or     *
 *   (at your option) any later version.                                   *
 *                                                                         *
 ***************************************************************************/

#include "meshdataprovider.h"

TINProvider::TINProvider(const QgsDataProvider::ProviderOptions &providerOption):QgsMeshDataProvider ("",providerOption)
{
    tinEditor.addMeshGenerator(new HdMeshGeneratorTriangleFile());
    tinEditor.setCurrentMeshGenerator("TriangleFile");
}


QgsDataProvider *createMeshEditorProvider(const QString &source, const QgsDataProvider::ProviderOptions &option)
{
    Q_UNUSED(source);
    return new TINProvider(option);
}
