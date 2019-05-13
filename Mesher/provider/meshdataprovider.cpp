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



QgsMeshDatasetGroupMetadata TINProvider::datasetGroupMetadata(int groupIndex) const
{
    if (groupIndex!=0)
        return QgsMeshDatasetGroupMetadata();

    QMap<QString,QString> extraOptions;

    extraOptions["By"]="vcloarec";
    return QgsMeshDatasetGroupMetadata(tr("Altitude terrain"),true,true,-5,20,extraOptions);
}

QgsMeshDatasetMetadata TINProvider::datasetMetadata(QgsMeshDatasetIndex index) const
{
    if (index.group()==0 && index.dataset()==0)
    {
        return QgsMeshDatasetMetadata(0,true,0,5);
    }

    return QgsMeshDatasetMetadata();
}

QgsMeshDatasetValue TINProvider::datasetValue(QgsMeshDatasetIndex index, int valueIndex) const {Q_UNUSED(index);Q_UNUSED(valueIndex);return QgsMeshDatasetValue();}

QgsMeshDataBlock TINProvider::datasetValues(QgsMeshDatasetIndex index, int valueIndex, int count) const {
    Q_UNUSED(valueIndex)

    if (index.group()==0 && index.dataset()==0)
    {
        QgsMeshDataBlock dataBlock(QgsMeshDataBlock::ScalarDouble,count);

        double *values=static_cast<double*>(dataBlock.buffer());

        for (int i=0;i<mMesh.verticesCount();++i)
        {
            values[i]=mMesh.vertex(i)->z();
        }

        return dataBlock;
    }
    else {
        return QgsMeshDataBlock();
    }


}

bool TINProvider::isFaceActive(QgsMeshDatasetIndex index, int faceIndex) const {
    Q_UNUSED(faceIndex);
    if (index.group()==0 && index.dataset()==0)
    {
        return true;
    }
    else {
        return false;
    }
}

QgsMeshDataBlock TINProvider::areFacesActive(QgsMeshDatasetIndex index, int faceIndex, int count) const
{
    Q_UNUSED(faceIndex);
    if (index.group()==0 && index.dataset()==0)
    {
        QgsMeshDataBlock dataBlock(QgsMeshDataBlock::ActiveFlagInteger,count);

        int *values=static_cast<int*>(dataBlock.buffer());

        for (int i=0;i<mMesh.facesCount();++i)
        {
            values[i]=1;
        }

        return dataBlock;
    }
    else {
        return QgsMeshDataBlock();
    }
}

bool TINProvider::persistDatasetGroup(const QString &path, const QgsMeshDatasetGroupMetadata &meta, const QVector<QgsMeshDataBlock> &datasetValues, const QVector<QgsMeshDataBlock> &datasetActive, const QVector<double> &times)
{
    Q_UNUSED(path);
    Q_UNUSED(meta);
    Q_UNUSED(datasetValues);
    Q_UNUSED(datasetActive);
    Q_UNUSED(times);
    return false;
}

int TINProvider::vertexCount() const
{
    return mMesh.verticesCount();
}

int TINProvider::faceCount() const {
    return mMesh.facesCount();
}

void TINProvider::populateMesh(QgsMesh *mesh) const
{
    if (!mesh)
        return;

    mesh->vertices.clear();
    mesh->faces.clear();

    for (int i=0;i<mMesh.verticesCount();++i)
    {
        auto v=mMesh.vertex(i);
        if (v)
            mesh->vertices.append(QgsMeshVertex(v->x(),v->y()));
    }

    for (int i=0;i<mMesh.facesCount();++i)
    {
        QgsMeshFace mf;
        FacePointer f=mMesh.face(i);
        if (f)
        {
            for (int j=0;j<f->verticesCount();++j)
            {
                mf.append(mMesh.index(f->vertex(j)));
            }
            mesh->faces.append(mf);
        }
    }

}

QgsRectangle TINProvider::extent() const {
    if (faceCount()==0)
        return QgsRectangle();

    double xmin=1e99;
    double ymin=1e99;
    double xmax=-1e99;
    double ymax=-1e99;

    for (int i=0;i<mMesh.verticesCount();++i)
    {
        auto v=mMesh.vertex(i);
        if (v->x()>=xmax)
            xmax=v->x();
        if (v->y()>=ymax)
            ymax=v->y();
        if (v->x()<=xmin)
            xmin=v->x();
        if (v->y()<=ymin)
            ymin=v->y();
    }

    return QgsRectangle(xmin,ymin,xmax,ymax);
}


QgsDataProvider *createTinEditorProvider(const QString &source, const QgsDataProvider::ProviderOptions &option)
{
    Q_UNUSED(source);
    return new TINProvider(option);
}

HdEditableMeshLayer::HdEditableMeshLayer():QgsMeshLayer("-","Editable mesh layer","TIN")
{
}
