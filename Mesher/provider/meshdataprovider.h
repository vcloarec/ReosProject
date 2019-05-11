/***************************************************************************
                      meshdataprovider.h
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

#ifndef MESHDATAPROVIDER_H
#define MESHDATAPROVIDER_H

#include <iostream>
#include <sstream>
#include <fstream>

#include <QDebug>
#include <QFileInfo>


#include <qgsmeshdataprovider.h>
#include <qgsprovidermetadata.h>
#include <qgsmeshlayer.h>


#include "hdmesheditor.h"


class HdEditableMeshLayer: public QgsMeshLayer
{
public:
    HdEditableMeshLayer();
};

class TINProvider: public QgsMeshDataProvider
{
public:
    TINProvider(const QgsDataProvider::ProviderOptions &providerOption=QgsDataProvider::ProviderOptions());

    TINEditor *editor()
    {
        return &tinEditor;
    }

    TINEditor tinEditor=TINEditor(mVerticies,mHardLine,mFaces);

private:
    std::vector<Vertex> mVerticies;
    std::vector<Segment> mHardLine;
    std::vector<Face> mFaces;

    // QgsMeshDatasetSourceInterface interface
public:
    bool addDataset(const QString &uri) override {return false;}
    QStringList extraDatasets() const override {return QStringList();}
    int datasetGroupCount() const override {return 1;}
    int datasetCount(int groupIndex) const override {return 1;}

    QgsMeshDatasetGroupMetadata datasetGroupMetadata(int groupIndex) const override
    {
        if (groupIndex!=0)
            return QgsMeshDatasetGroupMetadata();

        QMap<QString,QString> extraOptions;

        extraOptions["By"]="vcloarec";
        return QgsMeshDatasetGroupMetadata(tr("Altitude terrain"),true,true,-5,20,extraOptions);
    }
    QgsMeshDatasetMetadata datasetMetadata(QgsMeshDatasetIndex index) const override
    {
        if (index.group()==0 && index.dataset()==0)
        {
            return QgsMeshDatasetMetadata(0,true,0,5);
        }

        return QgsMeshDatasetMetadata();
    }
    QgsMeshDatasetValue datasetValue(QgsMeshDatasetIndex index, int valueIndex) const override {return QgsMeshDatasetValue();}
    QgsMeshDataBlock datasetValues(QgsMeshDatasetIndex index, int valueIndex, int count) const override {

        if (index.group()==0 && index.dataset()==0)
        {
            QgsMeshDataBlock dataBlock(QgsMeshDataBlock::ScalarDouble,count);

            double *values=static_cast<double*>(dataBlock.buffer());

            for (size_t i=0;i<mVerticies.size();++i)
            {
                values[i]=mVerticies.at(i).z();
            }

            return dataBlock;
        }
        else {
            return QgsMeshDataBlock();
        }


    }
    bool isFaceActive(QgsMeshDatasetIndex index, int faceIndex) const override {
        if (index.group()==0 && index.dataset()==0)
        {
            return true;
        }
        else {
            return false;
        }
    }

    QgsMeshDataBlock areFacesActive(QgsMeshDatasetIndex index, int faceIndex, int count) const override
    {
        if (index.group()==0 && index.dataset()==0)
        {
            QgsMeshDataBlock dataBlock(QgsMeshDataBlock::ActiveFlagInteger,count);

            int *values=static_cast<int*>(dataBlock.buffer());

            for (size_t i=0;i<mFaces.size();++i)
            {
                values[i]=1;
            }

            return dataBlock;
        }
        else {
            return QgsMeshDataBlock();
        }
    }

    bool persistDatasetGroup(const QString &path, const QgsMeshDatasetGroupMetadata &meta, const QVector<QgsMeshDataBlock> &datasetValues, const QVector<QgsMeshDataBlock> &datasetActive, const QVector<double> &times) override {return false;}

    // QgsMeshDataSourceInterface interface
public:
    int vertexCount() const override
    {
        return int(mVerticies.size());
    }
    int faceCount() const override {
        return int(mFaces.size());
    }
    void populateMesh(QgsMesh *mesh) const override;

    // QgsDataProvider interface
public:
    QgsCoordinateReferenceSystem crs() const override {return QgsCoordinateReferenceSystem();}
    QgsRectangle extent() const override {
        if (faceCount()==0)
            return QgsRectangle();

        double xmin=1e99;
        double ymin=1e99;
        double xmax=-1e99;
        double ymax=-1e99;

        for (auto v:mVerticies)
        {
            if (v.x()>=xmax)
                xmax=v.x();
            if (v.y()>=ymax)
                ymax=v.y();
            if (v.x()<=xmin)
                xmin=v.x();
            if (v.y()<=ymin)
                ymin=v.y();
        }

        return QgsRectangle(xmin,ymin,xmax,ymax);
    }
    bool isValid() const override {return true;}
    QString name() const override {return QStringLiteral("TIN");}
    QString description() const override {return QString();}

    // QgsMeshDataSourceInterface interface
public:
//    bool nativeMeshChanged() override
//    {
//        return tinEditor.meshRecentlyChanged();
//    }

};


QgsDataProvider *createMeshEditorProvider(const QString &source,const QgsDataProvider::ProviderOptions &option);

class HdMeshEditorProviderMetaData: public QgsProviderMetadata
{
public:
    HdMeshEditorProviderMetaData():QgsProviderMetadata("TIN","For editable mesh",createMeshEditorProvider) {}

};

#endif // MESHDATAPROVIDER_H
