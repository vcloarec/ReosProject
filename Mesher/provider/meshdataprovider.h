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

#include "../ReosTin/reostin.h"
#include "../ReosMesh/reosmesheditor.h"


class HdTinLayer: public QgsMeshLayer
{
  public:
    HdTinLayer( QString path );
};

class TINProvider: public QgsMeshDataProvider
{
  public:
    TINProvider( const QString &path, const QgsDataProvider::ProviderOptions &providerOption = QgsDataProvider::ProviderOptions() ):
      QgsMeshDataProvider( path, providerOption )
    {
      QFileInfo fileInfo( path );

      if ( fileInfo.exists() )
      {
        mTin.readUGRIDFormat( path.toStdString() );
      }
    }

    ReosTin *tin()
    {
      return &mTin;
    }

    void setCrs( QgsCoordinateReferenceSystem crs )
    {
      QString strCrs = crs.toWkt();
      mTin.setCrs( strCrs.toStdString() );
    }

    static void createEmptyTIN( QString filePath, QgsCoordinateReferenceSystem crs )
    {
      ReosTin tin;
      tin.setCrs( crs.authid().toStdString() );
      tin.writeUGRIDFormat( filePath.toStdString() );
    }

  private:
    ReosTin mTin;

    // QgsMeshDatasetSourceInterface interface
  public:
    bool addDataset( const QString &uri ) override {Q_UNUSED( uri ); return false;}
    QStringList extraDatasets() const override {return QStringList();}
    int datasetGroupCount() const override {return 1;}
    int datasetCount( int groupIndex ) const override {Q_UNUSED( groupIndex ); return 1;}
    QgsMeshDatasetGroupMetadata datasetGroupMetadata( int groupIndex ) const override;
    QgsMeshDatasetMetadata datasetMetadata( QgsMeshDatasetIndex index ) const override;
    QgsMeshDatasetValue datasetValue( QgsMeshDatasetIndex index, int valueIndex ) const override;
    QgsMeshDataBlock datasetValues( QgsMeshDatasetIndex index, int valueIndex, int count ) const override;
    bool isFaceActive( QgsMeshDatasetIndex index, int faceIndex ) const override;
    QgsMeshDataBlock areFacesActive( QgsMeshDatasetIndex index, int faceIndex, int count ) const override;
    bool persistDatasetGroup( const QString &path, const QgsMeshDatasetGroupMetadata &meta, const QVector<QgsMeshDataBlock> &datasetValues, const QVector<QgsMeshDataBlock> &datasetActive, const QVector<double> &times ) override;
    QgsMesh3dDataBlock dataset3dValues( QgsMeshDatasetIndex index, int faceIndex, int count ) const override;
    // QgsMeshDataSourceInterface interface
  public:
    int vertexCount() const override;
    int faceCount() const override;
    void populateMesh( QgsMesh *mesh ) const override;

    // QgsDataProvider interface
  public:
    QgsCoordinateReferenceSystem crs() const override;
    QgsRectangle extent() const override;
    bool isValid() const override {return true;}
    QString name() const override {return QStringLiteral( "TIN" );}
    QString description() const override {return QString();}

};


QgsDataProvider *createTinEditorProvider( const QString &source, const QgsDataProvider::ProviderOptions &option );

class HdTinEditorProviderMetaData: public QgsProviderMetadata
{
  public:
    HdTinEditorProviderMetaData(): QgsProviderMetadata( "TIN", "For editable mesh", createTinEditorProvider ) {}

};

#endif // MESHDATAPROVIDER_H
