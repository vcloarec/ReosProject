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
    int edgeCount() const override;
    void populateMesh( QgsMesh *mesh ) const override;

    // QgsDataProvider interface
  public:
    QgsCoordinateReferenceSystem crs() const override;
    QgsRectangle extent() const override;
    bool isValid() const override {return true;}
    QString name() const override {return QStringLiteral( "TIN" );}
    QString description() const override {return QString();}
};


class ReosMeshPoviderMetadata: public QgsProviderMetadata
{
  public:
    ReosMeshPoviderMetadata(): QgsProviderMetadata( "mdal", "For editable TIN" )
    {
      init();
    }

    QgsDataProvider *createProvider( const QString &uri, const QgsDataProvider::ProviderOptions &options ) override
    {
      int ncId;
      if ( nc_open( uri.toStdString().c_str(), NC_NOWRITE, &ncId ) == NC_NOERR )
      {
        // can open the file --> the file is a netCDf file
        // query for the Reos metadata
        int ncIdReosMetadataVariable;
        int error = nc_inq_varid( ncId, "Reos-Metadata", &ncIdReosMetadataVariable );

        if ( error == NC_NOERR )
        {
          size_t len;
          const char *attName = "mesh-type";
          error = nc_inq_attlen( ncId, ncIdReosMetadataVariable, attName, &len );
          if ( error == NC_NOERR )
          {
            std::string type( len, '0' );
            error = nc_get_att_text( ncId, ncIdReosMetadataVariable, attName, &type[0] );
            nc_close( ncId );
            if ( error == NC_NOERR && std::strcmp( type.c_str(), "TIN" ) == 0 )
              return new TINProvider( uri, options );
          }
        }
      }

      if ( metaMDAL )
        return metaMDAL->createProvider( uri, options );
      else
        return nullptr;
    }

  private:

    typedef QgsProviderMetadata *factory_function( );

    void init()
    {
      QString mdalLibPath = "./providers/mdal/libmdalprovider.so";
      QLibrary mdalLib( mdalLibPath );
      QFunctionPointer func = mdalLib.resolve( QStringLiteral( "providerMetadataFactory" ).toLatin1().data() );
      factory_function *function = reinterpret_cast< factory_function * >( cast_to_fptr( func ) );
      if ( !function )
        return;

      metaMDAL = function();
    }

    QLibrary mMdalLib;

    QgsProviderMetadata *metaMDAL = nullptr;

};

#endif // MESHDATAPROVIDER_H
