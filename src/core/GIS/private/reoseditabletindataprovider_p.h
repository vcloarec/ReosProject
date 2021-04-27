/***************************************************************************
  reoseditabletindataprovider.h - ReosEditableTinDataProvider

 ---------------------
 begin                : 12.4.2021
 copyright            : (C) 2021 by Vincent Cloarec
 email                : vcloarec at gmail dot com
 ***************************************************************************
 *                                                                         *
 *   This program is free software; you can redistribute it and/or modify  *
 *   it under the terms of the GNU General Public License as published by  *
 *   the Free Software Foundation; either version 2 of the License, or     *
 *   (at your option) any later version.                                   *
 *                                                                         *
 ***************************************************************************/
#ifndef REOSEDITABLETINDATAPROVIDER_H
#define REOSEDITABLETINDATAPROVIDER_H


#include <qgsmeshdataprovider.h>
#include <qgsprovidermetadata.h>

#include "reostriangularirregularnetworkqgsdualedge_p.h"


class ReosEditableTinDataProvider : public QgsMeshDataProvider
{
    Q_OBJECT
  public:
    ReosEditableTinDataProvider();

    // QgsMeshDatasetSourceInterface interface
  public:
    bool addDataset( const QString & ) override {return false;}
    QStringList extraDatasets() const override {return QStringList();}
    int datasetGroupCount() const override {return 1;}
    int datasetCount( int ) const override {return 1;}
    QgsMeshDatasetGroupMetadata datasetGroupMetadata( int ) const override;
    QgsMeshDatasetMetadata datasetMetadata( QgsMeshDatasetIndex ) const override;
    QgsMeshDatasetValue datasetValue( QgsMeshDatasetIndex index, int valueIndex ) const override;
    QgsMeshDataBlock datasetValues( QgsMeshDatasetIndex index, int valueIndex, int count ) const override;

    QgsMesh3dDataBlock dataset3dValues( QgsMeshDatasetIndex, int, int ) const  override {return QgsMesh3dDataBlock();}
    bool isFaceActive( QgsMeshDatasetIndex, int ) const  override {return true;}

    QgsMeshDataBlock areFacesActive( QgsMeshDatasetIndex index, int faceIndex, int count ) const override;
    bool persistDatasetGroup( const QString &, const QString &, const QgsMeshDatasetGroupMetadata &, const QVector<QgsMeshDataBlock> &, const QVector<QgsMeshDataBlock> &, const QVector<double> & ) override {return false;}
    bool persistDatasetGroup( const QString &, const QString &, QgsMeshDatasetSourceInterface *, int ) override {return false;}

    // QgsMeshDataSourceInterface interface
  public:
    int vertexCount() const override;
    int faceCount() const override;
    int edgeCount() const override {return 0;};
    void populateMesh( QgsMesh *mesh ) const override;
    void updateMesh( QgsMesh *mesh, QgsRectangle &updatedExtent ) const override;

    // QgsDataProvider interface
  public:
    QgsCoordinateReferenceSystem crs() const {return QgsCoordinateReferenceSystem();}
    QgsRectangle extent() const override;
    bool isValid() const override {return mTriangulation;}
    QString name() const override {return QStringLiteral( "ReosTinEditor" );}
    QString description() const override {return QString();}

    //! Return the TIN object used to handle the vertices/faces
    ReosTriangularIrregularNetwork *triangulation();

  private:
    ReosTriangularIrregularNetworkQgsDualEdge_p *mTriangulation = nullptr;
    double minimumElevation() const;
    double maximumElevation() const;
};

class ReosEditableTinProviderMetadata: public QgsProviderMetadata
{
  public:
    ReosEditableTinProviderMetadata(): QgsProviderMetadata( QStringLiteral( "ReosTinEditor" ), QString() ) {}
    QString filters( FilterType ) override {return QString();}
    QList<QgsMeshDriverMetadata> meshDriversMetadata() override {return QList<QgsMeshDriverMetadata>();}
    ReosEditableTinDataProvider *createProvider( const QString &, const QgsDataProvider::ProviderOptions &options, QgsDataProvider::ReadFlags flags = QgsDataProvider::ReadFlags() ) override
    {
      return new ReosEditableTinDataProvider();
    }
    QList<QgsDataItemProvider *> dataItemProviders() const override {return QList<QgsDataItemProvider *>(); }
    bool createMeshData( const QgsMesh &mesh,
                         const QString uri,
                         const QString &driverName,
                         const QgsCoordinateReferenceSystem &crs ) const override {return false;}
    QVariantMap decodeUri( const QString &uri ) const override {return QVariantMap();}
    QString encodeUri( const QVariantMap &parts ) const override {return QString();}
    ProviderCapabilities providerCapabilities() const override
    {
      return FileBasedUris;
    }

};

#endif // REOSEDITABLETINDATAPROVIDER_H
