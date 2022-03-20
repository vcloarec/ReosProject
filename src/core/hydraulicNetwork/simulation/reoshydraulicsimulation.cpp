/***************************************************************************
  reoshydraulicsimulation.cpp - ReosHydraulicSimulation

 ---------------------
 begin                : 19.3.2022
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
#include "reoshydraulicsimulation.h"

#include <qgsmeshdataset.h>
#include <qgsmeshlayer.h>
#include <qgsproviderregistry.h>
#include <qgsmeshtriangulation.h>

#include "reoshydraulicstructure2d.h"


ReosHydraulicSimulation::ReosHydraulicSimulation()
{

}

void ReosHydraulicSimulation::prepareInput( ReosHydraulicStructure2D *hydraulicStructure )
{
  QgsMeshLayer *meshLayer = qobject_cast<QgsMeshLayer *> ( hydraulicStructure->mesh()->data() );
  if ( !meshLayer )
    return;

  const QgsMesh &mesh = *meshLayer->nativeMesh();

  QDir dir = hydraulicStructure->structureDirectory();

  dir.mkdir( "TELEMAC_simulation" );
  dir.cd( "TELEMAC_simulation" );
  QString path = dir.path();
  path = path + "/geom_input.slf";

  QgsProviderMetadata *meta = QgsProviderRegistry::instance()->providerMetadata( QStringLiteral( "mdal" ) );
  if ( !meta )
    return;

  meta->createMeshData( mesh, path, QStringLiteral( "SELAFIN" ), meshLayer->crs() );

  std::unique_ptr<QgsMeshLayer> ouputMesh = std::make_unique < QgsMeshLayer>(
        path,
        QStringLiteral( "temp" ),
        QStringLiteral( "mdal" ) );

  //! Terrain elevation
  QgsMeshZValueDatasetGroup *zValueDatasetGroup = new QgsMeshZValueDatasetGroup( "BOTTOM", mesh );
  ouputMesh->addDatasets( zValueDatasetGroup );

  ouputMesh->saveDataset( path, 0, QStringLiteral( "SELAFIN" ) );

  //! Roughness
  std::unique_ptr<ReosPolygonStructureValues> roughness(
    hydraulicStructure->roughnessStructure()->structure()->values( meshLayer->crs().toWkt( QgsCoordinateReferenceSystem::WKT2_2019_SIMPLIFIED ) ) );


  std::shared_ptr<QgsMeshMemoryDataset> roughnessDataset( new QgsMeshMemoryDataset );
  roughnessDataset->values.resize( mesh.vertexCount() );

  int size = mesh.vertexCount();
  double defaultVal = hydraulicStructure->roughnessStructure()->defaultRoughness()->value();
  for ( int i = 0; i < size; ++i )
  {
    const QgsMeshVertex &vert = mesh.vertices.at( i );
    double val = roughness->value( vert.x(), vert.y(), false );
    if ( std::isnan( val ) )
      val = defaultVal;
    roughnessDataset->values[i] = val;
  }

  roughnessDataset->valid = true;
  roughnessDataset->time = 0;

  std::unique_ptr<QgsMeshMemoryDatasetGroup> roughnessGroup( new QgsMeshMemoryDatasetGroup( "ROUGHNESS", QgsMeshDatasetGroupMetadata::DataOnVertices ) );
  roughnessGroup->addDataset( roughnessDataset );
  roughnessGroup->initialize();

  ouputMesh->addDatasets( roughnessGroup.release() );
  ouputMesh->saveDataset( path, 1, QStringLiteral( "SELAFIN" ) );
}
