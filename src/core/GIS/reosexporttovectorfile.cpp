/***************************************************************************
  reosexporttovectorfile.cpp - ReosExportToVectorFile

 ---------------------
 begin                : 6.3.2021
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
#include "reosexporttovectorfile.h"
#include "qgsvectorfilewriter.h"
#include "qgsproject.h"
#include "qgspolygon.h"
#include "qgsabstractgeometry.h"

ReosExportToVectorFile::~ReosExportToVectorFile() = default;

static QgsFields constructQgisFields( const QList<ReosExportToVectorFile::Field> fields )
{
  QgsFields qgsFields;
  for ( const ReosExportToVectorFile::Field &field : fields )
  {
    QgsField qgsField( field.name, field.type, field.typeName, field.length );
    qgsFields.append( qgsField );
  }

  return qgsFields;
}

ReosExportToVectorFile::ReosExportToVectorFile( const QString &fileName,
    const QList<ReosExportToVectorFile::Field> fields,
    ReosExportToVectorFile::GeometryType geometryType,
    const QString &crs ):
  mType( geometryType ),
  mFields( fields )
{
  QgsFields qgsFields = constructQgisFields( fields );

  Qgis::WkbType qgsGeometryType;
  switch ( geometryType )
  {
    case ReosExportToVectorFile::Polyline:
      qgsGeometryType = Qgis::WkbType::LineString;
      break;
    case ReosExportToVectorFile::Polygon:
      qgsGeometryType = Qgis::WkbType::Polygon;
      break;
  }

  QgsCoordinateReferenceSystem qgsCrs = QgsCoordinateReferenceSystem::fromWkt( crs );
  QgsCoordinateTransformContext transformContext;
  if ( QgsProject::instance() )
  {
    transformContext = QgsProject::instance()->transformContext();
  }

  QgsVectorFileWriter::SaveVectorOptions options;
  options.fileEncoding = QStringLiteral( "utf-8" );
  options.driverName = QStringLiteral( "ESRI Shapefile" );

  mFileWriter.reset( QgsVectorFileWriter::create(
                       fileName,
                       qgsFields,
                       qgsGeometryType,
                       qgsCrs,
                       transformContext,
                       options ) );
}

void ReosExportToVectorFile::addPolygon( const QPolygonF &polygon, const QVariantMap &attributes )
{
  if ( mType != Polygon )
    return;
  std::unique_ptr<QgsLineString> linestring( QgsLineString::fromQPolygonF( polygon ) );
  std::unique_ptr< QgsAbstractGeometry > qgsPolygon = std::make_unique<QgsPolygon>( linestring.release() );

  addFeature( qgsPolygon.release(), attributes );
}

void ReosExportToVectorFile::addPolyline( const QPolygonF &polygon, const QVariantMap &attributes )
{
  if ( mType != Polyline )
    return;

  std::unique_ptr<QgsLineString> linestring( QgsLineString::fromQPolygonF( polygon ) );
  addFeature( linestring.release(), attributes );
}

void ReosExportToVectorFile::addFeature( QgsAbstractGeometry *ageom, const QVariantMap &attributes )
{
  QgsGeometry geom( ageom );

  QgsFields qgsFields = constructQgisFields( mFields );
  QgsFeature feat( qgsFields );
  feat.setGeometry( geom );
  for ( const QString &key : attributes.keys() )
  {
    feat.setAttribute( key, attributes[key] );
  }

  mFileWriter->addFeature( feat );
}
