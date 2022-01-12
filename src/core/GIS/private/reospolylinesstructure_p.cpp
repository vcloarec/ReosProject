/***************************************************************************
  reospolylinesstructure_p.cpp - ReosPolylinesStructure_p

 ---------------------
 begin                : 10.1.2022
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
#include "reospolylinesstructure_p.h"

#include <QUuid>

#include <qgsproject.h>
#include <qgscoordinatetransform.h>
#include <qgslinestring.h>

static std::unique_ptr<QgsLineString> createQgsPolyline( const QPolygonF &polygon )
{
  return std::unique_ptr<QgsLineString>( QgsLineString::fromQPolygonF( polygon ) );
}



ReosPolylineStructureVectorLayer::ReosPolylineStructureVectorLayer( const QString &wktCrs )
  : mVectorLayer( new QgsVectorLayer( QStringLiteral( "Linestring?crs=" )
                                      + wktCrs
                                      + QStringLiteral( "&index=yes" )
                                      , QStringLiteral( "internalLayer" ),
                                      QStringLiteral( "memory" ) ) )
{
  mVectorLayer->startEditing();
}

ReosPolylineStructureVectorLayer *ReosPolylineStructureVectorLayer::clone()
{
  std::unique_ptr<ReosPolylineStructureVectorLayer> other( new ReosPolylineStructureVectorLayer );
  other->mReosToQgisId = mReosToQgisId;
  other->mVectorLayer.reset( mVectorLayer->clone() );

  return other.release();
}

void ReosPolylineStructureVectorLayer::addPolylines( const QPolygonF &polyline, const QString &sourceCrs, const QString &id )
{
  QgsCoordinateReferenceSystem sourceQgsCrs;
  sourceQgsCrs.createFromString( sourceCrs );
  QgsGeometry geom( createQgsPolyline( polyline ) );

  QgsCoordinateTransform transform( sourceQgsCrs, mVectorLayer->crs(), QgsProject::instance() );
  if ( transform.isValid() )
  {
    try
    {
      geom.transform( transform );
    }
    catch ( QgsCsException &e )
    {
    }
  }

  QgsFeature feat;
  feat.setGeometry( geom );
  mVectorLayer->addFeature( feat );

  if ( id.isEmpty() )
    mReosToQgisId.insert( QUuid::createUuid().toString(), feat.id() );
  else
    mReosToQgisId.insert( id, feat.id() );
}

QPolygonF ReosPolylineStructureVectorLayer::polyline( const QString &destinationCrs, const QString &id ) const
{
  QgsFeatureId fid = 0;;
  if ( !id.isEmpty() )
  {
    auto it = mReosToQgisId.find( id );
    if ( it == mReosToQgisId.end() )
      return QPolygonF();

    fid = it.value();
  }
  else
  {
    QgsFeatureIterator it = mVectorLayer->getFeatures();
    QgsFeature feat;
    if ( !it.nextFeature( feat ) )
      return QPolygonF();

    fid = feat.id();
  }

  QgsCoordinateReferenceSystem destQgsCrs;
  destQgsCrs.createFromString( destinationCrs );

  QgsGeometry geom = mVectorLayer->getGeometry( fid );

  QgsCoordinateTransform transform( mVectorLayer->crs(), destQgsCrs, QgsProject::instance() );
  if ( transform.isValid() )
  {
    try
    {
      geom.transform( transform );
    }
    catch ( QgsCsException &e )
    {
    }
  }

  return geom.asQPolygonF();

}

void ReosPolylineStructureVectorLayer::removeAll()
{
  if ( !mVectorLayer )
    return;

  QgsFeatureIterator it = mVectorLayer->getFeatures();
  QgsFeature feat;
  while ( it.nextFeature( feat ) )
    mVectorLayer->deleteFeature( feat.id() );
}
