/***************************************************************************
                      reosdigitalelevationmodel.cpp
                     --------------------------------------
Date                 : 01-10-2020
Copyright            : (C) 2020 by Vincent Cloarec
email                : vcloarec at gmail dot com
 ***************************************************************************
 *                                                                         *
 *   This program is free software; you can redistribute it and/or modify  *
 *   it under the terms of the GNU General Public License as published by  *
 *   the Free Software Foundation; either version 2 of the License, or     *
 *   (at your option) any later version.                                   *
 *                                                                         *
 ***************************************************************************/

#include <QMutexLocker>

#include "reosprocess.h"
#include "reosdigitalelevationmodel_p.h"
#include "reosrasterline.h"

#include "qgsrasteridentifyresult.h"
#include "qgslinestring.h"

ReosDigitalElevationModelRaster::ReosDigitalElevationModelRaster(
  QgsRasterLayer *rasterLayer,
  const QgsCoordinateTransformContext &transformContext ):
  mTransformContext( transformContext )
{
  if ( rasterLayer )
  {
    mDataProvider.reset( rasterLayer->dataProvider()->clone() );
    mCrs = rasterLayer->crs();
    mSourceId = rasterLayer->id();
  }
}

double ReosDigitalElevationModelRaster::elevationAt( const QPointF &point, const QString &pointCrs ) const
{
  assert( mDataProvider );

  QgsCoordinateReferenceSystem ptCrs = QgsCoordinateReferenceSystem::fromWkt( pointCrs );
  QgsCoordinateTransform transform( ptCrs, mCrs, mTransformContext );
  QgsPointXY pointInDem;
  if ( transform.isValid() )
  {
    try
    {
      pointInDem = transform.transform( QgsPointXY( point.x(), point.y() ) );
    }
    catch ( QgsCsException &e )
    {
      pointInDem = QgsPointXY( point.x(), point.y() );
    }
  }
  else
  {
    pointInDem = QgsPointXY( point.x(), point.y() );
  }

  QgsRasterIdentifyResult result = mDataProvider->identify( pointInDem, QgsRaster::IdentifyFormatValue );
  if ( result.isValid() )
    return result.results().value( 1 ).toDouble();
  else
    return mDataProvider->sourceNoDataValue( 1 );
}

QPolygonF ReosDigitalElevationModelRaster::elevationOnPolyline( const QPolygonF &polyline, const QString &polylineCrs, ReosProcess *process ) const
{
  assert( mDataProvider );

  QPolygonF ret;

  if ( polyline.isEmpty() )
    return ret;

  ret.append( QPointF( 0, elevationAt( polyline.first(), polylineCrs ) ) );

  QgsCoordinateReferenceSystem qgsCrs = QgsCoordinateReferenceSystem::fromWkt( polylineCrs );
  QgsDistanceArea distanceCalculation;
  distanceCalculation.setSourceCrs( qgsCrs, mTransformContext );
  QgsUnitTypes::DistanceUnit unit = distanceCalculation.lengthUnits();
  double unitFactor = QgsUnitTypes::fromUnitToUnitFactor( unit, QgsUnitTypes::DistanceMeters );

  double s = 0;
  for ( int i = 0; i < polyline.count() - 1; ++i )
  {
    const QPointF &point1 = polyline.at( i );
    const QPointF &point2 = polyline.at( i + 1 );

    ReosMapExtent segmentExtent;
    segmentExtent.addPointToExtent( point1 );
    segmentExtent.addPointToExtent( point2 );

    ReosRasterExtent rasterExtent;
    if ( process )
      process->setInformation( QObject::tr( "Read DEM for segment %1/%2" ).arg( i + 1 ).arg( polyline.count() - 1 ) );
    ReosRasterMemory<float> segmentDEM = extractMemoryRasterSimplePrecision( segmentExtent, rasterExtent, polylineCrs, process );

    if ( process && !process->isSuccessful() )
      return ret;

    ReosRasterCellPos pos1 = rasterExtent.mapToCellPos( point1 );
    ReosRasterCellPos pos2 = rasterExtent.mapToCellPos( point2 );

    ReosRasterLine rasterLine( false );
    rasterLine.addPoint( pos1 );
    rasterLine.addPoint( pos2 );
    if ( process )
    {
      process->setMaxProgression( rasterLine.cellCount() );
      process->setInformation( QObject::tr( "Project segment %1/%2" ).arg( i ).arg( polyline.count() ) );
    }


    QVector<QgsPointXY> line;
    line << point1;
    line << point2;

    double len1 = unitFactor * distanceCalculation.measureLine( line );

    for ( unsigned cell = 1; cell < rasterLine.cellCount() - 1; ++cell )
    {
      if ( process )
        process->setCurrentProgression( cell );

      const ReosRasterCellPos &cellPos = rasterLine.cellPosition( cell );
      QPointF pointOnRaster = rasterExtent.cellCenterToMap( cellPos );
      float value = segmentDEM.value( cellPos.row(), cellPos.column() );

      QgsPointXY ptOnRaster( pointOnRaster );
      QgsPointXY projectedPt;
      // project point on pt1-pt2
      ptOnRaster.sqrDistToSegment( point1.x(), point1.y(), point2.x(), point2.y(), projectedPt );
      QVector<QgsPointXY> linePart;
      linePart << point1;
      linePart << projectedPt;
      double lenProj = unitFactor * distanceCalculation.measureLine( linePart );


      ret.append( QPointF( s + lenProj, value ) );

      if ( process && process->isStop() )
        break;
    }

    s = s + len1;
    ret.append( QPointF( s, elevationAt( point2, polylineCrs ) ) );

    if ( process && process->isStop() )
      break;
  }

  if ( process )
    process->setSuccesful( process->isSuccessful() && !process->isStop() );

  return ret;

}

ReosRasterMemory<float> ReosDigitalElevationModelRaster::extractMemoryRasterSimplePrecision(
  const ReosMapExtent &destinationExtent,
  ReosRasterExtent &outputRasterExtent,
  const QString &destinationCrs,
  ReosProcess *process ) const
{
  QgsCoordinateReferenceSystem destCrs = QgsCoordinateReferenceSystem::fromWkt( destinationCrs.isEmpty() ? destinationExtent.crs() : destinationCrs );
  QgsCoordinateTransform transform( mCrs, destCrs, mTransformContext );

  QgsRectangle destExtent( destinationExtent.xMapMin(),
                           destinationExtent.yMapMin(),
                           destinationExtent.xMapMax(),
                           destinationExtent.yMapMax() );

  QgsRectangle extentInDEMCoordinates;
  if ( transform.isValid() )
  {
    try
    {
      extentInDEMCoordinates = transform.transform( destExtent, QgsCoordinateTransform::ReverseTransform );
    }
    catch ( QgsCsException &e )
    {
      extentInDEMCoordinates = destExtent;
    }
  }
  else
    extentInDEMCoordinates = destExtent;

  ReosRasterExtent outputRasterExtentInDemCoorindates = rasterExtent( extentInDEMCoordinates );

  int xPixCount = outputRasterExtentInDemCoorindates.xCellCount();
  int yPixCount = outputRasterExtentInDemCoorindates.yCellCount();

  QgsRectangle adjustedExtent( outputRasterExtentInDemCoorindates.xMapMin(),
                               outputRasterExtentInDemCoorindates.yMapMin(),
                               outputRasterExtentInDemCoorindates.xMapMax(),
                               outputRasterExtentInDemCoorindates.yMapMax() );

  if ( transform.isValid() )
  {
    QgsRectangle adjustedExtentInDestinationCoordinates;
    try
    {
      adjustedExtentInDestinationCoordinates = transform.transform( adjustedExtent );
    }
    catch ( QgsCsException &e )
    {
      adjustedExtentInDestinationCoordinates = adjustedExtent;
    }

    outputRasterExtent =      ReosRasterExtent(
                                ReosMapExtent( adjustedExtentInDestinationCoordinates.xMinimum(),
                                    adjustedExtentInDestinationCoordinates.yMinimum(),
                                    adjustedExtentInDestinationCoordinates.xMaximum(),
                                    adjustedExtentInDestinationCoordinates.yMaximum() ),
                                xPixCount, yPixCount );
  }
  else
    outputRasterExtent = outputRasterExtentInDemCoorindates;


  ReosRasterMemory<float> ret = ReosRasterMemory<float>( yPixCount, xPixCount ); //(row, col)

  std::unique_ptr<QgsRasterBlock> block;
  block.reset( mDataProvider->block( 1, adjustedExtent, xPixCount, yPixCount ) );

  if ( !block->isValid() )
  {
    if ( process )
      process->setSuccesful( false );

    return ret;
  }

  ret.reserveMemory();

  if ( process )
  {
    process->setCurrentProgression( 0 );
    process->setMaxProgression( yPixCount );
  }

  for ( int i = 0; i < yPixCount; ++i )
  {
    for ( int j = 0; j < xPixCount; ++j )
    {
      ret.setValue( i, j, float( block->value( i, j ) ) );
      if ( process && process->isStop() )
      {
        ret.freeMemory();
        process->setSuccesful( false );
        return ret;
      }
    }

    if ( process )
      process->setCurrentProgression( i );
  }

  if ( process )
    process->setSuccesful( true );

  return ret;

}

QString ReosDigitalElevationModelRaster::source() const
{
  return mSourceId;
}

ReosRasterExtent ReosDigitalElevationModelRaster::rasterExtent( const QgsRectangle &originalExtent ) const
{
  assert( mDataProvider );

  QgsRectangle sourceRasterExtent = mDataProvider->extent();

  double xmin;
  double ymin;
  double xmax;
  double ymax;

  int yCount;
  int xCount;

  if ( mDataProvider->capabilities() & QgsRasterInterface::Size )
  {
    xCount = mDataProvider->xSize();
    yCount = mDataProvider->ySize();
  }
  else
  {
    xCount = sourceRasterExtent.width();
    yCount = sourceRasterExtent.height();
  }

  double xPixelSize = sourceRasterExtent.width() / xCount;;
  double yPixelSize = sourceRasterExtent.height() / yCount;

  if ( originalExtent.xMinimum() < sourceRasterExtent.xMinimum() )
    xmin = sourceRasterExtent.xMinimum();
  else
    xmin =  int( ( originalExtent.xMinimum() - sourceRasterExtent.xMinimum() ) / xPixelSize ) * xPixelSize + sourceRasterExtent.xMinimum();

  if ( originalExtent.yMinimum() < sourceRasterExtent.yMinimum() )
    ymin = sourceRasterExtent.yMinimum();
  else
    ymin = int( ( originalExtent.yMinimum() - sourceRasterExtent.yMinimum() ) / yPixelSize ) * yPixelSize + sourceRasterExtent.yMinimum();

  if ( originalExtent.xMaximum() > sourceRasterExtent.xMaximum() )
    xmax = sourceRasterExtent.xMaximum();
  else
    xmax = sourceRasterExtent.xMaximum() - int( ( sourceRasterExtent.xMaximum() - originalExtent.xMaximum() ) / xPixelSize ) * xPixelSize;

  if ( originalExtent.yMaximum() > sourceRasterExtent.yMaximum() )
    ymax = sourceRasterExtent.yMaximum();
  else
    ymax = sourceRasterExtent.yMaximum() - int( ( sourceRasterExtent.yMaximum() - originalExtent.yMaximum() ) / yPixelSize ) * yPixelSize;

  xCount = int( ( xmax - xmin ) / xPixelSize + 0.5 );
  yCount = int( ( ymax - ymin ) / yPixelSize + 0.5 );

  return ReosRasterExtent( ReosMapExtent( xmin, ymin, xmax, ymax ), xCount, yCount );
}
