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

#include "reosprocess.h"
#include "reosdigitalelevationmodel_p.h"

#include "qgsrasteridentifyresult.h"

ReosDigitalElevationModelRaster::ReosDigitalElevationModelRaster(
  QgsRasterLayer *rasterLayer,
  const QgsCoordinateTransformContext &transformContext ):
  mTransformContext( transformContext )
{
  if ( rasterLayer )
  {
    mDataProvider.reset( rasterLayer->dataProvider()->clone() );
    mCrs = rasterLayer->crs();
  }
}

double ReosDigitalElevationModelRaster::elevationAt( const QPointF &point, const QString &destinationCrs )
{
  assert( mDataProvider );

  QgsCoordinateReferenceSystem destCrs = QgsCoordinateReferenceSystem::fromWkt( destinationCrs );
  QgsCoordinateTransform transform( mCrs, destCrs, mTransformContext );
  QgsPointXY pointInDestination;
  if ( transform.isValid() )
  {
    try
    {
      pointInDestination = transform.transform( QgsPointXY( point.x(), point.y() ) );
    }
    catch ( QgsCsException &e )
    {
      pointInDestination = QgsPointXY( point.x(), point.y() );
    }
  }
  else
  {
    pointInDestination = QgsPointXY( point.x(), point.y() );
  }

  QgsRasterIdentifyResult result = mDataProvider->identify( pointInDestination, QgsRaster::IdentifyFormatValue );
  if ( result.isValid() )
    return result.results().value( 1 ).toDouble();
  else
    return mDataProvider->sourceNoDataValue( 1 );
}

ReosRasterMemory<float> ReosDigitalElevationModelRaster::extractMemoryRasterSimplePrecision( const ReosMapExtent &destinationExtent,
    ReosRasterExtent &outputRasterExtent,
    const QString &destinationCrs,
    ReosProcess *process )
{
  QgsCoordinateReferenceSystem destCrs = QgsCoordinateReferenceSystem::fromWkt( destinationCrs.isEmpty() ? destinationCrs : destinationExtent.crs() );
  QgsCoordinateTransform transform( mCrs, destCrs, mTransformContext );

  QgsRectangle destExtent( destinationExtent.xMapMin(),
                           destinationExtent.yMapMin(),
                           destinationExtent.xMapMax(),
                           destinationExtent.yMapMax() );

  QgsRectangle extentInDEMCoordinate;
  if ( transform.isValid() )
  {
    try
    {
      extentInDEMCoordinate = transform.transform( destExtent, QgsCoordinateTransform::ReverseTransform );
    }
    catch ( QgsCsException &e )
    {
      extentInDEMCoordinate = destExtent;
    }
  }
  else
    extentInDEMCoordinate = destExtent;

  outputRasterExtent = rasterExtent( extentInDEMCoordinate );

  int xPixCount = outputRasterExtent.xCellCount();
  int yPixCount = outputRasterExtent.yCellCount();

  QgsRectangle adjustedExtent( outputRasterExtent.xMapMin(),
                               outputRasterExtent.yMapMin(),
                               outputRasterExtent.xMapMax(),
                               outputRasterExtent.yMapMax() );


  std::unique_ptr<QgsRasterBlock> block;
  block.reset( mDataProvider->block( 1, adjustedExtent, xPixCount, yPixCount ) );

  ReosRasterMemory<float> ret = ReosRasterMemory<float>( yPixCount, xPixCount ); //(row, col)
  ret.reserveMemory();

  if ( process )
    process->setMaxProgression( yPixCount );

  for ( int i = 0; i < yPixCount; ++i )
  {
    for ( int j = 0; j < xPixCount; ++j )
    {
      ret.setValue( i, j, float( block->value( i, j ) ) );
      if ( process && process->isStopAsked() )
      {
        ret.freeMemory();
        return ret;
      }
    }

    if ( process )
      process->setCurrentProgression( i );
  }

  return ret;

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
