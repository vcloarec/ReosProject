/***************************************************************************
  reostestrenderedobject.cpp - ReosTestRenderedObject

 ---------------------
 begin                : 23.1.2023
 copyright            : (C) 2023 by Vincent Cloarec
 email                : vcloarec at gmail dot com
 ***************************************************************************
 *                                                                         *
 *   This program is free software; you can redistribute it and/or modify  *
 *   it under the terms of the GNU General Public License as published by  *
 *   the Free Software Foundation; either version 2 of the License, or     *
 *   (at your option) any later version.                                   *
 *                                                                         *
 ***************************************************************************/
#include "reostestrenderedobject.h"

#include <qgsmapsettings.h>

#include "reosmapextent.h"
#include "reosrenderedobject.h"
#include "reosrenderersettings.h"

ReosTestRenderedObject::ReosTestRenderedObject()
{

}

QImage ReosTestRenderedObject::render( ReosRenderedObject *object, const QDateTime &time )
{
  QgsMapSettings mapSettings;
  mapSettings.setOutputSize( mOutputSize );
  mapSettings.setOutputImageFormat( mImageFormat );
  mapSettings.setOutputDpi( 95 );
  mapSettings.setIsTemporal( true );
  mapSettings.setTemporalRange( QgsDateTimeRange( time, time.addSecs( 10 ) ) );

  ReosMapExtent objectExtent = object->extent();

  mapSettings.setDestinationCrs( QgsCoordinateReferenceSystem::fromWkt( objectExtent.crs() ) );
  mapSettings.setExtent( QgsRectangle( objectExtent.toRectF() ) );

  std::unique_ptr<ReosRendererSettings> settings( object->createRenderSettings( &mapSettings ) );

  std::unique_ptr<ReosObjectRenderer> renderer( object->createRenderer( settings.get() ) );

  renderer->render();

  return renderer->image();
}

bool ReosTestRenderedObject::compareRendering(
  ReosRenderedObject *object,
  const QDateTime &time,
  const QString &imageFile,
  int tolerance )
{
  QImage file( imageFile );
  QImage rendered = render( object, time );

  QImage diffImage( file.width(), file.height(), QImage::Format_RGB32 );

  bool success = false;
  if ( file.width() == rendered.width() && file.height() == rendered.height() )
  {
    size_t nbPixelDiff = 0;
    diffImage.fill( Qt::white );
    int width = file.width();
    int height = file.height();
    const QRgb *dataFile = reinterpret_cast <const QRgb *>( file.constBits() );
    const QRgb *actualImage = reinterpret_cast <const QRgb *>( rendered.constBits() );
    QRgb *diff = reinterpret_cast <QRgb *>( diffImage.bits() );
    for ( int i = 0; i < width; ++i )
    {
      for ( int j = 0; j < height; ++j )
      {

        size_t index = j * width + i;
        if ( std::abs( qRed( dataFile[index] ) - qRed( actualImage[index] ) ) > tolerance ||
             std::abs( qGreen( dataFile[index] ) - qGreen( actualImage[index] ) ) > tolerance ||
             std::abs( qBlue( dataFile[index] ) - qBlue( actualImage[index] ) ) > tolerance ||
             std::abs( qAlpha( dataFile[index] ) - qAlpha( actualImage[index] ) ) > tolerance )
        {
          diff[index] = qRgb( 255, 0, 0 );
          nbPixelDiff++; ;
        }
      }
    }
    success = nbPixelDiff < 40;
  }

  if ( !success )
  {
    QFileInfo fileInfo( imageFile );
    const QString diffRenderingFile = fileInfo.dir().filePath( "diff_" + fileInfo.fileName() );
    const QString actualRenderingFile = fileInfo.dir().filePath( "actual_" + fileInfo.fileName() );
    diffImage.save( diffRenderingFile, "PNG", 100 );
    rendered.save( actualRenderingFile, "PNG", 100 );
  }

  return success;
}
