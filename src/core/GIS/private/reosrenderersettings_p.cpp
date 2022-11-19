/*******************************getShader**********************************
  reosrenderersettings_p.cpp - ReosRendererSettings_p

 ---------------------
 begin                : 16.11.2022
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
#include "reosrenderersettings_p.h"

#include <qgsmaplayerrenderer.h>

ReosRendererSettings_p::ReosRendererSettings_p( const void *settings )
{
  const QgsMapSettings *mapSettings = static_cast<const QgsMapSettings *>( settings );
  mSettings = *mapSettings;
}

QDateTime ReosRendererSettings_p::mapTime() const
{
  return mSettings.temporalRange().begin();
}

const QgsMapSettings &ReosRendererSettings_p::settings() const
{
  return mSettings;
}

static bool reprojectToLayerExtent( const QgsMapLayer *ml, const QgsCoordinateTransform &ct, QgsRectangle &extent, QgsRectangle &r2 )
{
  // Completly copied code from from QGIS QgsMapRenderer.cpp

  bool res = true;
  // we can safely use ballpark transforms without bothering the user here -- at the likely scale of layer extents there
  // won't be an appreciable difference, and we aren't actually transforming any rendered points here anyway (just the layer extent)
  QgsCoordinateTransform approxTransform = ct;
  approxTransform.setBallparkTransformsAreAppropriate( true );

  try
  {
#ifdef QGISDEBUG
    // QgsLogger::debug<QgsRectangle>("Getting extent of canvas in layers CS. Canvas is ", extent, __FILE__, __FUNCTION__, __LINE__);
#endif
    // Split the extent into two if the source CRS is
    // geographic and the extent crosses the split in
    // geographic coordinates (usually +/- 180 degrees,
    // and is assumed to be so here), and draw each
    // extent separately.
    static const double SPLIT_COORD = 180.0;

    if ( ml->crs().isGeographic() )
    {
      if ( ml->type() == QgsMapLayerType::VectorLayer && !approxTransform.destinationCrs().isGeographic() )
      {
        // if we transform from a projected coordinate system check
        // check if transforming back roughly returns the input
        // extend - otherwise render the world.
        QgsRectangle extent1 = approxTransform.transformBoundingBox( extent, Qgis::TransformDirection::Reverse );
        QgsRectangle extent2 = approxTransform.transformBoundingBox( extent1, Qgis::TransformDirection::Forward );

        QgsDebugMsgLevel( QStringLiteral( "\n0:%1 %2x%3\n1:%4\n2:%5 %6x%7 (w:%8 h:%9)" )
                          .arg( extent.toString() ).arg( extent.width() ).arg( extent.height() )
                          .arg( extent1.toString(), extent2.toString() ).arg( extent2.width() ).arg( extent2.height() )
                          .arg( std::fabs( 1.0 - extent2.width() / extent.width() ) )
                          .arg( std::fabs( 1.0 - extent2.height() / extent.height() ) )
                          , 3 );

        // can differ by a maximum of up to 20% of height/width
        if ( qgsDoubleNear( extent2.xMinimum(), extent.xMinimum(), extent.width() * 0.2 )
             && qgsDoubleNear( extent2.xMaximum(), extent.xMaximum(), extent.width() * 0.2 )
             && qgsDoubleNear( extent2.yMinimum(), extent.yMinimum(), extent.height() * 0.2 )
             && qgsDoubleNear( extent2.yMaximum(), extent.yMaximum(), extent.height() * 0.2 )
           )
        {
          extent = extent1;
        }
        else
        {
          extent = QgsRectangle( -180.0, -90.0, 180.0, 90.0 );
          res = false;
        }
      }
      else
      {
        // Note: ll = lower left point
        QgsPointXY ll = approxTransform.transform( extent.xMinimum(), extent.yMinimum(),
                        Qgis::TransformDirection::Reverse );

        //   and ur = upper right point
        QgsPointXY ur = approxTransform.transform( extent.xMaximum(), extent.yMaximum(),
                        Qgis::TransformDirection::Reverse );

        QgsDebugMsgLevel( QStringLiteral( "in:%1 (ll:%2 ur:%3)" ).arg( extent.toString(), ll.toString(), ur.toString() ), 4 );

        extent = approxTransform.transformBoundingBox( extent, Qgis::TransformDirection::Reverse );

        QgsDebugMsgLevel( QStringLiteral( "out:%1 (w:%2 h:%3)" ).arg( extent.toString() ).arg( extent.width() ).arg( extent.height() ), 4 );

        if ( ll.x() > ur.x() )
        {
          // the coordinates projected in reverse order than what one would expect.
          // we are probably looking at an area that includes longitude of 180 degrees.
          // we need to take into account coordinates from two intervals: (-180,x1) and (x2,180)
          // so let's use (-180,180). This hopefully does not add too much overhead. It is
          // more straightforward than rendering with two separate extents and more consistent
          // for rendering, labeling and caching as everything is rendered just in one go
          extent.setXMinimum( -SPLIT_COORD );
          extent.setXMaximum( SPLIT_COORD );
          res = false;
        }
      }

      // TODO: the above rule still does not help if using a projection that covers the whole
      // world. E.g. with EPSG:3857 the longitude spectrum -180 to +180 is mapped to approx.
      // -2e7 to +2e7. Converting extent from -5e7 to +5e7 is transformed as -90 to +90,
      // but in fact the extent should cover the whole world.
    }
    else // can't cross 180
    {
      if ( approxTransform.destinationCrs().isGeographic() &&
           ( extent.xMinimum() <= -180 || extent.xMaximum() >= 180 ||
             extent.yMinimum() <= -90 || extent.yMaximum() >= 90 ) )
        // Use unlimited rectangle because otherwise we may end up transforming wrong coordinates.
        // E.g. longitude -200 to +160 would be understood as +40 to +160 due to periodicity.
        // We could try to clamp coords to (-180,180) for lon resp. (-90,90) for lat,
        // but this seems like a safer choice.
      {
        extent = QgsRectangle( std::numeric_limits<double>::lowest(), std::numeric_limits<double>::lowest(), std::numeric_limits<double>::max(), std::numeric_limits<double>::max() );
        res = false;
      }
      else
        extent = approxTransform.transformBoundingBox( extent, Qgis::TransformDirection::Reverse );
    }
  }
  catch ( QgsCsException & )
  {
    QgsDebugMsg( QStringLiteral( "Transform error caught" ) );
    extent = QgsRectangle( std::numeric_limits<double>::lowest(), std::numeric_limits<double>::lowest(), std::numeric_limits<double>::max(), std::numeric_limits<double>::max() );
    r2 = QgsRectangle( std::numeric_limits<double>::lowest(), std::numeric_limits<double>::lowest(), std::numeric_limits<double>::max(), std::numeric_limits<double>::max() );
    res = false;
  }

  return res;
}

ReosQgisLayerRenderer_p::ReosQgisLayerRenderer_p( ReosRendererSettings *settings, QgsMapLayer *layer, ReosRenderedObject *renderedObject ):
  ReosObjectRenderer( renderedObject )
{
  ReosRendererSettings_p *rendererSettings = dynamic_cast<ReosRendererSettings_p *>( settings );

  if ( rendererSettings )
  {
    const QgsMapSettings &settings = rendererSettings->settings();
    mImage = QImage( settings.deviceOutputSize(), settings.outputImageFormat() );
    mImage.setDevicePixelRatio( settings.devicePixelRatio() );
    mImage.setDotsPerMeterX( static_cast<int>( settings.outputDpi() * 39.37 ) );
    mImage.setDotsPerMeterY( static_cast<int>( settings.outputDpi() * 39.37 ) );
    mImage.fill( Qt::transparent );

    QgsRectangle renderExtent = settings.visibleExtent(), r2;
    renderExtent.buffered( settings.extentBuffer() );
    QgsCoordinateTransform ct = settings.layerTransform( layer );

    bool haveExtentInLayerCrs = true;
    if ( ct.isValid() )
    {
      haveExtentInLayerCrs = reprojectToLayerExtent( layer, ct, renderExtent, r2 );
    }

    mPainter.reset( new QPainter( &mImage ) );
    mRenderContext = QgsRenderContext::fromMapSettings( settings );
    mRenderContext.setCoordinateTransform( ct );
    mRenderContext.setPainter( mPainter.get() );
    mRenderContext.setExtent( renderExtent );
    if ( !haveExtentInLayerCrs )
      mRenderContext.setFlag( Qgis::RenderContextFlag::ApplyClipAfterReprojection, true );
    mLayerRenderer.reset( layer->createMapRenderer( mRenderContext ) );

    setExtent( settings.visibleExtent().toRectF() );
  }
}

ReosQgisLayerRenderer_p::~ReosQgisLayerRenderer_p() = default;

void ReosQgisLayerRenderer_p::render() const
{
  qDebug() << "Rendered object start rendering";
  mLayerRenderer->render();
}

bool ReosQgisLayerRenderer_p::isRenderingStopped() const
{
  return mLayerRenderer->renderContext()->renderingStopped() || isStop();
}

void ReosQgisLayerRenderer_p::stopRendering()
{
  mRenderContext.setRenderingStopped( true );
}


ReosColorShaderSettings_p::ReosColorShaderSettings_p()
{

}
