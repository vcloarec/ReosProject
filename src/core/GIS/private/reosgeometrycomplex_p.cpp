#include "reosgeometrycomplex_p.h"
#include <qgscoordinatetransform.h>
#include <qgsmapsettings.h>
#include <qgsproject.h>
#include <qgsmaplayerrenderer.h>
#include <qgis.h>

#include "reosmapextent.h"

ReosGeometryComplex_p::ReosGeometryComplex_p( const QString &type, const QString &wktCrs )
  : mVectorLayer( new QgsVectorLayer( type + QStringLiteral( "?crs=" ) + wktCrs + QStringLiteral( "&index=yes" ), QStringLiteral( "internalLayer" ), QStringLiteral( "memory" ) ) )
{}

QgsPointXY ReosGeometryComplex_p::toLayerCoordinates( const ReosSpatialPosition &position ) const
{
  QgsCoordinateReferenceSystem crs;
  crs.createFromWkt( position.crs() );

  QgsCoordinateTransform transform( crs, mVectorLayer->crs(), QgsProject::instance() );

  return transformCoordinates( position.position(), toLayerTransform( position.crs() ) );
}

QgsPointXY ReosGeometryComplex_p::transformCoordinates( const QPointF &position, const QgsCoordinateTransform &transform )
{
  return transformCoordinates( QgsPointXY( position ), transform );
}


const QgsCoordinateTransform ReosGeometryComplex_p::toLayerTransform( const QString &crs ) const
{
  auto transformIt = mCacheToLayerTransform.find( crs );
  if ( transformIt != mCacheToLayerTransform.constEnd() )
    return transformIt.value();

  QgsCoordinateReferenceSystem qgsCrs;
  qgsCrs.createFromWkt( crs );

  QgsCoordinateTransform ret( qgsCrs, mVectorLayer->crs(), QgsProject::instance() );

  mCacheToLayerTransform[crs] = ret;

  return ret;
}

const QgsCoordinateTransform ReosGeometryComplex_p::toDestinationTransform( const QString &destinationCrs ) const
{
  auto transformIt = mCacheToDestinationTransform.find( destinationCrs );
  if ( transformIt != mCacheToDestinationTransform.constEnd() )
    return transformIt.value();

  QgsCoordinateReferenceSystem qgsCrs;
  qgsCrs.createFromString( destinationCrs );

  QgsCoordinateTransform ret( mVectorLayer->crs(), qgsCrs, QgsProject::instance() );

  mCacheToDestinationTransform[destinationCrs] = ret;

  return ret;
}

ReosMapExtent ReosGeometryComplex_p::extent( const QString &destinationCrs ) const
{
  QgsRectangle internalExtent = mVectorLayer->extent();
  QgsCoordinateReferenceSystem qgsCrs;
  qgsCrs.createFromString( destinationCrs );

  QgsCoordinateTransform transform( mVectorLayer->crs(), qgsCrs, QgsProject::instance() );

  if ( transform.isValid() )
  {
    try
    {
      QgsRectangle destExtent;
      destExtent = transform.transformBoundingBox( internalExtent );
      ReosMapExtent ret( destExtent.toRectF() );
      ret.setCrs( destinationCrs );
      return ret;
    }
    catch ( ... )
    {}
  }

  ReosMapExtent ret( internalExtent.toRectF() );
  return ret;
}

QgsRectangle ReosGeometryComplex_p::layerZone( const ReosMapExtent &zone ) const
{
  QgsCoordinateTransform transform = toLayerTransform( zone.crs() );

  QgsRectangle rect( zone.toRectF() );
  QgsRectangle layerRect = rect;

  if ( transform.isValid() )
  {
    try
    {
      layerRect = transform.transform( rect );
    }
    catch ( QgsCsException & )
    {
      layerRect = rect;
    }
  }
  return layerRect;
}

QString ReosGeometryComplex_p::crs() const
{
  return mVectorLayer->crs().toWkt( Qgis::CrsWktVariant::PreferredSimplified );
}

void ReosGeometryComplex_p::setLayerCrs( const QString &wktCrs ) const
{
  mCacheToDestinationTransform.clear();
  mCacheToLayerTransform.clear();
  QgsCoordinateReferenceSystem crs = QgsCoordinateReferenceSystem::fromWkt( wktCrs );
  mVectorLayer->setCrs( crs );
}


static bool reprojectToLayerExtent( bool isLayerCrsGeographic, const QgsCoordinateTransform &ct, QgsRectangle &extent, QgsRectangle &r2 )
{
  // from QGIS QgsMapLayerRenderer::reprojectToLayerExtent() - reproject extent to layer coordinates

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

    if ( isLayerCrsGeographic )
    {
      if ( !approxTransform.destinationCrs().isGeographic() )
      {
        // if we transform from a projected coordinate system check
        // check if transforming back roughly returns the input
        // extend - otherwise render the world.
        QgsRectangle extent1 = approxTransform.transformBoundingBox( extent, Qgis::TransformDirection::Reverse );
        QgsRectangle extent2 = approxTransform.transformBoundingBox( extent1, Qgis::TransformDirection::Forward );

        // can differ by a maximum of up to 20% of height/width
        if ( qgsDoubleNear( extent2.xMinimum(), extent.xMinimum(), extent.width() * 0.2 )
             && qgsDoubleNear( extent2.xMaximum(), extent.xMaximum(), extent.width() * 0.2 )
             && qgsDoubleNear( extent2.yMinimum(), extent.yMinimum(), extent.height() * 0.2 )
             && qgsDoubleNear( extent2.yMaximum(), extent.yMaximum(), extent.height() * 0.2 ) )
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
        QgsPointXY ll = approxTransform.transform( extent.xMinimum(), extent.yMinimum(), Qgis::TransformDirection::Reverse );

        //   and ur = upper right point
        QgsPointXY ur = approxTransform.transform( extent.xMaximum(), extent.yMaximum(), Qgis::TransformDirection::Reverse );
        extent = approxTransform.transformBoundingBox( extent, Qgis::TransformDirection::Reverse );

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
      if ( approxTransform.destinationCrs().isGeographic() && ( extent.xMinimum() <= -180 || extent.xMaximum() >= 180 || extent.yMinimum() <= -90 || extent.yMaximum() >= 90 ) )
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
  catch ( QgsCsException &e )
  {
    extent = QgsRectangle( std::numeric_limits<double>::lowest(), std::numeric_limits<double>::lowest(), std::numeric_limits<double>::max(), std::numeric_limits<double>::max() );
    r2 = QgsRectangle( std::numeric_limits<double>::lowest(), std::numeric_limits<double>::lowest(), std::numeric_limits<double>::max(), std::numeric_limits<double>::max() );
    res = false;
  }

  return res;
}

static QgsRenderContext prepareRenderContext( bool isLayerCrsGeographic, const QgsMapSettings &mSettings, QPainter *painter, const QgsCoordinateTransform &transform )
{
  // from QGIS QgsMapLayerRenderer::prepareJobs()

  QgsRenderContext renderContext = QgsRenderContext::fromMapSettings( mSettings );
  QgsRectangle r1 = mSettings.visibleExtent(), r2;
  r1.grow( mSettings.extentBuffer() );

  bool haveExtentInLayerCrs = true;
  if ( transform.isValid() )
  {
    haveExtentInLayerCrs = reprojectToLayerExtent( isLayerCrsGeographic, transform, r1, r2 );
  }

  if ( !r1.isFinite() || !r2.isFinite() )
  {
    qDebug() << "There was a problem transforming the layer's extent. Layer skipped.";
    return renderContext;
  }

  renderContext.setPainter( painter );
  //renderContext->setLabelingEngine( labelingEngine2 );
  //renderContext->setLabelSink( labelSink() );
  renderContext.setCoordinateTransform( transform );
  renderContext.setExtent( r1 );

  if ( !haveExtentInLayerCrs || ( transform.isValid() && ( transform.sourceCrs().isGeographic() != transform.destinationCrs().isGeographic() ) ) )
    renderContext.setFlag( Qgis::RenderContextFlag::ApplyClipAfterReprojection, true );

  return renderContext;
}

void ReosGeometryComplex_p::renderGeometry( const QgsMapSettings &mapSettings, QPainter *painter ) const
{
  QgsRenderContext renderContext = prepareRenderContext( mVectorLayer->crs().isGeographic(), mapSettings, painter, toDestinationTransform( mapSettings.destinationCrs().toWkt() ) );
  std::unique_ptr<QgsMapLayerRenderer> renderer;
  renderer.reset( mVectorLayer.get()->createMapRenderer( renderContext ) );
  renderer->render();
}

QgsFeatureIterator ReosGeometryComplex_p::closeFeatures( const ReosMapExtent &zone, QgsRectangle &rect ) const
{
  rect = layerZone( zone );
  return closeFeaturesInLayerCoordinate( rect );
}

QgsFeatureIterator ReosGeometryComplex_p::closeFeaturesInLayerCoordinate( const QgsRectangle &rectLayer ) const
{
  QgsFeatureRequest request;
  request.setFilterRect( rectLayer );

  return mVectorLayer->getFeatures( request );
}

QgsPointXY ReosGeometryComplex_p::transformCoordinates( const QgsPointXY &position, const QgsCoordinateTransform &transform )
{
  if ( transform.isValid() )
  {
    try
    {
      return transform.transform( position );
    }
    catch ( ... )
    {
      return position;
    }
  }
  else
  {
    return position;
  }
}