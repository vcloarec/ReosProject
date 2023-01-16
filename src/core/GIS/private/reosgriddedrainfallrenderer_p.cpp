/***************************************************************************
  reosgriddedrainfallrenderer_p.cpp - ReosGriddedRainfallRenderer_p

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
#include "reosgriddedrainfallrenderer_p.h"

#include <QElapsedTimer>

#include "reosrenderersettings_p.h"
#include "reosgriddedrainitem.h"
#include "reosmemoryraster.h"

#include <qgsgdalutils.h>
#include <qgssinglebandpseudocolorrenderer.h>
#include <qgsrastershader.h>
#include <qgscolorrampimpl.h>



ReosGriddedRainfallRendererFactory_p::ReosGriddedRainfallRendererFactory_p( ReosGriddedRainfall *rainfall )
  : ReosGriddedRainfallRendererFactory( rainfall )
  , mColorRampSettings( new ReosGriddedRainfallColorShaderSettings_p( this ) )
{
  init();

  QList<QgsGradientStop> stops;
  stops << QgsGradientStop( 0.005, QColor( 200, 200, 255 ) );
  std::unique_ptr<QgsGradientColorRamp> colorRamp( new QgsGradientColorRamp( QColor( 255, 255, 255, 0 ), Qt::blue, false, stops ) );

  std::unique_ptr<QgsSingleBandPseudoColorRenderer> renderer( new QgsSingleBandPseudoColorRenderer( mRasterLayer->dataProvider(), 1 ) );
  double min = 0, max = 0;
  if ( rainfall->getDirectMinMaxValue( min, max ) )
  {
    renderer->setClassificationMin( min );
    renderer->setClassificationMax( max );
  }
  else
  {
    renderer->setClassificationMin( 0 );
    renderer->setClassificationMax( 20 );
  }

  renderer->createShader( colorRamp.release(), QgsColorRampShader::Interpolated, QgsColorRampShader::EqualInterval, 10, false );
  mColorRampSettings->setShader( renderer->shader()->rasterShaderFunction() );
  mRasterLayer->setRenderer( renderer.release() );
}

void ReosGriddedRainfallRendererFactory_p::init()
{
  ReosMapExtent extent = mRainfall->rasterExtent();
  QgsRectangle qgsExt = extent.toRectF();
  qgsExt.normalize();
  QString uri( QStringLiteral( "%1::%2" ).arg( qgsExt.asWktPolygon(), extent.crs() ) );

  mRasterLayer.reset( new QgsRasterLayer( uri, QString(), QStringLiteral( "gridded-precipitation" ) ) );
  mDataProvider = qobject_cast<ReosGriddedRainfallRasterProvider_p *>( mRasterLayer->dataProvider() );

  QObject::connect( mColorRampSettings.get(), &ReosColorShaderSettings::changed, mRainfall, &ReosRenderedObject::repaintRequested );
}


ReosGriddedRainfallRendererFactory_p::ReosGriddedRainfallRendererFactory_p( const ReosEncodedElement &element, ReosGriddedRainfall *rainfall )
  : ReosGriddedRainfallRendererFactory( rainfall )
  , mColorRampSettings( new ReosGriddedRainfallColorShaderSettings_p( this ) )
{
  init();

  std::unique_ptr<QgsSingleBandPseudoColorRenderer> renderer( new QgsSingleBandPseudoColorRenderer( mRasterLayer->dataProvider(), 1 ) );
  renderer->createShader();
  mRasterLayer->setRenderer( renderer.release() );
  mColorRampSettings->decode( element.getEncodedData( QStringLiteral( "color-ramp-settings" ) ) );
  mColorRampSettings->onSettingsUpdated();
}

ReosEncodedElement ReosGriddedRainfallRendererFactory_p::encode() const
{
  ReosEncodedElement element( QStringLiteral( "gridded-rainfall-renderer" ) );
  element.addEncodedData( QStringLiteral( "color-ramp-settings" ), mColorRampSettings->encode() );
  return element;
}

ReosObjectRenderer *ReosGriddedRainfallRendererFactory_p::createRasterRenderer( ReosRendererSettings *settings )
{
  if ( !mDataProvider )
    return nullptr;
  ReosRendererSettings_p *rendererSettings = dynamic_cast<ReosRendererSettings_p *>( settings );
  const QDateTime time = rendererSettings->settings().temporalRange().begin();

  int index = mRainfall->dataIndex( time );
  mDataProvider->setData( mRainfall->intensityValues( index ) );
  mDataProvider->setExtent( mRainfall->rasterExtent() );
  std::unique_ptr<ReosQgisLayerRenderer_p> objectRenderer( new ReosQgisLayerRenderer_p( settings, mRasterLayer.get(), mRainfall ) );

  return objectRenderer.release();
}

ReosColorShaderSettings *ReosGriddedRainfallRendererFactory_p::colorRampShaderSettings() const
{
  return mColorRampSettings.get();
}

void ReosGriddedRainfallRendererFactory_p::setColorRampShaderSettings( ReosColorShaderSettings *colorSettings )
{
  if ( ReosGriddedRainfallColorShaderSettings_p *newColorSettings =
         qobject_cast<ReosGriddedRainfallColorShaderSettings_p *>( colorSettings ) )
  {
    mColorRampSettings.reset( newColorSettings );
    mColorRampSettings->mRendererfactory = this;
    setColorRampShader( newColorSettings->mColorShader );
    QObject::connect( mColorRampSettings.get(), &ReosColorShaderSettings::changed, mRainfall, &ReosRenderedObject::repaintRequested );
  }
  else //not the good type, we have to destruct it because we take the ownership
    delete colorSettings;

}


QgsColorRampShader ReosGriddedRainfallRendererFactory_p::colorRampShader() const
{
  if ( mRasterLayer->renderer()->type() !=  QStringLiteral( "singlebandpseudocolor" ) )
    return QgsColorRampShader();

  QgsSingleBandPseudoColorRenderer *renderer = dynamic_cast<QgsSingleBandPseudoColorRenderer *>( mRasterLayer->renderer() );
  if ( !renderer )
    return QgsColorRampShader();

  const QgsColorRampShader *rampShader = dynamic_cast<const QgsColorRampShader *>( renderer->shader()->rasterShaderFunction() );

  if ( !rampShader )
    return QgsColorRampShader();

  return *( rampShader );
}

void ReosGriddedRainfallRendererFactory_p::setColorRampShader( const QgsColorRampShader &colorRampShader )
{
  if ( mRasterLayer->renderer()->type() !=  QStringLiteral( "singlebandpseudocolor" ) )
    return;

  QgsSingleBandPseudoColorRenderer *renderer = dynamic_cast<QgsSingleBandPseudoColorRenderer *>( mRasterLayer->renderer() );

  if ( !renderer )
    return;

  QgsColorRampShader *rampShader = dynamic_cast<QgsColorRampShader *>( renderer->shader()->rasterShaderFunction() );

  if ( !rampShader )
    return;

  *( rampShader ) = colorRampShader;
}

ReosGriddedRainfallRasterProvider_p::ReosGriddedRainfallRasterProvider_p( const QString &uri )
{
  QStringList uriPart = uri.split( QStringLiteral( "::" ) );
  if ( uriPart.count() == 2 )
  {
    ReosMapExtent extent( QgsRectangle::fromWkt( uriPart.at( 0 ) ).toRectF() );
    mExtent = extent.toRectF();
    mExtent.normalize();
    mCrs = QgsCoordinateReferenceSystem::fromWkt( uriPart.at( 1 ) );
  }
}

QgsRasterDataProvider *ReosGriddedRainfallRasterProvider_p::clone() const
{
  std::unique_ptr<ReosGriddedRainfallRasterProvider_p> newInstance( new ReosGriddedRainfallRasterProvider_p() );
  newInstance->mValues = mValues;
  newInstance->mExtent = mExtent;
  newInstance->mCrs = mCrs;
  newInstance->mXCount = mXCount;
  newInstance->mYCount = mYCount;

  return newInstance.release();
}

QgsRectangle ReosGriddedRainfallRasterProvider_p::extent() const
{
  return mExtent;
}

int ReosGriddedRainfallRasterProvider_p::xSize() const
{
  return mXCount;
}

int ReosGriddedRainfallRasterProvider_p::ySize() const
{
  return mYCount;
}

//int ReosGriddedRainfallRasterProvider_p::capabilities() const
//{
//  return Size;
//}

bool ReosGriddedRainfallRasterProvider_p::readBlock(
  int,
  const QgsRectangle &viewExtent,
  int width,
  int height,
  void *data,
  QgsRasterBlockFeedback * )
{
  int pixelWidth = mXCount;
  int pixelHeight = mYCount;
  QElapsedTimer timer;
  timer.start();

  GDALResampleAlg alg = GRA_NearestNeighbour;//GRA_Cubic;

  gdal::dataset_unique_ptr memData =
    QgsGdalUtils::blockToSingleBandMemoryDataset( pixelWidth, pixelHeight, mExtent, const_cast<double *>( mValues.constData() ),  GDALDataType::GDT_Float64 );

  gdal::dataset_unique_ptr gdalDsOutput = QgsGdalUtils::blockToSingleBandMemoryDataset( width, height, viewExtent, data, GDT_Float64 );

  if ( !memData || ! gdalDsOutput )
    return false;

  bool res = QgsGdalUtils::resampleSingleBandRaster( memData.get(), gdalDsOutput.get(), alg, nullptr );

  return res;
}

void ReosGriddedRainfallRasterProvider_p::setData( const QVector<double> values )
{
  mValues = values;
}

void ReosGriddedRainfallRasterProvider_p::setExtent( const ReosRasterExtent &extent )
{
  mExtent = extent.toRectF();
  mExtent.normalize();
  mCrs = QgsCoordinateReferenceSystem::fromWkt( extent.crs() );
  mXCount = extent.xCellCount();
  mYCount = extent.yCellCount();
}

ReosGriddedRainfallProviderMetaData::ReosGriddedRainfallProviderMetaData()
  : QgsProviderMetadata( QStringLiteral( "gridded-precipitation" ), QStringLiteral( "Gridded precipitation" ) )
{}


ReosGriddedRainfallRasterProvider_p *ReosGriddedRainfallProviderMetaData::createProvider( const QString &uri, const QgsDataProvider::ProviderOptions &, QgsDataProvider::ReadFlags )
{
  return new ReosGriddedRainfallRasterProvider_p( uri );
}

ReosRendererGriddedRainfallMapTimeStamp_p::ReosRendererGriddedRainfallMapTimeStamp_p( int index )
  : mDataIndex( index )
{}

bool ReosRendererGriddedRainfallMapTimeStamp_p::equal( ReosRendererObjectMapTimeStamp *other )
{
  ReosRendererGriddedRainfallMapTimeStamp_p *other_p = dynamic_cast<ReosRendererGriddedRainfallMapTimeStamp_p *>( other );
  if ( !other_p )
    return false;

  bool test = other_p->mDataIndex >= 0 || mDataIndex >= 0 ;

  return !( test && other_p->mDataIndex != mDataIndex );
}

ReosGriddedRainfallColorShaderSettings_p::ReosGriddedRainfallColorShaderSettings_p( ReosGriddedRainfallRendererFactory_p *rendererFactory ):
  mRendererfactory( rendererFactory )
{}

ReosGriddedRainfallColorShaderSettings_p *ReosGriddedRainfallColorShaderSettings_p::clone() const
{
  std::unique_ptr<ReosGriddedRainfallColorShaderSettings_p> other( new ReosGriddedRainfallColorShaderSettings_p );
  other->mColorShader = mColorShader;
  return other.release();
}


bool ReosGriddedRainfallColorShaderSettings_p::isValid() const
{
  return true;
}

double ReosGriddedRainfallColorShaderSettings_p::classificationMinimum() const
{
  return mColorShader.minimumValue();
}

void ReosGriddedRainfallColorShaderSettings_p::setClassificationMinimum( double newClassificationMinimum )
{
  mColorShader.setMinimumValue( newClassificationMinimum );
}

double ReosGriddedRainfallColorShaderSettings_p::classificationMaximum() const
{
  return mColorShader.maximumValue();
}

void ReosGriddedRainfallColorShaderSettings_p::setClassificationMaximum( double newClassificationMaximum )
{
  mColorShader.setMaximumValue( newClassificationMaximum );
}

double ReosGriddedRainfallColorShaderSettings_p::opacity() const
{
  return -1;
}

void ReosGriddedRainfallColorShaderSettings_p::setOpacity( double )
{
}

bool ReosGriddedRainfallColorShaderSettings_p::getDirectSourceMinMax( double &min, double &max ) const
{
  if ( !mRendererfactory || !mRendererfactory->mRainfall )
  {
    min = std::numeric_limits<double>::quiet_NaN();
    max = std::numeric_limits<double>::quiet_NaN();
    return false;
  }

  return mRendererfactory->mRainfall->getDirectMinMaxValue( min, max );
}

void ReosGriddedRainfallColorShaderSettings_p::calculateSourceMinMax( double &min, double &max ) const
{
  if ( !mRendererfactory || !mRendererfactory->mRainfall )
  {
    min = std::numeric_limits<double>::quiet_NaN();
    max = std::numeric_limits<double>::quiet_NaN();
    return;
  }

  mRendererfactory->mRainfall->calculateMinMaxValue( min, max );
}

void ReosGriddedRainfallColorShaderSettings_p::onSettingsUpdated()
{
  mRendererfactory->setColorRampShader( mColorShader );
  emit changed();
}

QString ReosGriddedRainfallColorShaderSettings_p::title() const
{
  if ( mRendererfactory && mRendererfactory->rainfall() )
    return mRendererfactory->rainfall()->name();
  return QString();
}
