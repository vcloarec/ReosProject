/***************************************************************************
  reostopographycollection_p.cpp - ReosTopographyCollection_p

 ---------------------
 begin                : 28.2.2022
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
#include "reostopographycollection_p.h"

#include "reosdigitalelevationmodel_p.h"
#include "reosgisengine.h"

ReosTopographyCollection_p::ReosTopographyCollection_p( ReosGisEngine *gisEngine, QObject *parent )
  : ReosTopographyCollection( gisEngine, parent )
{}

ReosTopographyCollection_p::ReosTopographyCollection_p( const ReosEncodedElement &element, ReosGisEngine *gisEngine, QObject *parent )
  : ReosTopographyCollection( element, gisEngine, parent )
{}

void ReosTopographyCollection_p::prepare_p( const QgsCoordinateReferenceSystem &sourceCrs ) const
{
  clean_p();

  for ( const QString &layerId : mTopographyIds )
  {
    if ( !mGisEngine->isDigitalElevationModel( layerId ) )
      continue;

    std::unique_ptr<ReosDigitalElevationModel> dem( mGisEngine->getDigitalElevationModel( layerId ) );
    if ( dynamic_cast<ReosDigitalElevationModelRaster *>( dem.get() ) )
    {
      mDems.emplace_back( dynamic_cast<ReosDigitalElevationModelRaster *>( dem.release() ) );
      mTransforms.append( mDems.back()->transformToDem( sourceCrs ) );
    }
  }
}

double ReosTopographyCollection_p::elevationAt_p( const QgsPointXY &point ) const
{
  if ( mDems.empty() )
    return 0;

  for ( size_t i = 0; i < mDems.size(); ++i )
  {
    double value = mDems.at( i )->elevationAt( point, mTransforms.at( int( i ) ) );
    if ( value != mDems.at( i )->noDataValue() && !std::isnan( value ) )
      return value;
  }

  return std::numeric_limits<double>::quiet_NaN();
}

void ReosTopographyCollection_p::clean_p() const
{
  mDems.clear();
  mTransforms.clear();
}
