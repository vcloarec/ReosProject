/***************************************************************************
  reoshydrographsource.cpp - ReosHydrographSource

 ---------------------
 begin                : 19.5.2021
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
#include "reoshydrographsource.h"
#include "reoshydrograph.h"
#include "reoscalculationcontext.h"


ReosHydrographNode::ReosHydrographNode( QObject *parent ): ReosHydraulicNode( parent )
{}

ReosHydrographSource::ReosHydrographSource( QObject *parent ) : ReosHydrographNode( parent )
{}

ReosHydrographSourceFixed::ReosHydrographSourceFixed( QObject *parent ):
  ReosHydrographSource( parent )
{}

ReosHydrograph *ReosHydrographSourceFixed::outputHydrograph( const ReosCalculationContext &context )
{
  return mHydrograph;
}

void ReosHydrographSourceFixed::setHydrograph( ReosHydrograph *hydrograph )
{
  mHydrograph = hydrograph;
  mHydrograph->setParent( this );
}


ReosHydrograph *ReosHydrographSourceWatershed::outputHydrograph( const ReosCalculationContext &context )
{
  if ( mWatershed.isNull() )
    return nullptr;
  if ( mHydrograph )
    mHydrograph->deleteLater();
  mHydrograph = mWatershed->createHydrograph( context.meteorologicModel()->associatedRainfall( mWatershed )->data(), this );

  return mHydrograph;
}
