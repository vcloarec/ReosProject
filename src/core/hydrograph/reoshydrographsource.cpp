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
#include "reoshydrographtransfer.h"

ReosHydrographNode::ReosHydrographNode( ReosHydraulicNetwork *parent ): ReosHydraulicNode( parent )
{}

ReosHydrographSource::ReosHydrographSource( ReosHydraulicNetwork *parent ) : ReosHydrographNode( parent )
{}

ReosHydrographRouting *ReosHydrographSource::outputHydrographTransfer() const
{
  if ( mLinksBySide1.isEmpty() )
    return nullptr;
  return qobject_cast<ReosHydrographRouting *>( mLinksBySide1.at( 0 ) );
}

ReosHydrographSourceFixed::ReosHydrographSourceFixed( ReosHydraulicNetwork *parent ):
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


ReosHydrographJunction::ReosHydrographJunction( const QPointF &position, ReosHydraulicNetwork *parent ):
  ReosHydrographSource( parent )
  , mHydrograph( new ReosHydrograph( this ) ),
  mPosition( position )
{}

ReosHydrograph *ReosHydrographJunction::outputHydrograph( const ReosCalculationContext &context )
{
  mHydrograph->clear();
  bool first = true;

  for ( const QPointer<ReosHydraulicLink> &link : mLinksBySide2 )
  {
    ReosHydrographRouting *transfer = qobject_cast<ReosHydrographRouting *>( link );
    if ( transfer )
    {
      ReosHydrograph *transferhydrograph = transfer->outputHydrograph( context );
      if ( first && transferhydrograph )
      {
        mHydrograph->referenceTime()->setValue( transferhydrograph->referenceTime()->value() );
        first = false;
      }

      if ( transferhydrograph )
        mHydrograph->addOther( *transferhydrograph );
    }
  }

  return mHydrograph;
}

QPointF ReosHydrographJunction::position() const
{
  return mPosition;
}

ReosHydrographTransferDirect::ReosHydrographTransferDirect( ReosHydraulicNetwork *parent ): ReosHydrographRouting( parent ) {}

ReosHydrographTransferDirect::ReosHydrographTransferDirect( ReosHydrographSource *hydrographSource, ReosHydrographNode *destination, ReosHydraulicNetwork *parent ):
  ReosHydrographRouting( hydrographSource, destination, parent )
{}

ReosHydrograph *ReosHydrographTransferDirect::outputHydrograph( const ReosCalculationContext &context )
{
  ReosHydrographSource *upstreamSource = inputHydrographSource();
  if ( upstreamSource )
    return upstreamSource->outputHydrograph( context );
  else
    return nullptr;
}


ReosHydrographSourceWatershed::ReosHydrographSourceWatershed( ReosWatershed *watershed, ReosHydraulicNetwork *parent ):
  ReosHydrographJunction( watershed ? watershed->outletPoint() : QPointF(), parent )
  , mWatershed( watershed )
{
  if ( mWatershed )
    connect( mWatershed, &ReosWatershed::changed, this, [this] {positionChanged();} );
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

ReosWatershed *ReosHydrographSourceWatershed::watershed() const
{
  if ( mWatershed.isNull() )
    return nullptr;
  else
    return mWatershed.data();
}

QPointF ReosHydrographSourceWatershed::position() const
{
  if ( mWatershed.isNull() )
    return QPointF();
  else
    return mWatershed->outletPoint();
}
