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

ReosHydrograph *ReosHydrographSourceFixed::outputHydrograph()
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

ReosHydrograph *ReosHydrographJunction::outputHydrograph( )
{
  return mHydrograph;
}

QPointF ReosHydrographJunction::position() const
{
  return mPosition;
}

void ReosHydrographJunction::setPosition( const QPointF &pos )
{
  mPosition = pos;
  positionChanged();
}

void ReosHydrographJunction::updateCalculation( const ReosCalculationContext &context )
{
  //update upstream routing
  for ( const QPointer<ReosHydraulicLink> &link : mLinksBySide2 )
  {
    ReosHydrographRouting *routing = qobject_cast<ReosHydrographRouting *>( link );
    if ( routing )
    {
      mWaitingForUpstreamLinksUpdated.append( routing->id() );
      routing->updateCalculation( context );
    }
  }

  if ( mWaitingForUpstreamLinksUpdated.isEmpty() )
  {
    calculateHydrograph();
    calculationUpdated();
  }
}



void ReosHydrographJunction::calculateHydrograph()
{
  bool first = true;
  mHydrograph->clear();

  for ( const QPointer<ReosHydraulicLink> &link : mLinksBySide2 )
  {
    ReosHydrographRouting *routing = qobject_cast<ReosHydrographRouting *>( link );
    if ( routing )
    {
      ReosHydrograph *transferhydrograph = routing->outputHydrograph();
      if ( first && transferhydrograph )
      {
        mHydrograph->referenceTime()->setValue( transferhydrograph->referenceTime()->value() );
        first = false;
      }

      if ( transferhydrograph )
        mHydrograph->addOther( *transferhydrograph );
    }
  }
}


ReosHydrographSourceWatershed::ReosHydrographSourceWatershed( ReosWatershed *watershed, ReosHydraulicNetwork *parent ):
  ReosHydrographJunction( watershed ? watershed->outletPoint() : QPointF(), parent )
  , mWatershed( watershed )
{
  if ( mWatershed )
    connect( mWatershed, &ReosWatershed::changed, this, [this] {positionChanged();} );
}

ReosHydrograph *ReosHydrographSourceWatershed::outputHydrograph()
{
  if ( mWatershed.isNull() )
    return nullptr;

  return mHydrograph;
}

ReosWatershed *ReosHydrographSourceWatershed::watershed() const
{
  if ( mWatershed.isNull() )
    return nullptr;
  else
    return mWatershed.data();
}

void ReosHydrographSourceWatershed::updateCalculation( const ReosCalculationContext &context )
{
  if ( mHydrograph )
    mHydrograph->deleteLater();

  if ( mWatershed.isNull() )
    return;

  mHydrograph = mWatershed->createHydrograph( context.meteorologicModel()->associatedRainfall( mWatershed )->data(), this );
  mHydrograph->setColor( QColor( 50, 150, 75 ) );

  calculationUpdated();
}

QPointF ReosHydrographSourceWatershed::position() const
{
  if ( mWatershed.isNull() )
    return QPointF();
  else
    return mWatershed->outletPoint();
}
