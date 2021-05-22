/***************************************************************************
  reoshydrographtransfer.cpp - ReosHydrographTransfer

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
#include "reoshydrographtransfer.h"
#include "reoshydrograph.h"

ReosHydrographTransfer::ReosHydrographTransfer( ReosHydraulicNetwork *parent ): ReosHydraulicLink( parent )
{}

void ReosHydrographTransfer::setInputHydrographSource( ReosHydrographSource *hydrographSource )
{
  attachOnSide1( hydrographSource );
}

ReosHydrographSource *ReosHydrographTransfer::inputHydrographSource() const
{
  if ( mNode_1.isNull() )
    return nullptr;
  else
    return qobject_cast<ReosHydrographSource *>( mNode_1 );
}

void ReosHydrographTransfer::setHydrographDestination( ReosHydrographNode *destination )
{
  attachOnSide2( destination );
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
    ReosHydrographTransfer *transfer = qobject_cast<ReosHydrographTransfer *>( link );
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

ReosHydrographTransferDirect::ReosHydrographTransferDirect( ReosHydraulicNetwork *parent ): ReosHydrographTransfer( parent ) {}

ReosHydrograph *ReosHydrographTransferDirect::outputHydrograph( const ReosCalculationContext &context )
{
  ReosHydrographSource *upstreamSource = inputHydrographSource();
  if ( upstreamSource )
    return upstreamSource->outputHydrograph( context );
  else
    return nullptr;
}
