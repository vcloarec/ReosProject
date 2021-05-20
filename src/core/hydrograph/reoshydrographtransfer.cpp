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

ReosHydrographTransfer::ReosHydrographTransfer( QObject *parent ): ReosHydraulicLink( parent )
{}

void ReosHydrographTransfer::setInputHydrographSource( ReosHydrographSource *hydrographSource )
{
  attachOnSide1( mNode_1 );
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

ReosHydrographJunction::ReosHydrographJunction( QObject *parent ):
  ReosHydrographSource( parent )
  , mHydrograph( new ReosHydrograph( this ) )
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

ReosHydrograph *ReosHydrographTransferDirect::outputHydrograph( const ReosCalculationContext &context )
{
  ReosHydrographSource *upstreamSource = inputHydrographSource();
  if ( upstreamSource )
    return upstreamSource->outputHydrograph( context );
  else
    return nullptr;
}
