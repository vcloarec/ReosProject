/***************************************************************************
                      reosdigitalelevationmodel.cpp
                     --------------------------------------
Date                 : 27-09-2020
Copyright            : (C) 2020 by Vincent Cloarec
email                : vcloarec at gmail dot com
 ***************************************************************************
 *                                                                         *
 *   This program is free software; you can redistribute it and/or modify  *
 *   it under the terms of the GNU General Public License as published by  *
 *   the Free Software Foundation; either version 2 of the License, or     *
 *   (at your option) any later version.                                   *
 *                                                                         *
 ***************************************************************************/

#include "reosdigitalelevationmodel.h"

ReosElevationOnPolylineProcess::ReosElevationOnPolylineProcess( ReosDigitalElevationModel *dem ): ReosProcess(), mDem( dem )
{}

void ReosElevationOnPolylineProcess::setEntryPolyline( const QPolygonF &polyline, const QString destinationCRS )
{
  mPolyline = polyline;
  mDestinationCRS = destinationCRS;
}

QPolygonF ReosElevationOnPolylineProcess::resultProfile() const
{
  if ( isSuccessful() )
    return mResult;

  return QPolygonF();
}

void ReosElevationOnPolylineProcess::start()
{
  mIsSuccessful = false;
  if ( mDem )
  {
    mResult = mDem->elevationOnPolyline( mPolyline, mDestinationCRS, this );
    mIsSuccessful = true;
  }
}
