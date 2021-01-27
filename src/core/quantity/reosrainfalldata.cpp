/***************************************************************************
  reosrainfalldata.cpp - ReosRainfallData

 ---------------------
 begin                : 26.1.2021
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
#include "reosrainfalldata.h"

ReosRainfallData::ReosRainfallData()
{

}

int ReosRainfallData::valueCount() const
{
  return mValuePerTimeStep.count();
}

ReosDuration ReosRainfallData::timeStep() const
{
  return mTimeStep;
}

void ReosRainfallData::setTimeStep( const ReosDuration &ts )
{
  mTimeStep = ts;
}

double ReosRainfallData::value( int i, ReosRainfallData::ValueMode mode ) const
{
  switch ( mode )
  {
    case ReosRainfallData::HeightPerTimeStep:
      return static_cast<double>( mValuePerTimeStep.at( i ) );
      break;
    case ReosRainfallData::HeightIncremental:
    {
      if ( mLastIncremantalRequestPos > i )
        mLastIncremantalRequestValue = -1;

      for ( int j = mLastIncremantalRequestPos + 1; j <= i; ++j )
        mLastIncremantalRequestValue += mValuePerTimeStep.at( j );
      mLastIncremantalRequestPos = i;
    }
    break;
    case ReosRainfallData::Intensity:
      return mValuePerTimeStep.at( i ) / mTimeStep.valueHour();
      break;
  }

  return 0;
}
