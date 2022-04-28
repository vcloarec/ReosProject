/***************************************************************************
  reostimeseriesgroup.cpp - ReosTimeSeriesGroup

 ---------------------
 begin                : 10.4.2022
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
#include "reostimeseriesgroup.h"

#include "reostimeserie.h"

ReosTimeSeriesVariableTimeStepGroup::ReosTimeSeriesVariableTimeStepGroup( QObject *parent ): ReosDataObject( parent )
{

}

int ReosTimeSeriesVariableTimeStepGroup::addTimeSeries( ReosTimeSerieVariableTimeStep *timeSeries )
{
  mTimeSeries.append( timeSeries );
  timeSeries->setParent( this );
  connect( timeSeries, &ReosDataObject::dataChanged, this, &ReosTimeSeriesVariableTimeStepGroup::serieChanged );
  emit dataChanged();

  return mTimeSeries.count() - 1;
}

int ReosTimeSeriesVariableTimeStepGroup::timeSeriesCount() const
{
  return mTimeSeries.count();
}

ReosTimeSerieVariableTimeStep *ReosTimeSeriesVariableTimeStepGroup::timeSeries( int index ) const
{
  if ( index < 0 || index >= mTimeSeries.count() )
    return nullptr;

  return mTimeSeries.at( index );
}

void ReosTimeSeriesVariableTimeStepGroup::removeTimeSeries( int index )
{
  if ( index < 0 || index >= mTimeSeries.count() )
    return;

  mTimeSeries.takeAt( index )->deleteLater();

  emit dataChanged();
}

QStringList ReosTimeSeriesVariableTimeStepGroup::seriesNames() const
{
  QStringList ret;

  for ( ReosTimeSerieVariableTimeStep *ts : mTimeSeries )
    ret.append( ts->name() );

  return ret;
}

ReosEncodedElement ReosTimeSeriesVariableTimeStepGroup::encode() const
{
  ReosEncodedElement element( QStringLiteral( "time-series-variable-time-step-group" ) );

  QList<ReosEncodedElement> seriesList;

  for ( ReosTimeSerieVariableTimeStep *series : mTimeSeries )
  {
    seriesList.append( series->encode() );
  }

  element.addListEncodedData( QStringLiteral( "series-list" ), seriesList );

  return element;
}

void ReosTimeSeriesVariableTimeStepGroup::decode( const ReosEncodedElement &element )
{
  qDeleteAll( mTimeSeries );

  mTimeSeries.clear();
  QList<ReosEncodedElement> seriesList = element.getListEncodedData( QStringLiteral( "series-list" ) );

  for ( ReosEncodedElement elem : std::as_const( seriesList ) )
    mTimeSeries.append( ReosTimeSerieVariableTimeStep::decode( elem, this ) );
}