/***************************************************************************
  reostimeseriesgroup.h - ReosTimeSeriesGroup

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
#ifndef REOSTIMESERIESGROUP_H
#define REOSTIMESERIESGROUP_H

#include "reosdataobject.h"

class ReosTimeSerieVariableTimeStep;
 
class REOSCORE_EXPORT ReosTimeSeriesVariableTimeStepGroup : public ReosDataObject
{
  public:
    ReosTimeSeriesVariableTimeStepGroup( QObject *parent = nullptr );

    int addTimeSeries( ReosTimeSerieVariableTimeStep *timeSeries );
    int timeSeriesCount() const;

    ReosTimeSerieVariableTimeStep *timeSeries( int index ) const;

    void removeTimeSeries( int index );

    QStringList seriesNames() const;

    ReosEncodedElement encode() const;
    void decode( const ReosEncodedElement &element );

  private:
    QList<ReosTimeSerieVariableTimeStep *> mTimeSeries;
};

#endif // REOSTIMESERIESGROUP_H
