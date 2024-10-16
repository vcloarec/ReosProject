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

#define SIP_NO_FILE

#include "reosdataobject.h"
#include "reosencodedelement.h"

class ReosTimeSeriesVariableTimeStep;

class REOSCORE_EXPORT ReosTimeSeriesVariableTimeStepGroup : public ReosDataObject
{
    Q_OBJECT
  public:
    ReosTimeSeriesVariableTimeStepGroup( QObject *parent = nullptr );

    int addTimeSeries( ReosTimeSeriesVariableTimeStep *timeSeries );
    int timeSeriesCount() const;

    ReosTimeSeriesVariableTimeStep *timeSeries( int index ) const;

    void removeTimeSeries( int index );

    QStringList seriesNames() const;

    ReosEncodedElement encode( const ReosEncodeContext &context ) const;
    void decode( const ReosEncodedElement &element, const ReosEncodeContext &context );

  signals:
    void serieChanged();

  private:
    QList<ReosTimeSeriesVariableTimeStep *> mTimeSeries;
};

#endif // REOSTIMESERIESGROUP_H
