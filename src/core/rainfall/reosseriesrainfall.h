/***************************************************************************
  reosseriesrainfall.h - ReosSeriesRainfall

 ---------------------
 begin                : 22.11.2022
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
#ifndef REOSSERIESRAINFALL_H
#define REOSSERIESRAINFALL_H

#include "reoscore.h"
#include "reostimeserie.h"

class ReosWatershed;
class ReosGriddedRainfall;

class REOSCORE_EXPORT ReosSeriesRainfall : public ReosTimeSerieConstantInterval
{
    Q_OBJECT
  public:
    ReosSeriesRainfall( QObject *parent = nullptr, const QString &providerKey = QString(), const QString &dataSource = QString() );

    QString type() const override {return staticType();}

    ReosEncodedElement encode() const;

    //! Creates new instance from the encoded element
    static ReosSeriesRainfall *decode( const ReosEncodedElement &element, QObject *parent = nullptr );
    static QString staticType();

  protected:
    ReosSeriesRainfall( const ReosEncodedElement &element, QObject *parent = nullptr );

  private:
    void setupData();

};


class ReosSeriesRainfallFromGriddedOnWatershed : public ReosSeriesRainfall
{
    Q_OBJECT
  public:
    ReosSeriesRainfallFromGriddedOnWatershed( ReosWatershed *watershed, ReosGriddedRainfall *griddedRainfall, QObject *parent = nullptr );

  protected:
    void updateData() const override;

  private:
    QPointer<ReosWatershed> mWatershed;
    QPointer<ReosGriddedRainfall> mGriddedRainfall;

    void updateAverageRainfallCalculation() const;

};


#endif // REOSSERIESRAINFALL_H
