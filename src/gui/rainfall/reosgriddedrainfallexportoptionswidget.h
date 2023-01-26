/***************************************************************************
  reosgriddedrainfallexportoptionswidget.h - ReosGriddedRainfallExportOptionsWidget

 ---------------------
 begin                : 16.1.2023
 copyright            : (C) 2023 by Vincent Cloarec
 email                : vcloarec at gmail dot com
 ***************************************************************************
 *                                                                         *
 *   This program is free software; you can redistribute it and/or modify  *
 *   it under the terms of the GNU General Public License as published by  *
 *   the Free Software Foundation; either version 2 of the License, or     *
 *   (at your option) any later version.                                   *
 *                                                                         *
 ***************************************************************************/
#ifndef REOSGRIDDEDRAINFALLEXPORTOPTIONSWIDGET_H
#define REOSGRIDDEDRAINFALLEXPORTOPTIONSWIDGET_H

#include <QWidget>

#include "reosmemoryraster.h"
#include "reosgriddedrainfallprovider.h"

class ReosParameterDouble;
class ReosMapToolDrawExtent;
class ReosMap;
class ReosGriddedRainfall;

namespace Ui
{
  class ReosGriddedRainfallExportOptionsWidget;
}

class ReosGriddedRainfallExportOptionsWidget : public QWidget
{
    Q_OBJECT

  public:
    explicit ReosGriddedRainfallExportOptionsWidget( QWidget *parent = nullptr );
    ~ReosGriddedRainfallExportOptionsWidget();

    void syncRainfall( ReosGriddedRainfall *rainfall );
    void setSupportedGridOrigin( ReosGriddedRainfallProvider::SupportedGridOrigins origins );

    ReosRasterExtent rasterExtent() const;

    ReosTimeWindow timeWindow() const;

  private slots:
    void reprojectExtent();
    void onAdjustResolution();
    void onAdjustExtent();
    void onTimeFromChanged();
    void onTimeToChanged();

  private:
    Ui::ReosGriddedRainfallExportOptionsWidget *ui;
    ReosRasterExtent mRainfallExtent;
    ReosRasterExtent mCurrentExtent;
    QString mCurrentCrs;
    ReosParameterDouble *mNorthParam = nullptr;
    ReosParameterDouble *mSouthParam = nullptr;
    ReosParameterDouble *mWestParam = nullptr;
    ReosParameterDouble *mEastParam = nullptr;
    ReosParameterDouble *mHorizontalResolution = nullptr;
    ReosParameterDouble *mVerticalResolution = nullptr;

    void setExtent( const ReosRasterExtent &extent );
};

#endif // REOSGRIDDEDRAINFALLEXPORTOPTIONSWIDGET_H
