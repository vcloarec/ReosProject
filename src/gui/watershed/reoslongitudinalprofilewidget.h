/***************************************************************************
  reoslongitudinalprofilewidget.h - ReosLongitudinalProfileWidget

 ---------------------
 begin                : 11.1.2021
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
#ifndef REOSLONGITUDINALPROFILEWIDGET_H
#define REOSLONGITUDINALPROFILEWIDGET_H

#include <QWidget>

#include "reoseditableprofile.h"

namespace Ui
{
  class ReosLongitudinalProfileWidget;
}

namespace QtCharts
{
  class QChartView;
  class QChart;
}

class QAction;
class ReosWatershed;
class ReosPlotWidget;
class ReosMap;

class ReosLongitudinalProfileWidget : public QWidget
{
    Q_OBJECT

  public:
    explicit ReosLongitudinalProfileWidget( ReosMap *map, QWidget *parent = nullptr );
    ~ReosLongitudinalProfileWidget();

    //! Sets the action that will commmand open/close the widget
    void setAction( QAction *action );

  public slots:
    void setCurrentWatershed( ReosWatershed *ws );

  private slots:
    void updateProfile();

  protected:
    void closeEvent( QCloseEvent *event );
  private:
    Ui::ReosLongitudinalProfileWidget *ui;
    ReosMap *mMap = nullptr;
    QAction *mAction = nullptr;
    ReosWatershed *mCurrentWatershed = nullptr;

    ReosEditableProfile *mProfile = nullptr;
    ReosPlotCurve *mDemCurve = nullptr;

    void updateDEMProfile();


    void storeGeometry();
    void restore();
};

#endif // REOSLONGITUDINALPROFILEWIDGET_H
