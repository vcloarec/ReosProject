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
#include "reosactionwidget.h"
#include "reosmapitem.h"

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
class ReosMapToolDrawPoint;
class ReosMapToolEditMapPolyline;
class ReosMapToolDrawPolyline;
class ReosGuiContext;


class ReosLongitudinalProfileWidget : public ReosActionWidget
{
    Q_OBJECT

  public:
    explicit ReosLongitudinalProfileWidget( const ReosGuiContext &context );
    ~ReosLongitudinalProfileWidget();

    void setVisibleStreamLine( bool visible );

  public slots:
    void setCurrentWatershed( ReosWatershed *ws );

  private slots:
    void updateProfile();
    void onProfileCursorMove( const QPointF &point );
    void onStreamLineChanged( const QPolygonF &streamLine );
    void onStreamLineEdited();
    void askForUpdateDEMProfile();
    void zoomOnDEMProfileExtent();
    void drawStreamLinefromPointToDownstream( const QPointF &point );
    void drawStreamLinefromPointToUpStream();
    void updateWithDirectionTools();
    void onOpened();

  private:
    Ui::ReosLongitudinalProfileWidget *ui;
    ReosMap *mMap = nullptr;
    QAction *mAction = nullptr;
    ReosWatershed *mCurrentWatershed = nullptr;
    ReosMapPolyline mCurrentStreamLine;

    ReosEditableProfile *mProfile = nullptr;
    ReosPlotCurve *mDemCurve = nullptr;

    QAction *mActionStreamLineFromUpstream = nullptr;
    QAction *mActionDrawStreamLine = nullptr;
    QAction *mActionEditStreamLine = nullptr;
    QAction *mActionZoomOnDEMProfileExtent = nullptr;
    QAction *mActionDrawStreamLineFromDownstream = nullptr;
    QAction *mActionDrawStreamLineFromPointToDownstream = nullptr;

    ReosMapToolDrawPolyline *mMapToolDrawStreamLine = nullptr;
    ReosMapToolEditMapPolyline *mMapToolEditStreamLine = nullptr;
    QActionGroup *mActionGroupStreamLineMapTool;
    ReosMapToolDrawPoint *mMapToolSelectMapUpstreamPoint = nullptr;

    void updateDEMProfile();

    bool mNeedUpdateDEMProfil;
};

#endif // REOSLONGITUDINALPROFILEWIDGET_H
