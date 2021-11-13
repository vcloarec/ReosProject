/***************************************************************************
  reoshubeauwidget.h - ReosHubEauWidget

 ---------------------
 begin                : 1.11.2021
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
#ifndef REOSHUBEAUWIDGET_H
#define REOSHUBEAUWIDGET_H

#include <memory>
#include <QPointer>

#include "reosactionwidget.h"
#include "reoshubeauserver.h"
#include "reosmapitem.h"
#include "reosmap.h"
#include "reoshydrograph.h"
#include "reosdataprovidergui.h"
#include "reoshubeausettingswidget.h"

class ReosMapMarker;

namespace Ui
{
  class ReosHubEauWidget;
}

class ReosHubEauServer;
class ReosMapToolSelectMapItem;
class ReosHubEauStationMarker;
class ReosHydrograph;
class ReosPlotTimeSerieVariableStep;

class ReosHubEauStationMarker: public ReosMapMarkerFilledCircle
{
  public:
    ReosHubEauStationMarker( ReosMap *map, const QPointF &point );
    int stationIndex;
};

class ReosHubEauWidget : public ReosDataProviderSelectorWidget
{
    Q_OBJECT
  public:
    explicit ReosHubEauWidget( ReosMap *map, QWidget *parent = nullptr );
    ~ReosHubEauWidget();

    ReosHydrograph *createData( QObject *parent = nullptr ) const override;
    ReosHydrograph *selectedData() const override;

    //! Returns the station Id of the current station conformly to hub-eau specifiations
    QString currentStationId() const;

    //! Sets the station Id of the current station conformly to hub-eau specifiations
    void setCurrentStationId( const QString &currentStationId );

  public slots:
    //! Called when the widget is closed
    void onClosed() override;

    //! Called when the widget is open
    void onOpened() override;

  private slots:
    void onMapExtentChanged();
    void onStationUpdated();
    void onSelectStation( ReosMapItem *item, const QPointF & );
    void onHydrographUpdated();

  private:
    Ui::ReosHubEauWidget *ui;
    ReosMap *mMap = nullptr;
    ReosHubEauServer *mServer = nullptr;
    QList<ReosHubEauStation> mStations;
    std::vector < std::unique_ptr<ReosHubEauStationMarker>> mStationsMarker;
    ReosMapToolSelectMapItem *mSelectStation = nullptr;
    QPointer<ReosHydrograph> mCurrentHydrograph = nullptr;
    ReosPlotTimeSerieVariableStep *mHydrographPlot = nullptr;
    ReosHubEauStationMarker *mCurrentMarker = nullptr;
    QString mCurrentStationId;
    QVariantMap mCurrentStationMeta;

    void populateMeta( const QVariantMap &meta );
};

class ReosHubEauHydrometryGuiFactory : public ReosDataProviderGuiFactory
{
  public:
    GuiCapabilities capabilities() const override;
    QString key() const override;
    ReosHubEauWidget *createProviderSelectorWidget( ReosMap *map, QWidget *parent = nullptr ) const override;
    ReosHubEauSettingsWidget *createProviderSettingsWidget( ReosDataProvider *provider, QWidget *parent = nullptr ) const override;
    QString dataType() const override;
    QPixmap icon() const override;
};

#endif // REOSHUBEAUWIDGET_H
