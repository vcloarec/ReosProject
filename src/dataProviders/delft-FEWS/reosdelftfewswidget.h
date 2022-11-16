/***************************************************************************
  reosdelftfewswidget.h - ReosDelftFewsWidget

 ---------------------
 begin                : 9.11.2021
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
#ifndef REOSDELFTFEWSWIDGET_H
#define REOSDELFTFEWSWIDGET_H

#include <QWidget>
#include <QAbstractListModel>
#include <QDomElement>
#include<QPointer>

#include "reosdataprovidergui.h"
#include "reoshydrograph.h"
#include "reossyntheticrainfall.h"
#include "reosmapitem.h"
#include "reosmap.h"
#include "reosdelftfewssettingswidget.h"

class ReosPlotTimeSerieVariableStep;
class ReosPlotTimeHistogram;
class ReosSeriesRainfall;
class ReosMapToolSelectMapItem;

namespace Ui
{
  class ReosDelftFewsWidget;
}

class ReosDelftFewsStationMarker: public ReosMapMarkerFilledCircle
{
  public:
    ReosDelftFewsStationMarker( ReosMap *map, const QPointF &point );
    int stationIndex;
};

struct ReosDelftFewsStation
{
  int index;
  QString dataType() const;
  QVariantMap meta;
};

class ReosDelftFewsStationsModel: public QAbstractListModel
{
  public:
    ReosDelftFewsStationsModel( const QString &dataType, QObject *parent );

    QModelIndex index( int row, int column, const QModelIndex &parent ) const;
    QModelIndex parent( const QModelIndex &child ) const;
    int rowCount( const QModelIndex &parent ) const;
    int columnCount( const QModelIndex &parent ) const;
    QVariant data( const QModelIndex &index, int role ) const;

    void setStationsList( const QList<ReosDelftFewsStation> &stations );

    ReosDelftFewsStation station( int i ) const;

  private:
    QDomElement mRooElement;
    QList<ReosDelftFewsStation> mStations;
    QString mDataType;

    void updateStationsList();
};

class ReosDelftFewsWidget : public ReosDataProviderSelectorWidget
{
    Q_OBJECT

  public:
    explicit ReosDelftFewsWidget( ReosMap *map, const QString &dataType, QWidget *parent = nullptr );
    ~ReosDelftFewsWidget();

    ReosDataObject *createData( QObject *parent ) const override;
    ReosDataObject *selectedData() const override;
    QVariantMap selectedMetadata() const override;

  private slots:
    void onOpenFile();
    void onFileNameChanged();
    void onStationChanged();
    void onStationSelectOnMap( ReosMapItem *item, const QPointF & );
    void populateTextBrowser();

  private:
    Ui::ReosDelftFewsWidget *ui;
    ReosMap *mMap = nullptr;
    QString mDataType;
    ReosMapToolSelectMapItem *mMapToolSelectOnMap = nullptr;
    QAction *mActionSelectOnMap = nullptr;
    std::vector < std::unique_ptr<ReosDelftFewsStationMarker>> mStationsMarker;
    ReosDelftFewsStationsModel *mStationsModel = nullptr;
    QPointer<ReosHydrograph> mCurrentHydrograph = nullptr;
    QPointer<ReosSeriesRainfall> mCurrentRainfall = nullptr;
    ReosPlotTimeSerieVariableStep *mHydrographPlot = nullptr;
    ReosPlotTimeHistogram *mRainfallPlot = nullptr;

    bool parseFile( const QString &fileName );
    ReosHydrograph *createHydrograph( QObject *parent = nullptr ) const;
    ReosSeriesRainfall *createRainfall( QObject *parent = nullptr ) const;

    QString currentUri() const;
};


class ReosDelftFewsGuiFactory : public ReosDataProviderGuiFactory
{
  public:
    GuiCapabilities capabilities() const override;
    QString key() const override;
    ReosDelftFewsWidget *createProviderSelectorWidget( ReosMap *map, const QString &dataType, QWidget *parent = nullptr ) const override;
    ReosDelftFewsSettingsWidget *createProviderSettingsWidget( ReosDataProvider *provider, QWidget *parent = nullptr ) const override;
    QString dataType() const override;
    QIcon icon() const override;
    QString displayText() const override;
};

#endif // REOSDELFTFEWSWIDGET_H
