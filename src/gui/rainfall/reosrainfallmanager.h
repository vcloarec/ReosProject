/***************************************************************************
  reosrainfallmanager.h - ReosRainfallManager

 ---------------------
 begin                : 24.1.2021
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
#ifndef REOSRAINFALLMANAGER_H
#define REOSRAINFALLMANAGER_H

#include <QWidget>
#include <QDialog>
#include "reosactionwidget.h"
#include "reosrainfallitem.h"
#include "reosplotwidget.h"
#include "reosformwidget.h"

namespace Ui
{
  class ReosRainfallManager;
}

class QAction;
class ReosRainfallModel;
class ReosRainfallDataItem;
class ReosStationItem;
class ReosPlotWidget;
class ReosMapToolDrawPoint;
class ReosMap;
class ReosMapItem;
class ReosMapMarkerSvg;
class ReosMapToolMoveMapItem;


//! Widget to handle rainfall data
class REOSGUI_EXPORT ReosRainfallManager : public ReosActionWidget
{
    Q_OBJECT

  public:
    explicit ReosRainfallManager( ReosMap *map, ReosRainfallModel *rainfallmodel, QWidget *parent = nullptr );
    ~ReosRainfallManager();

    //! Loads the data file defined on settings
    void loadDataFile();

  public slots:
    void saveRainfallFile();

  private slots:
    void onOpenRainfallFile();
    void onSaveAsRainfallFile();
    void onAddRootZone();
    void onAddZoneToZone();
    void onAddStation();
    void onAddStationOnMap( const QPointF &point );
    void onAddGaugedRainfall();
    void onAddChicagoRainfall();
    void onAddAlternatingBlockRainfall();
    void onAddDoubleTriangleRainfall();
    void onAddIDFCurves();
    void onAddIDCurve();
    void onReorderIDCurve();
    void onRemoveItem();
    void onCurrentTreeIndexChanged();
    void onTreeViewContextMenu( const QPoint &pos );

    void onImportFromTextFile();

    void updateMarkers();

  private:
    Ui::ReosRainfallManager *ui;
    ReosMap *mMap = nullptr;
    ReosRainfallModel *mModel = nullptr;
    QAction *mAction;

    QAction *mActionOpenRainfallDataFile = nullptr;
    QAction *mActionSaveRainfallDataFile = nullptr;
    QAction *mActionSaveAsRainfallDataFile = nullptr;
    QAction *mActionAddRootZone = nullptr;
    QAction *mActionAddZoneToZone = nullptr;
    QAction *mActionAddStation = nullptr;
    QAction *mActionAddStationOnMap = nullptr;
    QAction *mActionAddGaugedRainfall = nullptr;
    QAction *mActionAddChicagoRainfall = nullptr;
    QAction *mActionAddAlternatingBlockRainfall = nullptr;
    QAction *mActionAddDoubleTriangleRainfall = nullptr;
    QAction *mActionAddIDFCurves = nullptr;
    QAction *mActionAddIDCurve = nullptr;
    QAction *mActionReorderIdVurve = nullptr;
    QAction *mActionRemoveItem = nullptr;

    QAction *mActionImportFromTextFile = nullptr;

    ReosMapToolDrawPoint *mMapToolAddStationOnMap;

    ReosFormWidget *mCurrentForm = nullptr;
    ReosPlotWidget *mCurrentPlot = nullptr;

    std::map<ReosStationItem *, std::unique_ptr<ReosMapMarkerSvg>> mStationsMarker;
    ReosMapItem *addMapItem( ReosRainfallItem *item );
    void removeMarker( ReosRainfallItem *item );
    void buildMarkers();
    void clearMarkers();
    void setMarkersVisible( bool b );

    void selectItem( ReosRainfallItem *item );
    bool saveOnFile( const QString &fileName );
    QList<QAction *> dataItemActions( ReosRainfallDataItem *dataItem );
    bool addSimpleItemDialog( const QString &title, QString &name, QString &description );
    void addStation( const QPointF &point = QPointF(), bool isSpattial = false );

    ReosFormWidget *createForm( ReosRainfallItem *item );
    void setupFormForStation( ReosFormWidget *form, ReosStationItem *stationItem );
};

class ReosTextFileData;
class QComboBox;
class QDialogButtonBox;
class QLabel;
class QToolButton;
class QTreeView;
class ReosTimeSerieConstantInterval;

class ReosImportRainfallDialog: public QDialog
{
    Q_OBJECT
  public:
    explicit ReosImportRainfallDialog( ReosRainfallModel *model, QWidget *parent = nullptr );

  private slots:
    void onImportButton();
    void onSelectStationButton();

  private:
    ReosRainfallModel *mModel = nullptr;
    ReosTextFileData *mTextFile = nullptr;
    QComboBox *mComboSelectedField = nullptr;
    ReosSerieRainfall *mImportedRainfall = nullptr;
    QToolButton *mImportButton = nullptr;
    QToolButton *mSelectStationButton = nullptr;
    ReosParameterString *mName = nullptr;
    ReosParameterString *mDescription = nullptr;
};


class ReosPlotItemRainfallIntensityDurationFrequencyFactory: public ReosDataPlotItemFactory
{
  public:
    QString datatype() const override {return QStringLiteral( "rainfall-intensity-duration-frequency-curves" );}
    void buildPlotItems( ReosPlotWidget *plotWidget, ReosDataObject *data ) override;
};

class ReosPlotItemRainfallIntensityDurationFactory: public ReosDataPlotItemFactory
{
  public:
    QString datatype() const override {return QStringLiteral( "rainfall-intensity-duration-curve" );}
    void buildPlotItems( ReosPlotWidget *plotWidget, ReosDataObject *data ) override;
};

class ReosPlotItemRainfallSerieFactory: public ReosDataPlotItemFactory
{
  public:
    QString datatype() const override {return QStringLiteral( "serie-rainfall" );}
    void buildPlotItems( ReosPlotWidget *plotWidget, ReosDataObject *data ) override;
};

class ReosPlotItemRainfallChicagoFactory: public ReosPlotItemRainfallSerieFactory
{
  public:
    QString datatype() const override {return QStringLiteral( "chicago-rainfall" );}
};

class ReosPlotItemRainfallAlternatingBlockFactory: public ReosPlotItemRainfallSerieFactory
{
  public:
    QString datatype() const override {return QStringLiteral( "alternating-block-rainfall" );}
};

class ReosPlotItemRainfallDoubleTriangleFactory: public ReosPlotItemRainfallSerieFactory
{
  public:
    QString datatype() const override {return QStringLiteral( "double-triangle-rainfall" );}
};


class ReosSpatialStationWidgetToolbar: public QWidget
{
    Q_OBJECT
  public:
    ReosSpatialStationWidgetToolbar( ReosMap *map,  ReosMapItem *marker, QWidget *parent = nullptr );
    ~ReosSpatialStationWidgetToolbar();

    void setCurrentMarker( ReosMapItem *currentMarker );

  signals:
    void removeMarker();
    void setMarker( const ReosSpatialPosition &position );
    void movePosition( const ReosSpatialPosition &position );
    void mapOnMarker();

  private:
    ReosMapToolDrawPoint *mSetPositionTool = nullptr;
    ReosMapToolMoveMapItem *mMovePositionTool = nullptr;

    QAction *mActionSetPosition = nullptr;
    QAction *mActionMovePosition = nullptr;
    QAction *mActiontRemovePosition = nullptr;
    QAction *mActionMapOnStation = nullptr;
    ReosMapItem *mCurrentMarker;

    void updateTools();
};

#endif // REOSRAINFALLMANAGER_H
