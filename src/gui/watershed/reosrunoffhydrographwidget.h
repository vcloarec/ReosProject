/***************************************************************************
  reosrunoffhydrographwidget.h - ReosRunoffHydrographWidget

 ---------------------
 begin                : 22.2.2021
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
#ifndef REOSRUNOFFHYDROGRAPHWIDGET_H
#define REOSRUNOFFHYDROGRAPHWIDGET_H

#include <QAbstractTableModel>

#include "reosactionwidget.h"
#include "reosformwidget.h"
#include "reostransferfunction.h"

class QMenu;

class ReosRunoffModelsGroup;
class ReosRunoffModel;
class ReosRunoff;
class ReosWatershed;
class ReosWatershedModule;
class ReosPlotTimeHistogram;
class ReosMeteorologicModel;
class ReosPlotTimeSerieVariableStep;
class ReosTransferFunction;
class ReosSerieRainfall;

namespace Ui
{
  class ReosRunoffHydrographWidget;
}

class ReosWatershedRunoffModelsModel: public QAbstractTableModel
{
    Q_OBJECT
  public:
    ReosWatershedRunoffModelsModel( QObject *parent = nullptr );

    QModelIndex index( int row, int column, const QModelIndex & ) const override;
    QModelIndex parent( const QModelIndex & ) const override;
    int rowCount( const QModelIndex &parent ) const override;
    int columnCount( const QModelIndex & ) const override;
    QVariant data( const QModelIndex &index, int role ) const override;
    bool setData( const QModelIndex &index, const QVariant &value, int role ) override;
    Qt::ItemFlags flags( const QModelIndex &index ) const override;
    QVariant headerData( int section, Qt::Orientation orientation, int role ) const override;

    void setWatershedRunoffModels( ReosRunoffModelsGroup *watershedRunoffModels );
    void addRunoffModel( ReosRunoffModel *runoffModel );
    void replaceRunoffModel( int row, ReosRunoffModel *runoffModel );
    bool canBeRemoved( int row );
    void removeRunoffModel( int row );

    int runoffCount() const;

  private:
    ReosRunoffModelsGroup *mWatershedRunoffModels = nullptr;

    bool portionEditable() const;
    void allDataChanged();

    bool replacePortion( int position, double portion );
};

class ReosTimeSeriesTabModel: public QAbstractTableModel
{
  public:
    ReosTimeSeriesTabModel( QObject *parent = nullptr );
    QModelIndex index( int row, int column, const QModelIndex & ) const override;
    QModelIndex parent( const QModelIndex & ) const override;
    int rowCount( const QModelIndex & ) const override;
    int columnCount( const QModelIndex & ) const override;
    QVariant data( const QModelIndex &index, int role ) const override;
    QVariant headerData( int section, Qt::Orientation orientation, int role ) const override;

    void addTimeSerie( ReosTimeSerie *timeSerie, const QString &name );

    void clearSerie();

  private:
    QList<QPointer<ReosTimeSerie>> mTimeSerie;
    QStringList mHeaderName;
};

class ReosRunoffHydrographWidget : public ReosActionWidget
{
    Q_OBJECT
  public:
    explicit ReosRunoffHydrographWidget( ReosWatershedModule *watershedModule, QWidget *parent = nullptr );
    ~ReosRunoffHydrographWidget();

    void setCurrentWatershed( ReosWatershed *watershed );

  public slots:
    void setCurrentMeteorologicModel( int index );

  private slots:
    void onModelMeteoChanged();
    void updateRainall();
    void updateRunoff();
    void updateHydrograph();
    void onTransferFunctionChanged();
    void onRunoffTableViewContextMenu( const QPoint &pos );
    void copyHydrographSelected( bool withHeader );
    void copyRainfallRunoffSelected( bool withHeader );
    void hydrographTabContextMenu( const QPoint &pos );
    void rainfallRunoffTabContextMenu( const QPoint &pos );
    void onTransferFunctionFormulation();

  private:
    Ui::ReosRunoffHydrographWidget *ui;
    ReosWatershedModule *mWatershedModule = nullptr;
    ReosWatershedRunoffModelsModel *mWatershedRunoffModelsModel = nullptr;
    ReosWatershed *mCurrentWatershed = nullptr;
    ReosMeteorologicModel *mCurrentMeteoModel = nullptr;
    ReosTimeSeriesTabModel *mRunoffResultTabModel;
    ReosTimeSeriesTabModel *mHydrographResultModel;

    ReosTransferFunction *mCurrentTransferFunction = nullptr; //to remove?
    ReosFormWidget *mCurrentTransferFunctionForm = nullptr;

    ReosPlotTimeHistogram *mRainfallHistogram = nullptr;
    ReosPlotTimeHistogram *mRunoffHistogram = nullptr;
    ReosPlotTimeSerieVariableStep *mHydrographCurve = nullptr;

    ReosRunoff *mCurrentRunoff = nullptr;
    ReosHydrograph *mCurrentHydrograph = nullptr;

    void buildRunoffChoiceMenu( QMenu *menu, int row );
    void syncTransferFunction( ReosTransferFunction *function );
};

//**************************************************

class ReosFormLinearReservoirWidgetFactory: public ReosFormWidgetDataFactory
{
  public:
    virtual ReosFormWidget *createDataWidget( ReosDataObject *dataObject, QWidget *parent );
    virtual QString datatype() const {return QStringLiteral( "transfer-function-linear-reservoir" );}
};

class ReosFormGeneralizedRationalMethodWidgetFactory: public ReosFormWidgetDataFactory
{
  public:
    virtual ReosFormWidget *createDataWidget( ReosDataObject *dataObject, QWidget *parent );
    virtual QString datatype() const {return QStringLiteral( "transfer-function-generalized-rational-method" );}
};

class ReosFormSCSUnithydrographWidgetFactory: public ReosFormWidgetDataFactory
{
  public:
    virtual ReosFormWidget *createDataWidget( ReosDataObject *dataObject, QWidget *parent );
    virtual QString datatype() const {return QStringLiteral( "transfer-function-scs-unit-hydrograph" );}
};

#endif // REOSRUNOFFHYDROGRAPHWIDGET_H
