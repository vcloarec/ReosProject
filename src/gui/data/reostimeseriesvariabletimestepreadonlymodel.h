/***************************************************************************
  reostimeseriesvariabletimestepreadonlymodel.h - ReosTimeSeriesVariableTimeStepReadOnlyModel

 ---------------------
 begin                : 21.12.2021
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
#ifndef REOSTIMESERIESVARIABLETIMESTEPREADONLYMODEL_H
#define REOSTIMESERIESVARIABLETIMESTEPREADONLYMODEL_H

#include <QAbstractTableModel>
#include <QTableView>
#include <QDateTime>

#include "reosduration.h"

class ReosTimeSerieVariableTimeStep;
class QHBoxLayout;
class QCheckBox;
class ReosParameterDurationWidget;
class ReosParameterDuration;
class ReosParameterBoolean;


class ReosTimeSeriesVariableTimeStepReadOnlyModel: public QAbstractTableModel
{
    Q_OBJECT
  public:
    ReosTimeSeriesVariableTimeStepReadOnlyModel( QObject *parent = nullptr ): QAbstractTableModel( parent ) {}
    QModelIndex index( int row, int column, const QModelIndex & ) const override;
    QModelIndex parent( const QModelIndex & ) const override;
    int rowCount( const QModelIndex & ) const override;
    int columnCount( const QModelIndex & ) const override;
    QVariant data( const QModelIndex &index, int role ) const override;
    QVariant headerData( int section, Qt::Orientation orientation, int role ) const override;

    void addTimeSerie( ReosTimeSerieVariableTimeStep *timeSerie, const QString &name );

    void clearSerie();

    bool isFixedTimeStep() const;
    void setIsFixedTimeStep( bool isFixedTimeStep );

    ReosDuration timeStep() const;
    void setTimeStep( const ReosDuration &timeStep );

    void copyResultHydrographValues( QItemSelectionModel *selectionModel, bool withHeader );

  signals:
    void hasBeenReset();

  private slots:
    void updateTimeStep();

  private:
    QList<QPointer<ReosTimeSerieVariableTimeStep>> mTimeSeries;
    QStringList mHeaderName;
    bool mIsFixedTimeStep = false;
    QDateTime mFirstTime;
    int mFirstSerieIndex = -1;
    int mLastSerieIndex = -1;
    ReosDuration mTimeStep = ReosDuration( 70, ReosDuration::second );
    int mTimeStepCount = 0;

    QDateTime timeAtRow( int row ) const;
    QVariant valueAt( int row, int column ) const;
};


class ReosTimeSeriesVariableTimeStepReadOnlyTableView : public QTableView
{
    Q_OBJECT
  public:
    ReosTimeSeriesVariableTimeStepReadOnlyTableView( QWidget *parent = nullptr );

    void setTableModel( ReosTimeSeriesVariableTimeStepReadOnlyModel *model );

  private slots:
    void onContextMenu( const QPoint &pos );

  private:
    ReosTimeSeriesVariableTimeStepReadOnlyModel *mModel = nullptr;

    void copySelected( bool withHeader );
};


class ReosTimeSeriesVariableTimeStepReadOnlyTablesView : public QWidget
{
  public:
    ReosTimeSeriesVariableTimeStepReadOnlyTablesView( QWidget *parent = nullptr );

    void setConstantTimeStepParameter( ReosParameterDuration *constantTimeSepParamater,
                                       ReosParameterBoolean *useConstantTimeStepParameter );

    void setSeries( QList<ReosTimeSerieVariableTimeStep *> series, const QString &unit );
    void clearSeries();

  private:
    QHBoxLayout *mTableLayout = nullptr;
    ReosTimeSeriesVariableTimeStepReadOnlyTableView *mView = nullptr;
    ReosTimeSeriesVariableTimeStepReadOnlyModel *mModel = nullptr;
    QHBoxLayout *mTimeStepLayout = nullptr;

    QCheckBox *mConstantTimeSteCheckBox = nullptr;
    ReosParameterDurationWidget *mTimeStepWidget = nullptr;
};

#endif // REOSTIMESERIESVARIABLETIMESTEPREADONLYMODEL_H
