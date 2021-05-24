/***************************************************************************
  reosrainfalldataform.h - %{Cpp:License:ClassName}

 ---------------------
 begin                : 25.2.2021
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
#ifndef REOSRAINFALLDATAFORM_H
#define REOSRAINFALLDATAFORM_H

#include "reosformwidget.h"


//! Widget that can be uses to display/edits paramters of a ReosTimeSerieConstantInterval
class ReosTimeSerieConstantIntervalWidget: public ReosFormWidget
{
    Q_OBJECT
  public:
    explicit ReosTimeSerieConstantIntervalWidget( ReosTimeSerieConstantInterval *timeSerie, QWidget *parent );

  private:
    ReosTimeSerieConstantIntervalModel *mModel = nullptr;
    QComboBox *mValueModeComboBox = nullptr;
    QComboBox *mIntensityUnitComboBox = nullptr;
};

//! Table view widget custmed to display/edit time series with constant time step
class ReosTimeSerieConstantIntervalView: public QTableView
{
    Q_OBJECT
  public:
    ReosTimeSerieConstantIntervalView( QWidget *parent = nullptr );

  signals:
    void pastDataFromClipboard( const QModelIndex &index, const QList<double> &data );
    void insertRow( const QModelIndex &fromIndex, int count );
    void deleteRows( const QModelIndex &fromIndex, int count );
    void insertRowFromClipboard( const QModelIndex &index, const QList<double> &data );

  protected:
    void keyPressEvent( QKeyEvent *event ) override;
    void contextMenuEvent( QContextMenuEvent *event ) override;

  private:
    QList<double> clipboardToValues();
};

class ReosFormWidgetIntensityDurationCurveFactory : public ReosFormWidgetDataFactory
{
  public:
    ReosFormWidget *createDataWidget( ReosDataObject *dataObject, QWidget *parent ) override;
    QString datatype() const override {return  QStringLiteral( "rainfall-intensity-duration-curve" );}
};

class ReosFormWidgetTimeSerieConstantIntervalFactory : public ReosFormWidgetDataFactory
{
  public:
    ReosFormWidget *createDataWidget( ReosDataObject *dataObject, QWidget *parent ) override;
    QString datatype() const override {return QStringLiteral( "time-serie-constant-interval" );}
};

class ReosFormWidgetRainFallSerieFactory : public ReosFormWidgetTimeSerieConstantIntervalFactory
{
  public:
    QString datatype() const override {return QStringLiteral( "serie-rainfall" );}
};


//! Widget that can be uses to display/edits paramters of a Chicago rainfall
class ReosChicagoRainfallWidget: public ReosTimeSerieConstantIntervalWidget
{
    Q_OBJECT
  public:
    explicit ReosChicagoRainfallWidget( ReosChicagoRainfall *rainfall, QWidget *parent );

  private:
    ReosIntensityDurationSelectedCurveWidget *mIdfWidget = nullptr;
};

class ReosFormWidgetChicagoRainfalFactory : public ReosFormWidgetDataFactory
{
  public:
    ReosFormWidget *createDataWidget( ReosDataObject *dataObject, QWidget *parent ) override;
    QString datatype() const override {return QStringLiteral( "chicago-rainfall" );}
};

//! Widget that can be uses to display/edits paramters of a Chicago rainfall
class ReosAlternatingBlockRainfallWidget: public ReosTimeSerieConstantIntervalWidget
{
    Q_OBJECT
  public:
    explicit ReosAlternatingBlockRainfallWidget( ReosAlternatingBlockRainfall *rainfall, QWidget *parent );

  private:
    ReosIntensityDurationSelectedCurveWidget *mIdfWidget = nullptr;
};

class ReosFormWidgetAlternatingBlockRainfalFactory : public ReosFormWidgetDataFactory
{
  public:
    ReosFormWidget *createDataWidget( ReosDataObject *dataObject, QWidget *parent ) override;
    QString datatype() const override {return QStringLiteral( "alternating-block-rainfall" );}
};

//! Widget that can be uses to display/edits paramters of a Chicago rainfall
class ReosDoubleTriangleRainfallWidget: public ReosTimeSerieConstantIntervalWidget
{
    Q_OBJECT
  public:
    explicit ReosDoubleTriangleRainfallWidget( ReosDoubleTriangleRainfall *rainfall, QWidget *parent );

  private:
    ReosIntensityDurationSelectedCurveWidget *mIntenseIdfWidget = nullptr;
    ReosIntensityDurationSelectedCurveWidget *mTotalIdfWidget = nullptr;
};

class ReosFormWidgetDoubleTriangleRainfalFactory : public ReosFormWidgetDataFactory
{
  public:
    ReosFormWidget *createDataWidget( ReosDataObject *dataObject, QWidget *parent ) override;
    QString datatype() const override {return QStringLiteral( "double-triangle-rainfall" );}
};



#endif // REOSRAINFALLDATAFORM_H
