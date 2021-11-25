/***************************************************************************
  reosrainfalldataform.cpp - %{Cpp:License:ClassName}

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

#include <QApplication>
#include <QKeyEvent>
#include <QHeaderView>
#include <QHBoxLayout>
#include <QLabel>
#include <QMenu>
#include <QMessageBox>
#include <QClipboard>

#include "reosrainfalldataform.h"
#include "reostimeserie.h"
#include "reosidfcurves.h"
#include "reossyntheticrainfall.h"
#include "reosintensitydurationselectedcurvewidget.h"
#include "reosrainfallintensitydurationwidget.h"
#include "reosrainfallregistery.h"


ReosTimeSerieConstantIntervalWidget::ReosTimeSerieConstantIntervalWidget( ReosTimeSerieConstantInterval *timeSerie, QWidget *parent ):
  ReosFormWidget( parent, Qt::Vertical, false )
  , mModel( new ReosTimeSerieConstantIntervalModel( this ) )
{
  mModel->setSerieData( timeSerie );
  addParameter( timeSerie->timeStep() );
  addParameter( timeSerie->referenceTimeParameter() );

  mValueModeComboBox = new QComboBox( this );
  mValueModeComboBox->addItem( timeSerie->valueModeName( ReosTimeSerieConstantInterval::Value ), ReosTimeSerieConstantInterval::Value );
  mValueModeComboBox->addItem( timeSerie->valueModeName( ReosTimeSerieConstantInterval::Intensity ), ReosTimeSerieConstantInterval::Intensity );
  mValueModeComboBox->setCurrentIndex( mValueModeComboBox->findData( timeSerie->valueMode() ) );
  mIntensityUnitComboBox = new QComboBox( this );

  mIntensityUnitComboBox->addItem( tr( "millisecond" ), ReosDuration::millisecond );
  mIntensityUnitComboBox->addItem( tr( "second" ), ReosDuration::second );
  mIntensityUnitComboBox->addItem( tr( "minute" ), ReosDuration::minute );
  mIntensityUnitComboBox->addItem( tr( "hour" ), ReosDuration::hour );
  mIntensityUnitComboBox->addItem( tr( "day" ), ReosDuration::day );
  mIntensityUnitComboBox->addItem( tr( "week" ), ReosDuration::week );
  mIntensityUnitComboBox->addItem( tr( "month" ), ReosDuration::month );
  mIntensityUnitComboBox->addItem( tr( "year" ), ReosDuration::year );
  mIntensityUnitComboBox->setCurrentIndex( mIntensityUnitComboBox->findData( timeSerie->intensityTimeUnit() ) );
  mIntensityUnitComboBox->setEnabled( timeSerie->valueMode() == ReosTimeSerieConstantInterval::Intensity );

  QHBoxLayout *layoutIntUnit = new QHBoxLayout( this );
  layoutIntUnit->addWidget( new QLabel( tr( "Intensity time unit" ), this ) );
  layoutIntUnit->addWidget( mIntensityUnitComboBox );
  addItem( layoutIntUnit );
  QHBoxLayout *layoutMode = new QHBoxLayout( this );
  layoutMode->addWidget( new QLabel( tr( "Value type" ), this ) );
  layoutMode->addWidget( mValueModeComboBox );
  addItem( layoutMode );

  connect( mValueModeComboBox, QOverload<int>::of( &QComboBox::currentIndexChanged ), timeSerie, [timeSerie, this]()
  {
    ReosTimeSerieConstantInterval::ValueMode mode = static_cast<ReosTimeSerieConstantInterval::ValueMode>( this->mValueModeComboBox->currentData().toInt() );
    timeSerie->setValueMode( mode );
    mIntensityUnitComboBox->setEnabled( mode == ReosTimeSerieConstantInterval::Intensity );
  } );

  connect( mIntensityUnitComboBox, QOverload<int>::of( &QComboBox::currentIndexChanged ), timeSerie, [timeSerie, this]()
  {
    ReosDuration::Unit unit = static_cast<ReosDuration::Unit>( this->mIntensityUnitComboBox->currentData().toInt() );
    timeSerie->setIntensityTimeUnit( unit );
  } );

  ReosTimeSerieTableView *view = new ReosTimeSerieTableView( this );
  addWidget( view );
  setStretch( count() - 1, 1 );
  view->setModel( mModel );

  setSizePolicy( QSizePolicy::MinimumExpanding, QSizePolicy::MinimumExpanding );

}


ReosChicagoRainfallWidget::ReosChicagoRainfallWidget( ReosChicagoRainfall *rainfall, QWidget *parent ):
  ReosTimeSerieConstantIntervalWidget( rainfall, parent ),
  mIdfWidget( new ReosIntensityDurationSelectedCurveWidget( this ) )
{
  addParameter( rainfall->totalDuration(), 1 );

  if ( ReosRainfallRegistery::isInstantiate() )
  {
    ReosRainfallIntensityDurationCurveItem *curveItem =
      qobject_cast<ReosRainfallIntensityDurationCurveItem *>( ReosRainfallRegistery::instance()->itemByUniqueId( rainfall->intensityDurationUid() ) );
    if ( curveItem )
      mIdfWidget->setCurveItem( curveItem );
    else
      mIdfWidget->clearCurveItem();
  }

  connect( mIdfWidget, &ReosIntensityDurationSelectedCurveWidget::curveChanged, rainfall, [rainfall, this]
  {
    if ( mIdfWidget->curveItem() )
      rainfall->setIntensityDurationCurve( this->mIdfWidget->curveItem()->data(), this->mIdfWidget->curveItem()->uniqueId() );
  } );

  addWidget( mIdfWidget, 3 );
  addParameter( rainfall->centerCoefficient() );
}

ReosDoubleTriangleRainfallWidget::ReosDoubleTriangleRainfallWidget( ReosDoubleTriangleRainfall *rainfall, QWidget *parent ):
  ReosTimeSerieConstantIntervalWidget( rainfall, parent ),
  mIntenseIdfWidget( new ReosIntensityDurationSelectedCurveWidget( this ) ),
  mTotalIdfWidget( new ReosIntensityDurationSelectedCurveWidget( this ) )
{
  addParameter( rainfall->intenseDuration(), 1 );
  addParameter( rainfall->totalDuration(), 2 );

  mIntenseIdfWidget->setTitle( tr( "Intense Intensity Duration Curve" ) );
  mTotalIdfWidget->setTitle( tr( "Total Intensity Duration Curve" ) );

  if ( ReosRainfallRegistery::isInstantiate() )
  {
    ReosRainfallIntensityDurationCurveItem *intenseCurveItem =
      qobject_cast<ReosRainfallIntensityDurationCurveItem *>( ReosRainfallRegistery::instance()->itemByUniqueId( rainfall->intensityDurationUniqueIdIntense() ) );
    if ( intenseCurveItem )
      mIntenseIdfWidget->setCurveItem( intenseCurveItem );
    else
      mIntenseIdfWidget->clearCurveItem();

    ReosRainfallIntensityDurationCurveItem *totalCurveItem =
      qobject_cast<ReosRainfallIntensityDurationCurveItem *>( ReosRainfallRegistery::instance()->itemByUniqueId( rainfall->intensityDurationUniqueIdTotal() ) );
    if ( totalCurveItem )
      mTotalIdfWidget->setCurveItem( totalCurveItem );
    else
      mTotalIdfWidget->clearCurveItem();
  }

  connect( mIntenseIdfWidget, &ReosIntensityDurationSelectedCurveWidget::curveChanged, rainfall, [rainfall, this]
  {
    if ( this->mIntenseIdfWidget->curveItem() && this->mTotalIdfWidget->curveItem() )
      rainfall->setIntensityDurationCurve( this->mIntenseIdfWidget->curveItem()->data(),
                                           this->mTotalIdfWidget->curveItem()->data(),
                                           this->mIntenseIdfWidget->curveItem()->uniqueId(),
                                           this->mTotalIdfWidget->curveItem()->uniqueId() );
  } );

  connect( mTotalIdfWidget, &ReosIntensityDurationSelectedCurveWidget::curveChanged, rainfall, [rainfall, this]
  {
    if ( this->mIntenseIdfWidget->curveItem() && this->mTotalIdfWidget->curveItem() )
      rainfall->setIntensityDurationCurve( this->mIntenseIdfWidget->curveItem()->data(),
                                           this->mTotalIdfWidget->curveItem()->data(),
                                           this->mIntenseIdfWidget->curveItem()->uniqueId(),
                                           this->mTotalIdfWidget->curveItem()->uniqueId() );
  } );

  addWidget( mIntenseIdfWidget, 4 );
  addWidget( mTotalIdfWidget, 4 );
  addParameter( rainfall->centerCoefficient() );
}


ReosFormWidget *ReosFormWidgetTimeSerieConstantIntervalFactory::createDataWidget( ReosDataObject *dataObject, QWidget *parent )
{
  ReosTimeSerieConstantInterval *object = qobject_cast<ReosTimeSerieConstantInterval *>( dataObject );
  if ( object )
    return new ReosTimeSerieConstantIntervalWidget( object, parent );

  return nullptr;
}

QString ReosFormWidgetTimeSerieConstantIntervalFactory::datatype() const {return ReosTimeSerieConstantInterval::staticType();}


ReosFormWidget *ReosFormWidgetChicagoRainfalFactory::createDataWidget( ReosDataObject *dataObject, QWidget *parent )
{
  ReosChicagoRainfall *object = qobject_cast<ReosChicagoRainfall *>( dataObject );
  if ( object )
    return new ReosChicagoRainfallWidget( object, parent );

  return nullptr;
}

QString ReosFormWidgetChicagoRainfalFactory::datatype() const {return ReosChicagoRainfall::staticType();}

ReosFormWidget *ReosFormWidgetDoubleTriangleRainfalFactory::createDataWidget( ReosDataObject *dataObject, QWidget *parent )
{
  ReosDoubleTriangleRainfall *object = qobject_cast<ReosDoubleTriangleRainfall *>( dataObject );
  if ( object )
    return new ReosDoubleTriangleRainfallWidget( object, parent );

  return nullptr;
}

QString ReosFormWidgetDoubleTriangleRainfalFactory::datatype() const {return ReosDoubleTriangleRainfall::staticType();}

ReosFormWidget *ReosFormWidgetIntensityDurationCurveFactory::createDataWidget( ReosDataObject *dataObject, QWidget *parent )
{
  ReosIntensityDurationCurve *object = qobject_cast<ReosIntensityDurationCurve *>( dataObject );
  if ( object )
    return new ReosRainfallIntensityDurationWidget( object, parent );

  return nullptr;
}

QString ReosFormWidgetIntensityDurationCurveFactory::datatype() const {return  ReosIntensityDurationCurve::staticType();}


ReosFormWidget *ReosFormWidgetAlternatingBlockRainfalFactory::createDataWidget( ReosDataObject *dataObject, QWidget *parent )
{
  ReosAlternatingBlockRainfall *object = qobject_cast<ReosAlternatingBlockRainfall *>( dataObject );
  if ( object )
    return new ReosAlternatingBlockRainfallWidget( object, parent );

  return nullptr;
}

QString ReosFormWidgetAlternatingBlockRainfalFactory::datatype() const {return ReosAlternatingBlockRainfall::staticType();}

ReosAlternatingBlockRainfallWidget::ReosAlternatingBlockRainfallWidget( ReosAlternatingBlockRainfall *rainfall, QWidget *parent ):
  ReosTimeSerieConstantIntervalWidget( rainfall, parent ),
  mIdfWidget( new ReosIntensityDurationSelectedCurveWidget( this ) )
{
  addParameter( rainfall->totalDuration(), 1 );

  if ( ReosRainfallRegistery::isInstantiate() )
  {
    ReosRainfallIntensityDurationCurveItem *curveItem =
      qobject_cast<ReosRainfallIntensityDurationCurveItem *>( ReosRainfallRegistery::instance()->itemByUniqueId( rainfall->intensityDurationUid() ) );
    if ( curveItem )
      mIdfWidget->setCurveItem( curveItem );
    else
      mIdfWidget->clearCurveItem();
  }

  connect( mIdfWidget, &ReosIntensityDurationSelectedCurveWidget::curveChanged, rainfall, [rainfall, this]
  {
    if ( mIdfWidget->curveItem() )
      rainfall->setIntensityDurationCurve( this->mIdfWidget->curveItem()->data(), this->mIdfWidget->curveItem()->uniqueId() );
  } );

  addWidget( mIdfWidget, 3 );
  addParameter( rainfall->centerCoefficient() );
}
