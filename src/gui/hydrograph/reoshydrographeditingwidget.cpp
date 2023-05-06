/***************************************************************************
  reoshydrographeditingwidget.cpp - ReosHydrographEditingWidget

 ---------------------
 begin                : 25.10.2021
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
#include "reoshydrographeditingwidget.h"

#include <QHBoxLayout>
#include <QLabel>
#include <QComboBox>

#include "reosparameter.h"
#include "reostimeseries.h"
#include "reoshydrograph.h"
#include "reostableview.h"
#include "reossettings.h"
#include "reosplottimeconstantinterval.h"
#include "reosguicontext.h"


ReosHydrographEditingWidget::ReosHydrographEditingWidget( ReosHydrograph *hydrograph, QWidget *parent )
  : ReosFormWidget( parent, Qt::Vertical, false )
  , mIsUseConstantTimeStepForNewEntry( new ReosParameterBoolean( tr( "Use constant time step for new entry" ), false, this ) )
  , mConstantTimeStepForNewEntry( new ReosParameterDuration( tr( "Constant time step" ) ) )
{
  ReosSettings settings;

  mDataModel = qobject_cast<ReosTimeSeriesVariableTimeStepModel *>( hydrograph->model() );

  mIsUseConstantTimeStepForNewEntry->setValue( false );
  addParameter( hydrograph->referenceTimeParameter() );
  if ( hydrograph->dataProvider()->isEditable() )
  {
    addParameter( mIsUseConstantTimeStepForNewEntry );

    ReosDuration newEntryFixedTimeStep( 5, ReosDuration::minute );
    if ( settings.contains( QStringLiteral( "/hydrograh/new-entry-time-step-value" ) ) &&
         settings.contains( QStringLiteral( "/hydrograh/new-entry-time-step-unit" ) ) )
    {
      double timeStepValue = settings.value( QStringLiteral( "/hydrograh/new-entry-time-step-value" ) ).toDouble();
      ReosDuration::Unit unit = static_cast<ReosDuration::Unit>(
                                  settings.value( QStringLiteral( "/hydrograh/new-entry-time-step-value" ) ).toInt() );

      newEntryFixedTimeStep = ReosDuration( timeStepValue, unit );
    }
    mConstantTimeStepForNewEntry->setValue( newEntryFixedTimeStep );



    if ( settings.contains( QStringLiteral( "/hydrograh/new-entry-use-constant-time-step" ) ) )
    {
      mIsUseConstantTimeStepForNewEntry->setValue( settings.value( QStringLiteral( "/hydrograh/new-entry-use-constant-time-step" ) ).toBool() );
    }

    ReosDuration::Unit timeStepUnit = ReosDuration::minute;
    if ( settings.contains( QStringLiteral( "/hydrograh/time-step-unit" ) ) )
    {
      timeStepUnit = static_cast<ReosDuration::Unit>(
                       settings.value( QStringLiteral( "/hydrograh/time-step-unit" ) ).toInt() );
    }

    QWidget *relativeTimeUnitWidget = new QWidget( this );
    relativeTimeUnitWidget->setLayout( new QHBoxLayout );
    relativeTimeUnitWidget->layout()->setContentsMargins( 0, 0, 0, 0 );
    relativeTimeUnitWidget->layout()->addWidget( new QLabel( tr( "Time step unit" ) ) );
    mTimeStepUnitCombo = new ReosDurationUnitComboBox( this, timeStepUnit );
    relativeTimeUnitWidget->layout()->addWidget( mTimeStepUnitCombo );
    addWidget( relativeTimeUnitWidget );
    mConstantTimeStepForNewEntryWidget = addParameter( mConstantTimeStepForNewEntry );
    mConstantTimeStepForNewEntryWidget->setVisible( mIsUseConstantTimeStepForNewEntry->value() );
    mDataModel->setNewRowWithFixedTimeStep( mIsUseConstantTimeStepForNewEntry->value() );

    connect( mIsUseConstantTimeStepForNewEntry, &ReosParameter::valueChanged, this, [this, relativeTimeUnitWidget]
    {
      bool useConstantTimeStep = mIsUseConstantTimeStepForNewEntry->value();
      ReosSettings settings;
      settings.setValue( QStringLiteral( "/hydrograh/new-entry-use-constant-time-step" ), useConstantTimeStep );
      mConstantTimeStepForNewEntryWidget->setVisible( useConstantTimeStep );
      mDataModel->setNewRowWithFixedTimeStep( useConstantTimeStep );
      relativeTimeUnitWidget->setVisible( !useConstantTimeStep );
    } );

    connect( mConstantTimeStepForNewEntry, &ReosParameter::valueChanged, this, [this]
    {
      mDataModel->setFixedTimeStep( mConstantTimeStepForNewEntry->value() );
      ReosSettings settings;
      settings.value( QStringLiteral( "/hydrograh/new-entry-time-step-value" ), mConstantTimeStepForNewEntry->value().valueUnit() );
      settings.value( QStringLiteral( "/hydrograh/new-entry-time-step-unit" ), mConstantTimeStepForNewEntry->value().unit() );
    } );

    connect( mTimeStepUnitCombo, QOverload<int>::of( &QComboBox::currentIndexChanged ), this, [this]
    {
      ReosDuration::Unit unit = mTimeStepUnitCombo->currentUnit();
      mDataModel->setVariableTimeStepUnit( unit );
      ReosSettings settings;
      settings.value( QStringLiteral( "/hydrograh/time-step-unit" ), unit );
    } );

  }

  ReosTimeSerieTableView *tableView = new ReosTimeSerieTableView( this );
  addWidget( tableView );
  tableView->setModel( mDataModel );
  tableView->verticalHeader()->hide();
}

ReosHydrographEditingWidget::~ReosHydrographEditingWidget()
{
}

QString ReosHydrographEditingWidgetFactory::datatype() const {return ReosHydrograph::staticType();}

ReosFormWidget *ReosHydrographEditingWidgetFactory::createDataWidget( ReosDataObject *dataObject, const ReosGuiContext &context )
{
  ReosHydrograph *hyd = qobject_cast<ReosHydrograph *>( dataObject );
  if ( hyd )
    return new ReosHydrographEditingWidget( hyd, context.parent() );
  else
    return nullptr;
}

QString ReosHydrographPlotFactory::datatype() const {return ReosHydrograph::staticType();}

void ReosHydrographPlotFactory::buildPlotItemsAndSetup( ReosPlotWidget *, ReosDataObject * )  {}

ReosPlotItem *ReosHydrographPlotFactory::buildPlotItem( ReosPlotWidget *plotWidget, ReosDataObject *data )
{
  ReosHydrograph *hyd = qobject_cast<ReosHydrograph *>( data );

  if ( !hyd )
    return nullptr;

  std::unique_ptr<ReosPlotTimeSerieVariableStep> plotItem( new ReosPlotTimeSerieVariableStep( hyd->name() ) );
  plotItem->setTimeSeries( hyd );

  plotWidget->addPlotItem( plotItem.get() );

  return plotItem.release();
}
