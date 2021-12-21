/***************************************************************************
  reoshydrographpropertieswidget.cpp - ReosHydrographPropertiesWidget

 ---------------------
 begin                : 28.5.2021
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
#include "reoshydrographroutingpropertieswidget.h"
#include "ui_reoshydrographroutingpropertieswidget.h"

#include "reoshydrographtransfer.h"
#include "reosformwidget.h"
#include "reosmuskingumclassicroutine.h"
#include "reosplottimeconstantinterval.h"
#include "reoshydrograph.h"

ReosHydrographRoutinePropertiesWidget::ReosHydrographRoutinePropertiesWidget( ReosHydrographRoutineLink *hydrographRoutine, QWidget *parent )
  :  ReosHydraulicElementWidget( parent )
  ,  ui( new Ui::ReosHydrographRoutingPropertiesWidget )
  , mRoutine( hydrographRoutine )
{
  ui->setupUi( this );

  mInputHydrographCurve = new ReosPlotTimeSerieVariableStep( tr( "Input hydrograph" ) );
  mInputHydrographCurve->setTimeSerie( hydrographRoutine->inputHydrograph() );
  mOutputtHydrographCurve = new ReosPlotTimeSerieVariableStep( tr( "Output hydrograph" ) );
  mOutputtHydrographCurve->setTimeSerie( hydrographRoutine->outputHydrograph() );

  ui->mPlotsWidget->addPlotItem( mInputHydrographCurve );
  ui->mPlotsWidget->addPlotItem( mOutputtHydrographCurve );

  ui->mPlotsWidget->setTitleAxeX( tr( "Time" ) );
  ui->mPlotsWidget->setAxeXType( ReosPlotWidget::temporal );
  ui->mPlotsWidget->enableAxeYright( true );
  ui->mPlotsWidget->setTitleAxeYLeft( tr( "Flow rate (%1)" ).arg( QString( "m%1/s" ).arg( QChar( 0x00B3 ) ) ) );
  ui->mPlotsWidget->setMagnifierType( ReosPlotWidget::positiveMagnifier );

  hydrographRoutine->setCurrentRoutingMethod( ReosMuskingumClassicRoutine::staticType() );

  QWidget *routingWidget = ReosFormWidgetFactories::instance()->createDataFormWidget( hydrographRoutine->currentRoutingMethod() );
  if ( routingWidget )
    ui->groupBoxParameters->layout()->addWidget( routingWidget );

//  connect( hydrographRoutine, &ReosHydrographRoutineLink::isSetObsolete, this, [this]
//  {
//    ReosCalculationContext context;
//    mRoutine->updateCalculationContext( context );
//  } );
}

ReosHydrographRoutinePropertiesWidget::~ReosHydrographRoutinePropertiesWidget()
{
  delete ui;
}

ReosHydraulicElementWidget *ReosHydrographRoutingPropertiesWidgetFactory::createWidget( ReosHydraulicNetworkElement *element, QWidget *parent )
{
  if ( !element )
    return nullptr;

  if ( !element->type().contains( ReosHydrographRoutineLink::staticType() ) )
    return nullptr;

  ReosHydrographRoutineLink *routing = qobject_cast<ReosHydrographRoutineLink *>( element );

  if ( !routing )
    return nullptr;

  return new ReosHydrographRoutinePropertiesWidget( routing, parent );
}

QString ReosHydrographRoutingPropertiesWidgetFactory::elementType() {return ReosHydrographRoutineLink::staticType();}

ReosFormWidget *ReosFormMuskingumClassicRoutingWidgetFactory::createDataWidget( ReosDataObject *dataObject, QWidget *parent )
{
  ReosMuskingumClassicRoutine *routing = qobject_cast<ReosMuskingumClassicRoutine *>( dataObject );
  if ( !routing )
    return nullptr;

  ReosFormWidget *form = new ReosFormWidget( parent );
  form->addParameter( routing->kParameter() );
  form->addParameter( routing->xParameter() );

  return form;
}

QString ReosFormMuskingumClassicRoutingWidgetFactory::datatype() const {return ReosMuskingumClassicRoutine::staticType();}
