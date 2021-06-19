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
#include "reosmuskingumclassicrouting.h"
#include "reosplottimeconstantinterval.h"
#include "reoshydrograph.h"

ReosHydrographRoutingPropertiesWidget::ReosHydrographRoutingPropertiesWidget( ReosHydrographRouting *hydrographRouting, QWidget *parent ) :
  QWidget( parent ),
  ui( new Ui::ReosHydrographRoutingPropertiesWidget )
{
  ui->setupUi( this );

  ui->mNameWidget->setLayout( new QHBoxLayout );
  ui->mNameWidget->layout()->addWidget( new ReosParameterStringWidget( hydrographRouting->name(), this ) );

  mInputHydrographCurve = new ReosPlotTimeSerieVariableStep( tr( "Input hydrograph" ) );
  mInputHydrographCurve->setTimeSerie( hydrographRouting->inputHydrographSource()->outputHydrograph() );
  mOutputtHydrographCurve = new ReosPlotTimeSerieVariableStep( tr( "Output hydrograph" ) );
  mOutputtHydrographCurve->setTimeSerie( hydrographRouting->outputHydrograph() );

  ui->mPlotsWidget->addPlotItem( mInputHydrographCurve );
  ui->mPlotsWidget->addPlotItem( mOutputtHydrographCurve );

  ui->mPlotsWidget->setTitleAxeX( tr( "Time" ) );
  ui->mPlotsWidget->setAxeXType( ReosPlotWidget::temporal );
  ui->mPlotsWidget->enableAxeYright( true );
  ui->mPlotsWidget->setTitleAxeYLeft( tr( "Flow rate (%1)" ).arg( QString( "m%1/s" ).arg( QChar( 0x00B3 ) ) ) );
  ui->mPlotsWidget->setMagnifierType( ReosPlotWidget::positiveMagnifier );

  hydrographRouting->setCurrentRoutingMethod( ReosMuskingumClassicRouting::typeString() );
  QWidget *routingWidget = ReosFormWidgetFactories::instance()->createDataFormWidget( hydrographRouting->currentRoutingMethod() );
  if ( routingWidget )
    ui->groupBoxParameters->layout()->addWidget( routingWidget );
}

ReosHydrographRoutingPropertiesWidget::~ReosHydrographRoutingPropertiesWidget()
{
  delete ui;
}

QWidget *ReosHydrographRoutingPropertiesWidgetFactory::createWidget( ReosHydraulicNetworkElement *element, QWidget *parent )
{
  if ( !element )
    return nullptr;

  if ( !element->type().contains( ReosHydrographRouting::typeString() ) )
    return nullptr;

  ReosHydrographRouting *routing = qobject_cast<ReosHydrographRouting *>( element );

  if ( !routing )
    return nullptr;

  return new ReosHydrographRoutingPropertiesWidget( routing, parent );
}

QString ReosHydrographRoutingPropertiesWidgetFactory::elementType() {return QString();}

ReosFormWidget *ReosFormMuskingumClassicRoutingWidgetFactory::createDataWidget( ReosDataObject *dataObject, QWidget *parent )
{
  ReosMuskingumClassicRouting *routing = qobject_cast<ReosMuskingumClassicRouting *>( dataObject );
  if ( !routing )
    return nullptr;

  ReosFormWidget *form = new ReosFormWidget( parent );
  form->addParameter( routing->kParameter() );
  form->addParameter( routing->xParameter() );

  return form;
}

QString ReosFormMuskingumClassicRoutingWidgetFactory::datatype() const {return ReosMuskingumClassicRouting::typeString();}
