/***************************************************************************
  reoshydraulichydrographjunctionpropertieswidget.cpp - ReosHydraulicHydrographJuntionPropertiesWidget

 ---------------------
 begin                : 8.12.2021
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
#include "reoshydraulichydrographjunctionpropertieswidget.h"
#include "ui_reoshydraulichydrographjunctionpropertieswidget.h"

#include <QMenu>
#include <QWidgetAction>
#include <QLayout>
#include <QLabel>
#include <QHBoxLayout>

#include "reoshydrographsource.h"
#include "reoshydrographtransfer.h"
#include "reosplotitemlist.h"
#include "reosplottimeconstantinterval.h"


ReosHydraulicHydrographJunctionPropertiesWidget::ReosHydraulicHydrographJunctionPropertiesWidget( ReosHydrographJunction *junctionNode, QWidget *parent )
  : ReosHydraulicElementWidget( parent )
  , ui( new Ui::ReosHydraulicHydrographJunctionPropertiesWidget )
  , mJunctionNode( junctionNode )
{
  ui->setupUi( this );

  ui->mPlotWidget->setSettingsContext( QStringLiteral( "hyraulic-network-node-watershed" ) );
  ui->mPlotWidget->setTitleAxeX( tr( "Time" ) );
  ui->mPlotWidget->setAxeXType( ReosPlotWidget::temporal );
  ui->mPlotWidget->enableAxeYright( false );
  ui->mPlotWidget->setTitleAxeYLeft( tr( "Flow rate (%1)" ).arg( QString( "m%1/s" ).arg( QChar( 0x00B3 ) ) ) );
  ui->mPlotWidget->setMagnifierType( ReosPlotWidget::positiveMagnifier );

  mOutputCurve = new ReosPlotTimeSerieVariableStep();
  ui->mPlotWidget->addPlotItem( mOutputCurve );

  QList<ReosHydrographRoutineLink *> upstreamRoutinesList = ReosHydraulicNetworkUtils::upstreamLinkOfType<ReosHydrographRoutineLink> ( junctionNode );

  ReosVariableTimeStepPlotListButton *upstreamHydrographButton = new ReosVariableTimeStepPlotListButton( tr( "Upstream Hydrographs" ), ui->mPlotWidget );

  for ( ReosHydrographRoutineLink *routine : std::as_const( upstreamRoutinesList ) )
    upstreamHydrographButton->addData( routine->outputHydrograph() );

  upstreamHydrographButton->setEnabled( true );

  QWidget *nodeWidget = ReosFormWidgetFactories::instance()->createDataFormWidget( junctionNode );
  if ( nodeWidget )
    ui->mGroupBoxParameters->layout()->addWidget( nodeWidget );
}

ReosHydraulicHydrographJunctionPropertiesWidget::~ReosHydraulicHydrographJunctionPropertiesWidget()
{
  delete ui;
}

void ReosHydraulicHydrographJunctionPropertiesWidget::setCurrentCalculationContext( const ReosCalculationContext &calculationContext )
{
  mJunctionNode->updateCalculationContext( calculationContext );
  mOutputCurve->setTimeSerie( mJunctionNode->outputHydrograph() );
}

ReosHydraulicHydrographNodePropertiesWidgetFactory::ReosHydraulicHydrographNodePropertiesWidgetFactory( QObject *parent ): ReosHydraulicElementWidgetFactory( parent ) {}

ReosHydraulicElementWidget *ReosHydraulicHydrographNodePropertiesWidgetFactory::createWidget( ReosHydraulicNetworkElement *element, QWidget *parent )
{
  ReosHydrographJunction *junctionNode = qobject_cast<ReosHydrographJunction *>( element );
  return new ReosHydraulicHydrographJunctionPropertiesWidget( junctionNode, parent );
}

QString ReosHydraulicHydrographNodePropertiesWidgetFactory::elementType()
{
  return ReosHydrographNode::staticType();
}

ReosFormWidget *ReosFormWatershedNodeWidgetFactory::createDataWidget( ReosDataObject *dataObject, QWidget *parent )
{
  ReosHydrographNodeWatershed *watershedNode = qobject_cast<ReosHydrographNodeWatershed *>( dataObject );
  if ( !watershedNode )
    return nullptr;

  return new ReosFormWatershedNodeWidget( watershedNode, parent );
}

QString ReosFormWatershedNodeWidgetFactory::datatype() const
{
  return ReosHydrographNodeWatershed::staticType();
}

ReosFormWatershedNodeWidget::ReosFormWatershedNodeWidget( ReosHydrographNodeWatershed *watershedNode, QWidget *parent )
  : ReosFormWidget( parent )
  , mNode( watershedNode )
{

  QGridLayout *gridLayout = new QGridLayout( this );
  gridLayout->setContentsMargins( 0, 0, 0, 0 );
  gridLayout->setSpacing( 6 );
  addItem( gridLayout );
  gridLayout->addWidget( new QLabel( tr( "Hydrograph origin" ), this ), 0, 0 );
  mOriginCombo = new QComboBox( this );
  gridLayout->addWidget( mOriginCombo, 0, 1 );
  mOriginCombo->addItem( QObject::tr( "Runoff hydrograph calculation" ), ReosHydrographNodeWatershed::RunoffHydrograph );
  mOriginCombo->addItem( QObject::tr( "Gauged hydrograph" ), ReosHydrographNodeWatershed::GaugedHydrograph );

  mGaugedLabel = new QLabel( QObject::tr( "Gauged Hydrograph" ), this );
  gridLayout->addWidget( mGaugedLabel, 1, 0 );

  mGaugedHydrographCombo = new QComboBox( this );
  gridLayout->addWidget( mGaugedHydrographCombo, 1, 1 );

  mOriginCombo->setCurrentIndex( mOriginCombo->findData( watershedNode->origin() ) );

  originChange();

  connect( mGaugedHydrographCombo, QOverload<int>::of( &QComboBox::currentIndexChanged ), mNode, &ReosHydrographNodeWatershed::setGaugedHydrographIndex );
  connect( mOriginCombo, QOverload<int>::of( &QComboBox::currentIndexChanged ), this,  &ReosFormWatershedNodeWidget::originChange );


}

void ReosFormWatershedNodeWidget::updateGaugedHydrograph()
{
  switch ( mNode->origin() )
  {
    case ReosHydrographNodeWatershed::RunoffHydrograph:
      mGaugedHydrographCombo->setVisible( false );
      mGaugedLabel->setVisible( false );
      break;
    case ReosHydrographNodeWatershed::GaugedHydrograph:
      mGaugedHydrographCombo->clear();
      const QStringList hydrographNames = mNode->watershed()->gaugedHydrographs()->hydrographNames();
      for ( const QString &name : hydrographNames )
        mGaugedHydrographCombo->addItem( name );

      if ( mNode->gaugedHydrographIndex() < mGaugedHydrographCombo->count() )
        mGaugedHydrographCombo->setCurrentIndex( mNode->gaugedHydrographIndex() );

      mGaugedHydrographCombo->setVisible( true );
      mGaugedLabel->setVisible( true );
      break;

  }


}

void ReosFormWatershedNodeWidget::originChange()
{
  ReosHydrographNodeWatershed::HydrographOrigin origin =
    static_cast<ReosHydrographNodeWatershed::HydrographOrigin>( mOriginCombo->currentData().toInt() );

  mNode->setOrigin( origin );
  updateGaugedHydrograph();
}
