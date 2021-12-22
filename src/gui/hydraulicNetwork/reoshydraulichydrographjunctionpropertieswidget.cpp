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
#include "reostimeseriesvariabletimestepreadonlymodel.h"


ReosHydraulicHydrographJunctionPropertiesWidget::ReosHydraulicHydrographJunctionPropertiesWidget( ReosHydrographJunction *junctionNode, QWidget *parent )
  : ReosHydraulicElementWidget( parent )
  , ui( new Ui::ReosHydraulicHydrographJunctionPropertiesWidget )
  , mJunctionNode( junctionNode )
{
  ui->setupUi( this );

  QString settingsString = QStringLiteral( "hydraulic-network-node-watershed" );
  ui->mPlotWidget->setSettingsContext( settingsString );
  ui->mPlotWidget->setTitleAxeX( tr( "Time" ) );
  ui->mPlotWidget->setAxeXType( ReosPlotWidget::temporal );
  ui->mPlotWidget->enableAxeYright( false );
  ui->mPlotWidget->setTitleAxeYLeft( tr( "Flow rate (%1)" ).arg( QString( "m%1/s" ).arg( QChar( 0x00B3 ) ) ) );
  ui->mPlotWidget->setMagnifierType( ReosPlotWidget::positiveMagnifier );

  mOutputCurve = new ReosPlotTimeSerieVariableStep();
  ui->mPlotWidget->addPlotItem( mOutputCurve );
  mOutputCurve->setTimeSerie( mJunctionNode->outputHydrograph() );

  mInputHydrographButton = new ReosVariableTimeStepPlotListButton( tr( "Upstream Hydrographs" ), ui->mPlotWidget );
  ReosSettings settings;
  if ( settings.contains( settingsString ) )
    mInputHydrographButton->setChecked( settings.value( settingsString ).toBool() );
  else
    mInputHydrographButton->setChecked( true );

  QWidget *nodeWidget = ReosFormWidgetFactories::instance()->createDataFormWidget( junctionNode );
  if ( nodeWidget )
    ui->mGroupBoxParameters->layout()->addWidget( nodeWidget );

  populateHydrographs();

  if ( settings.contains( QStringLiteral( "hydraulic-network-properties-widget/table-visible" ) ) )
  {
    if ( settings.value( QStringLiteral( "hydraulic-network-properties-widget/table-visible" ) ).toBool() )
      ui->mTabResult->setCurrentIndex( 1 );
    else
      ui->mTabResult->setCurrentIndex( 0 );
  }

  connect( ui->mTabResult, &QTabWidget::currentChanged, this, [this]
  {
    ReosSettings settings;
    settings.setValue( QStringLiteral( "hydraulic-network-properties-widget/table-visible" ), ui->mTabResult->currentIndex() == 1 );
  } );

  connect( mJunctionNode, &ReosHydrographJunction::internalHydrographPointerChange, this, &ReosHydraulicHydrographJunctionPropertiesWidget::populateHydrographs );
}

ReosHydraulicHydrographJunctionPropertiesWidget::~ReosHydraulicHydrographJunctionPropertiesWidget()
{
  delete ui;
}

void ReosHydraulicHydrographJunctionPropertiesWidget::setCurrentCalculationContext( const ReosCalculationContext &calculationContext )
{
  mJunctionNode->updateCalculationContext( calculationContext );
}

void ReosHydraulicHydrographJunctionPropertiesWidget::populateHydrographs()
{
  ui->mHydrographTabsWidget->clearSeries();

  QList<QPointer<ReosHydrograph>> hydrographs;

  mInputHydrographButton->clear();
  ReosHydrograph *internalHyd = mJunctionNode->internalHydrograph();
  QList<ReosHydrographRoutingLink *> upstreamRoutinesList = ReosHydraulicNetworkUtils::upstreamLinkOfType<ReosHydrographRoutingLink> ( mJunctionNode );

  for ( ReosHydrographRoutingLink *routine : std::as_const( upstreamRoutinesList ) )
  {
    mInputHydrographButton->addData( routine->outputHydrograph() );
    hydrographs.append( routine->outputHydrograph() );
  }

  if ( internalHyd )
  {
    hydrographs.append( internalHyd );
    mInputHydrographButton->addData( internalHyd );
  }

  if ( hydrographs.count() == 1 )
  {
    mOutputCurve->setVisible( false );
    mOutputCurve->setLegendActive( false );
  }
  else
    hydrographs.prepend( mJunctionNode->outputHydrograph() );

  mInputHydrographButton->setEnabled( hydrographs.count() > 1 );

  QList<ReosTimeSerieVariableTimeStep *> tsList;

  for ( ReosHydrograph *hyd : std::as_const( hydrographs ) )
    tsList.append( hyd );

  ui->mHydrographTabsWidget->setSeries( tsList, QString( "m%1/s" ).arg( QChar( 0x00B3 ) ) );
  ui->mHydrographTabsWidget->setConstantTimeStepParameter( mJunctionNode->constantTimeStepInTable(), mJunctionNode->useConstantTimeStepInTable() );

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
