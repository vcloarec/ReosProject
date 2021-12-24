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
#include "reoshydrographrouting.h"
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

  mProgressControler = new ReosHydrauylicNetworkElementCalculationControler( junctionNode, this );
  mProgressControler->setProgressBar( ui->mProgressBar );

  mHydrographPlotButton = new ReosVariableTimeStepPlotListButton( tr( "Hydrographs" ), ui->mPlotWidget );
  ReosSettings settings;
  if ( settings.contains( settingsString ) )
    mHydrographPlotButton->setChecked( settings.value( settingsString ).toBool() );
  else
    mHydrographPlotButton->setChecked( true );

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
  connect( mJunctionNode, &ReosHydrographJunction::calculationIsUpdated, this, &ReosHydraulicHydrographJunctionPropertiesWidget::updateInformation );

  updateInformation();
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

  mHydrographPlotButton->clear();
  ReosHydrograph *internalHyd = mJunctionNode->internalHydrograph();
  QList<ReosHydrographRoutingLink *> upstreamRoutingList = ReosHydraulicNetworkUtils::upstreamLinkOfType<ReosHydrographRoutingLink> ( mJunctionNode );

  hydrographs.append( mJunctionNode->outputHydrograph() );

  for ( ReosHydrographRoutingLink *routing : std::as_const( upstreamRoutingList ) )
    hydrographs.append( routing->outputHydrograph() );

  if ( hydrographs.count() > 1 && internalHyd )
    hydrographs.append( internalHyd );

  QList<ReosTimeSerieVariableTimeStep *> tsList;

  for ( ReosHydrograph *hyd : std::as_const( hydrographs ) )
  {
    mHydrographPlotButton->addData( hyd );
    tsList.append( hyd );
  }

  ui->mHydrographTabsWidget->setSeries( tsList, QString( "m%1/s" ).arg( QChar( 0x00B3 ) ) );
  ui->mHydrographTabsWidget->setConstantTimeStepParameter( mJunctionNode->constantTimeStepInTable(), mJunctionNode->useConstantTimeStepInTable() );

}

void ReosHydraulicHydrographJunctionPropertiesWidget::updateInformation()
{
  if ( ! mJunctionNode->outputHydrograph() || mJunctionNode->outputHydrograph()->valueCount() == 0 )
  {
    ui->mLabelPeak->setText( tr( "none" ) );
    ui->mLabelValueCount->setText( QLocale().toString( 0 ) );
  }
  else
  {
    ui->mLabelPeak->setText( QStringLiteral( "%1 %2" ).arg( QLocale().toString( mJunctionNode->outputHydrograph()->maximum() ),
                             QString( "m%1/s" ).arg( QChar( 0x00B3 ) ) ) );
    ui->mLabelValueCount->setText( QLocale().toString( mJunctionNode->outputHydrograph()->valueCount() ) );
  }
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
  : ReosFormJunctionNodeWidget( watershedNode, parent )
  , mNode( watershedNode )
{
  QGridLayout *gridLayout = new QGridLayout( this );
  gridLayout->setContentsMargins( 0, 0, 0, 0 );
  gridLayout->setSpacing( 6 );
  addItem( gridLayout, 0 );
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

  addLine( 1 );

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

ReosFormJunctionNodeWidget::ReosFormJunctionNodeWidget( ReosHydrographJunction *junction, QWidget *parent ): ReosFormWidget( parent )
{
  addParameter( junction->useForceOutputTimeStep() );
  ReosParameterWidget *otsw = addParameter( junction->forceOutputTimeStep() );
  otsw->setVisible( junction->useForceOutputTimeStep()->value() );
  connect( junction->useForceOutputTimeStep(), &ReosParameter::valueChanged, otsw, [otsw, junction]
  {
    otsw->setVisible( junction->useForceOutputTimeStep()->value() );
  } );
}

ReosFormWidget *ReosFormJunctionNodeWidgetFactory::createDataWidget( ReosDataObject *dataObject, QWidget *parent )
{
  ReosHydrographJunction *junctionNode = qobject_cast<ReosHydrographJunction *>( dataObject );
  if ( !junctionNode )
    return nullptr;

  return new ReosFormJunctionNodeWidget( junctionNode, parent );
}

QString ReosFormJunctionNodeWidgetFactory::datatype() const
{
  return ReosHydrographJunction::staticType();
}
