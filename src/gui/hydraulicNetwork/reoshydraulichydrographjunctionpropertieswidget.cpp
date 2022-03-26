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
#include <QCheckBox>

#include "reoshydrographsource.h"
#include "reoshydrographrouting.h"
#include "reosplotitemlist.h"
#include "reosplottimeconstantinterval.h"
#include "reostimeseriesvariabletimestepreadonlymodel.h"
#include "reosgaugedhydrographwidget.h"


ReosHydraulicHydrographJunctionPropertiesWidget::ReosHydraulicHydrographJunctionPropertiesWidget( ReosHydrographJunction *junctionNode, const ReosGuiContext &context )
  : ReosHydraulicElementWidget( context.parent() )
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

  ReosSettings settings;
  mHydrographPlotButton = new ReosVariableTimeStepPlotListButton( tr( "Calc. Hydrographs" ), ui->mPlotWidget );
  if ( settings.contains( settingsString ) )
    mHydrographPlotButton->setChecked( settings.value( settingsString + QStringLiteral( "/hydrograph-button-checked" ) ).toBool() );
  else
    mHydrographPlotButton->setChecked( true );

  mGaugedHydrographPlotButton = new ReosVariableTimeStepPlotListButton( tr( "Gauged Hydrographs" ), ui->mPlotWidget );
  if ( settings.contains( settingsString ) )
    mGaugedHydrographPlotButton->setChecked( settings.value( settingsString + QStringLiteral( "/gauged-button-checked" ) ).toBool() );
  else
    mGaugedHydrographPlotButton->setChecked( false );


  ReosFormWidget *nodeFormWidget = ReosFormWidgetFactories::instance()->createDataFormWidget( junctionNode, context );
  if ( nodeFormWidget )
  {
    ui->mGroupBoxParameters->layout()->addWidget( nodeFormWidget );
    connect( nodeFormWidget, &ReosFormWidget::stackedPageWidgetOpened, this, &ReosHydraulicElementWidget::stackedPageWidgetOpened );
  }

  ui->mHydrographTabsWidget->setConstantTimeStepParameter( mJunctionNode->constantTimeStepInTable(), mJunctionNode->useConstantTimeStepInTable() );

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

  connect( mJunctionNode->outputHydrograph(), &ReosDataObject::dataChanged, this, &ReosHydraulicHydrographJunctionPropertiesWidget::updateGaugedHydrograph );

  populateHydrographs();
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

  if ( hydrographs.count() > 1 && internalHyd && mJunctionNode->internalHydrographOrigin() != ReosHydrographJunction::None )
    hydrographs.append( internalHyd );

  QList<ReosTimeSerieVariableTimeStep *> tsList;

  for ( ReosHydrograph *hyd : std::as_const( hydrographs ) )
  {
    mHydrographPlotButton->addData( hyd );
    tsList.append( hyd );
  }

  ui->mHydrographTabsWidget->setSeries( tsList, QString( "m%1/s" ).arg( QChar( 0x00B3 ) ) );

  updateGaugedHydrograph();
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

void ReosHydraulicHydrographJunctionPropertiesWidget::updateGaugedHydrograph()
{
  mGaugedHydrographPlotButton->clear();
  QPair<QDateTime, QDateTime> timeExent = mJunctionNode->outputHydrograph()->timeExtent();
  QList<ReosHydrograph *> gaugedHyd;
  if ( !timeExent.first.isValid() || !timeExent.second.isValid() )
    gaugedHyd = mJunctionNode->gaugedHydrographsStore()->allHydrographs();
  else
    gaugedHyd = mJunctionNode->gaugedHydrographsStore()->hydrographsForTimeRange( timeExent.first, timeExent.second );

  for ( ReosHydrograph *hyd : gaugedHyd )
  {
    if ( mJunctionNode->internalHydrographOrigin() != ReosHydrographJunction::GaugedHydrograph || hyd != mJunctionNode->internalHydrograph() )
    {
      ReosPlotItem *itemPlot = mGaugedHydrographPlotButton->addData( hyd );
      if ( itemPlot )
      {
        itemPlot->setAutoScale( false );
        itemPlot->setStyle( Qt::DotLine );
        itemPlot->setWidth( 2 );
        itemPlot->setZ( 15 );
      }
    }
  }
}

ReosHydraulicHydrographNodePropertiesWidgetFactory::ReosHydraulicHydrographNodePropertiesWidgetFactory( QObject *parent )
  : ReosHydraulicElementWidgetFactory( parent ) {}

ReosHydraulicElementWidget *ReosHydraulicHydrographNodePropertiesWidgetFactory::createWidget( ReosHydraulicNetworkElement *element, const ReosGuiContext &context )
{
  ReosHydrographJunction *junctionNode = qobject_cast<ReosHydrographJunction *>( element );
  return new ReosHydraulicHydrographJunctionPropertiesWidget( junctionNode, context );
}

QString ReosHydraulicHydrographNodePropertiesWidgetFactory::elementType()
{
  return ReosHydrographNode::staticType();
}

ReosFormWidget *ReosFormWatershedNodeWidgetFactory::createDataWidget( ReosDataObject *dataObject, const ReosGuiContext &context )
{
  ReosHydrographNodeWatershed *watershedNode = qobject_cast<ReosHydrographNodeWatershed *>( dataObject );
  if ( !watershedNode )
    return nullptr;

  return new ReosFormWatershedNodeWidget( watershedNode, context );
}

QString ReosFormWatershedNodeWidgetFactory::datatype() const
{
  return ReosHydrographNodeWatershed::staticType();
}

ReosFormWatershedNodeWidget::ReosFormWatershedNodeWidget( ReosHydrographNodeWatershed *watershedNode, const ReosGuiContext &context )
  : ReosFormBaseJunctionNodeWidget( watershedNode, context )
  , mNode( watershedNode )
{
  QWidget *hydWidget = new QWidget( this );
  hydWidget->setSizePolicy( QSizePolicy::Ignored, QSizePolicy::Preferred );
  QGridLayout *gridLayout = new QGridLayout;
  hydWidget->setLayout( gridLayout );
  gridLayout->setContentsMargins( 0, 0, 0, 0 );
  gridLayout->setSpacing( 6 );
  QLabel *labelOri = new QLabel( tr( "Hydrograph origin" ), this );
  labelOri->setSizePolicy( QSizePolicy::Minimum, QSizePolicy::Fixed );
  gridLayout->addWidget( labelOri, 0, 0 );
  mOriginCombo = new QComboBox( this );
  gridLayout->addWidget( mOriginCombo, 0, 1 );
  mOriginCombo->addItem( QObject::tr( "Runoff hydrograph calculation" ), ReosHydrographNodeWatershed::RunoffHydrograph );
  mOriginCombo->addItem( QObject::tr( "Gauged hydrograph" ), ReosHydrographNodeWatershed::GaugedHydrograph );

  mGaugedLabel = new QLabel( QObject::tr( "Gauged Hydrograph" ), this );
  mGaugedLabel->setSizePolicy( QSizePolicy::Minimum, QSizePolicy::Fixed );
  gridLayout->addWidget( mGaugedLabel, 1, 0 );

  mGaugedHydrographCombo = new QComboBox( this );
  gridLayout->addWidget( mGaugedHydrographCombo, 1, 1 );
  mGaugedHydrographCombo->setSizeAdjustPolicy( QComboBox::AdjustToContents );
  gridLayout->setColumnStretch( 1, 1 );

  mOriginCombo->setCurrentIndex( mOriginCombo->findData( watershedNode->internalHydrographOrigin() ) );

  addWidget( hydWidget, 0 );

  addLine( 1 );

  originChange();

  connect( mGaugedHydrographCombo, QOverload<int>::of( &QComboBox::currentIndexChanged ), mNode, &ReosHydrographNodeWatershed::setGaugedHydrographIndex );
  connect( mOriginCombo, QOverload<int>::of( &QComboBox::currentIndexChanged ), this,  &ReosFormWatershedNodeWidget::originChange );

  connect( mNode->gaugedHydrographsStore(), &ReosDataObject::dataChanged, this, &ReosFormWatershedNodeWidget::updateGaugedHydrograph );

}

ReosHydrographJunction *ReosFormWatershedNodeWidget::node() const
{
  return mNode;
}


void ReosFormWatershedNodeWidget::originChange()
{
  ReosHydrographNodeWatershed::InternalHydrographOrigin origin =
    static_cast<ReosHydrographNodeWatershed::InternalHydrographOrigin>( mOriginCombo->currentData().toInt() );

  mNode->setInternalHydrographOrigin( origin );
  updateGaugedHydrograph();
}

ReosFormBaseJunctionNodeWidget::ReosFormBaseJunctionNodeWidget( ReosHydrographJunction *junction, const ReosGuiContext &context )
  : ReosFormWidget( context.parent() )
{
  addParameter( junction->useForceOutputTimeStep(), -1, ReosParameterWidget::SpacerAfter );
  ReosParameterWidget *otsw = addParameter( junction->forceOutputTimeStep(), -1, ReosParameterWidget::SpacerAfter );
  otsw->setVisible( junction->useForceOutputTimeStep()->value() );
  connect( junction->useForceOutputTimeStep(), &ReosParameter::valueChanged, otsw, [otsw, junction]
  {
    otsw->setVisible( junction->useForceOutputTimeStep()->value() );
  } );

  QToolButton *gaugedButton = new QToolButton( this );
  gaugedButton->setToolButtonStyle( Qt::ToolButtonTextBesideIcon );
  gaugedButton->setAutoRaise( true );
  gaugedButton->setText( tr( "Gauged Hydrographs Manager" ) );
  gaugedButton->setIcon( QPixmap( QStringLiteral( ":/images/gaugedHydrograph.svg" ) ) );
  gaugedButton->setIconSize( QSize( 20, 20 ) );
  addWidget( gaugedButton );

  connect( gaugedButton, &QToolButton::clicked, this, [this, context, junction]
  {
    ReosGaugedHydrographWidget *gaugedWidget = new ReosGaugedHydrographWidget( context.map(), this );
    gaugedWidget->setHydrographStore( junction->gaugedHydrographsStore() );
    emit stackedPageWidgetOpened( gaugedWidget );
  } );
}

void ReosFormBaseJunctionNodeWidget::updateGaugedHydrograph()
{
  if ( !node() )
    return;

  switch ( node()->internalHydrographOrigin() )
  {
    case ReosHydrographNodeWatershed::None:
    case ReosHydrographNodeWatershed::RunoffHydrograph:
      mGaugedHydrographCombo->setVisible( false );
      mGaugedLabel->setVisible( false );
      break;
    case ReosHydrographNodeWatershed::GaugedHydrograph:
      mGaugedHydrographCombo->blockSignals( true );
      mGaugedHydrographCombo->clear();
      const QStringList hydrographNames = node()->gaugedHydrographsStore()->hydrographNames();
      for ( const QString &name : hydrographNames )
        mGaugedHydrographCombo->addItem( name );

      if ( node()->gaugedHydrographIndex() < mGaugedHydrographCombo->count() )
        mGaugedHydrographCombo->setCurrentIndex( node()->gaugedHydrographIndex() );

      mGaugedHydrographCombo->setVisible( true );
      mGaugedHydrographCombo->blockSignals( false );
      mGaugedLabel->setVisible( true );
      break;
  }
}

ReosFormWidget *ReosFormJunctionNodeWidgetFactory::createDataWidget( ReosDataObject *dataObject, const ReosGuiContext &context )
{
  ReosHydrographJunction *junctionNode = qobject_cast<ReosHydrographJunction *>( dataObject );
  if ( !junctionNode )
    return nullptr;

  return new ReosFormJunctionNodeWidget( junctionNode, context );
}

QString ReosFormJunctionNodeWidgetFactory::datatype() const
{
  return ReosHydrographJunction::staticType();
}

ReosFormJunctionNodeWidget::ReosFormJunctionNodeWidget( ReosHydrographJunction *junction, const ReosGuiContext &context )
  : ReosFormBaseJunctionNodeWidget( junction, context )
  , mJunctioNode( junction )
{
  QCheckBox *checkBoxGauged = new QCheckBox( tr( "Inject gauged hydrograph" ), this );
  checkBoxGauged->setChecked( junction->internalHydrographOrigin() == ReosHydrographJunction::GaugedHydrograph );
  addWidget( checkBoxGauged, 0 );

  QWidget *gaugedWidget = new QWidget( this );
  QHBoxLayout *gaugedLayout = new QHBoxLayout ;
  gaugedLayout->setContentsMargins( 0, 0, 0, 0 );
  gaugedWidget->setLayout( gaugedLayout );
  gaugedWidget->setSizePolicy( QSizePolicy::Ignored, QSizePolicy::Preferred );
  mGaugedLabel = new QLabel( QObject::tr( "Hydrograph to inject" ), this );
  gaugedLayout->addWidget( mGaugedLabel );
  gaugedLayout->addSpacerItem( new QSpacerItem( 0, 0 ) );
  mGaugedHydrographCombo = new QComboBox( this );
  mGaugedHydrographCombo->setSizeAdjustPolicy( QComboBox::AdjustToContents );
  gaugedLayout->addWidget( mGaugedHydrographCombo );
  gaugedLayout->setStretch( 2, 1 );
  addWidget( gaugedWidget, 1 );

  addLine( 2 );

  connect( checkBoxGauged, &QCheckBox::toggled, this, [this, checkBoxGauged]
  {
    mJunctioNode->setInternalHydrographOrigin( checkBoxGauged->isChecked() ? ReosHydrographJunction::GaugedHydrograph : ReosHydrographJunction::None );
    updateGaugedHydrograph();
  } );

  updateGaugedHydrograph();

  connect( junction->gaugedHydrographsStore(), &ReosDataObject::dataChanged, this, &ReosFormJunctionNodeWidget::updateGaugedHydrograph );
  connect( mGaugedHydrographCombo, QOverload<int>::of( &QComboBox::currentIndexChanged ), mJunctioNode, &ReosHydrographNodeWatershed::setGaugedHydrographIndex );
}

ReosHydrographJunction *ReosFormJunctionNodeWidget::node() const
{
  return mJunctioNode;
}
