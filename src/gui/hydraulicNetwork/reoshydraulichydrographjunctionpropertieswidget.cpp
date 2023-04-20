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
#include <QPushButton>

#include "reoshydrographsource.h"
#include "reoshydrographrouting.h"
#include "reoshydraulicstructureboundarycondition.h"
#include "reosplotitemlist.h"
#include "reosplottimeconstantinterval.h"
#include "reostimeseriesvariabletimestepreadonlymodel.h"
#include "reosgaugedhydrographwidget.h"
#include "reosvariabletimesteptimeseriesgroupwidget.h"
#include "reostimeseriesgroup.h"
#include "reosstyleregistery.h"
#include "reoshydraulicstructure2d.h"
#include "reossettings.h"


ReosHydraulicHydrographJunctionPropertiesWidget::ReosHydraulicHydrographJunctionPropertiesWidget( ReosHydrographJunction *junctionNode, const ReosGuiContext &context )
  : ReosHydraulicElementWidget( context.parent() )
  , ui( new Ui::ReosHydraulicHydrographJunctionPropertiesWidget )
  , mJunctionNode( junctionNode )
{
  ui->setupUi( this );

  QString settingsString = QStringLiteral( "hydraulic-network-node-watershed" );
  ui->mPlotWidget->setTitleAxeX( tr( "Time" ) );
  ui->mPlotWidget->setAxeXType( ReosPlotWidget::temporal );
  ui->mPlotWidget->enableTimeLine( true );
  ui->mPlotWidget->enableAxeYRight( false );
  ui->mPlotWidget->setTitleAxeYLeft( tr( "Flow rate (%1)" ).arg( QString( "m%1/s" ).arg( QChar( 0x00B3 ) ) ) );
  ui->mPlotWidget->setMagnifierType( ReosPlotWidget::positiveMagnifier );
  ui->mPlotWidget->setSettingsContext( settingsString );

  mProgressControler = new ReosHydrauylicNetworkElementCalculationControler( junctionNode, this );
  mProgressControler->setProgressBar( ui->mProgressBar );

  ReosSettings settings;
  mHydrographPlotButton = new ReosVariableTimeStepPlotListButton( tr( "Calc. Hydrographs" ), ui->mPlotWidget );
  if ( settings.contains( settingsString + QStringLiteral( "/hydrograph-button-checked" ) ) )
    mHydrographPlotButton->setChecked( settings.value( settingsString + QStringLiteral( "/hydrograph-button-checked" ) ).toBool() );
  else
    mHydrographPlotButton->setChecked( true );

  mGaugedHydrographPlotButton = new ReosVariableTimeStepPlotListButton( tr( "Gauged Hydrographs" ), ui->mPlotWidget );
  if ( settings.contains( settingsString + QStringLiteral( "/hydrograph-button-checked" ) ) )
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
  connect( mJunctionNode->gaugedHydrographsStore(), &ReosHydrographsStore::hydrographChanged, this, &ReosHydraulicHydrographJunctionPropertiesWidget::updateGaugedHydrograph );

  if ( context.map() )
    connect( context.map(), &ReosMap::timeChanged, ui->mPlotWidget, &ReosPlotWidget::setTime );

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
  qDebug() << QString( "********** Hydrograph junction %1" ).arg( mJunctionNode->elementName()->value() ) << QString( " with %1 values" ).arg( mJunctionNode->outputHydrograph()->valueCount() );

  for ( ReosHydrographRoutingLink *routing : std::as_const( upstreamRoutingList ) )
    hydrographs.append( routing->outputHydrograph() );

  if ( hydrographs.count() > 1 && internalHyd && mJunctionNode->internalHydrographOrigin() != ReosHydrographJunction::None )
    hydrographs.append( internalHyd );

  QList<ReosTimeSeriesVariableTimeStep *> tsList;

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

  for ( ReosHydrograph *hyd : std::as_const( gaugedHyd ) )
  {
    if ( mJunctionNode->internalHydrographOrigin() != ReosHydrographJunction::GaugedHydrograph || hyd != mJunctionNode->internalHydrograph() )
    {
      ReosPlotItem *itemPlot = mGaugedHydrographPlotButton->addData( hyd );
      if ( itemPlot )
      {
        itemPlot->setAutoScale( false );
        //itemPlot->setStyle( Qt::DotLine );  //create weird plot trying to simplify the curve
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
  if ( junctionNode )
    return new ReosHydraulicHydrographJunctionPropertiesWidget( junctionNode, context );

  return nullptr;
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

  connect( mNode, &ReosDataObject::dataChanged, this, &ReosFormWatershedNodeWidget::syncToNode );
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

void ReosFormWatershedNodeWidget::syncToNode()
{
  mOriginCombo->setCurrentIndex( mOriginCombo->findData( mNode->internalHydrographOrigin() ) );
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
  gaugedButton->setIcon( QIcon( QStringLiteral( ":/images/gaugedHydrograph.svg" ) ) );
  gaugedButton->setIconSize( QSize( 20, 20 ) );
  addWidget( gaugedButton );

  connect( gaugedButton, &QToolButton::clicked, this, [this, context, junction]
  {
    ReosGaugedHydrographWidget *gaugedWidget = new ReosGaugedHydrographWidget( ReosGuiContext( context, this ) );
    gaugedWidget->setHydrographStore( junction->gaugedHydrographsStore() );
    emit stackedPageWidgetOpened( gaugedWidget, true );
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
      mGaugedHydrographCombo->addItems( node()->gaugedHydrographsStore()->hydrographNames() );

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
  mCheckBoxGauged = new QCheckBox( tr( "Inject gauged hydrograph" ), this );
  mCheckBoxGauged->setChecked( junction->internalHydrographOrigin() == ReosHydrographJunction::GaugedHydrograph );
  addWidget( mCheckBoxGauged, 0 );

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

  connect( mCheckBoxGauged, &QCheckBox::toggled, this, [this]
  {
    mJunctioNode->setInternalHydrographOrigin( mCheckBoxGauged->isChecked() ? ReosHydrographJunction::GaugedHydrograph : ReosHydrographJunction::None );
    updateGaugedHydrograph();
  } );

  updateGaugedHydrograph();
  connect( mJunctioNode, &ReosDataObject::dataChanged, this, &ReosFormJunctionNodeWidget::syncToNode );
  connect( junction->gaugedHydrographsStore(), &ReosDataObject::dataChanged, this, &ReosFormJunctionNodeWidget::updateGaugedHydrograph );
  connect( mGaugedHydrographCombo, QOverload<int>::of( &QComboBox::currentIndexChanged ), mJunctioNode, &ReosHydrographNodeWatershed::setGaugedHydrographIndex );
}

ReosHydrographJunction *ReosFormJunctionNodeWidget::node() const
{
  return mJunctioNode;
}

void ReosFormJunctionNodeWidget::syncToNode()
{
  mCheckBoxGauged->setChecked( mJunctioNode->internalHydrographOrigin() == ReosHydrographJunction::GaugedHydrograph );
  updateGaugedHydrograph();
}

ReosFormWidget *ReosFormJunctionBoundaryConditionWidgetFactory::createDataWidget( ReosDataObject *dataObject, const ReosGuiContext &context )
{
  ReosHydraulicStructureBoundaryCondition *boundary = qobject_cast<ReosHydraulicStructureBoundaryCondition *>( dataObject );
  if ( !boundary )
    return nullptr;

  switch ( boundary->connectionState() )
  {
    case ReosHydraulicStructureBoundaryCondition::ConnectionState::NotConnected:
      return new ReosFormJunctionBoundaryConditionWidget( boundary, context );
      break;
    case ReosHydraulicStructureBoundaryCondition::ConnectionState::ConnectedToUpstreamLink:
      return new ReosFormJunctionBoundaryConditionWidget( boundary, ReosFormJunctionBoundaryConditionWidget::WaterLevel, context );
      break;
    case ReosHydraulicStructureBoundaryCondition::ConnectionState::ConnectedToDownstreamLink:
      return new ReosFormJunctionNodeWidget( boundary, context );
      break;
  }

  return nullptr;
}

QString ReosFormJunctionBoundaryConditionWidgetFactory::datatype() const
{
  return ReosHydraulicStructureBoundaryCondition::staticType();
}

ReosFormJunctionBoundaryConditionWidget::ReosFormJunctionBoundaryConditionWidget( ReosHydraulicStructureBoundaryCondition *boundary, const ReosGuiContext &context )
  : ReosFormBaseJunctionNodeWidget( boundary, context )
  , mNode( boundary )
{
  QHBoxLayout *typeLayout = new QHBoxLayout( this );
  typeLayout->setContentsMargins( 0, 0, 0, 0 );
  typeLayout->addWidget( new QLabel( tr( "Boundary type" ), this ) );
  mTypeCombo = new QComboBox( this );
  mTypeCombo->addItem( tr( "Water Level" ), WaterLevel );
  mTypeCombo->addItem( tr( "Flow rate" ), FlowRate );
  if ( boundary->structure()->hasCapability( ReosHydraulicStructure2D::DefinedExternally ) )
    mTypeCombo->addItem( tr( "Defined externally" ), DefinedExternally );

  typeLayout->addWidget( mTypeCombo );
  addItem( typeLayout, 0 );

  //*** level widget
  mIsElevationConstant = new ReosParameterBooleanWidget( this );
  mIsElevationConstant->setBooleanParameter( boundary->isWaterLevelConstant() );
  mIsElevationConstant->enableSpacer( ReosParameterWidget::SpacerAfter );
  addWidget( mIsElevationConstant, 1 );
  mConstantLevel = new ReosParameterDoubleWidget( this );
  mConstantLevel->enableSpacer( ReosParameterWidget::SpacerInMiddle );
  mConstantLevel->setDouble( boundary->constantWaterElevation() );
  addWidget( mConstantLevel, 2 );

  mWaterLevelSeriesWidget = new QWidget( this );
  mWaterLevelSeriesWidget->setLayout( new QHBoxLayout );
  mWaterLevelSeriesWidget->layout()->setContentsMargins( 0, 0, 0, 0 );
  mWaterLevelSeriesWidget->layout()->addWidget( new QLabel( tr( "Water level series" ), this ) );
  mWaterLevelCombo = new QComboBox( this );
  mWaterLevelSeriesWidget->layout()->addWidget( mWaterLevelCombo );
  connect( mWaterLevelCombo, QOverload<int>::of( &QComboBox::currentIndexChanged ), this, [this]
  {
    mNode->setWaterLevelSeriesIndex( mWaterLevelCombo->currentIndex() );
  } );

  addWidget( mWaterLevelSeriesWidget, 3 );

  mButtonWaterLevelSeries = new QToolButton( this );
  mButtonWaterLevelSeries->setIconSize( ReosStyleRegistery::instance()->toolBarIconSize( this ) );
  mButtonWaterLevelSeries->setToolButtonStyle( Qt::ToolButtonTextBesideIcon );
  mButtonWaterLevelSeries->setAutoRaise( true );
  mButtonWaterLevelSeries->setText( tr( "Water Level Series Manager" ) );
  mButtonWaterLevelSeries->setIcon( QIcon( QStringLiteral( ":/images/gaugedHydrograph.svg" ) ) );
  mButtonWaterLevelSeries->setIconSize( QSize( 20, 20 ) );
  addWidget( mButtonWaterLevelSeries, 4 );

  mConstantLevel->setVisible( boundary->isWaterLevelConstant()->value() );
  mButtonWaterLevelSeries->setVisible( !mNode->isWaterLevelConstant()->value() );
  connect( boundary->isWaterLevelConstant(), &ReosParameter::valueChanged, mConstantLevel, [this]
  {
    mConstantLevel->setVisible( mNode->isWaterLevelConstant()->value() );
    mButtonWaterLevelSeries->setVisible( !mNode->isWaterLevelConstant()->value() );
    mWaterLevelSeriesWidget->setVisible( !mNode->isWaterLevelConstant()->value() );
  } );

  connect( mButtonWaterLevelSeries, &QToolButton::clicked, this, [this, context]
  {
    ReosVariableTimeStepTimeSeriesGroupWidget *waterLevelWidget =
    new ReosVariableTimeStepTimeSeriesGroupWidget( ReosGuiContext( context, this ), tr( "Water Level Series" ), tr( "meter" ), mNode->waterLevelSeriesIndex() );
    waterLevelWidget->setTimeSeriesGroup( mNode->waterLevelSeriesGroup() );
    emit stackedPageWidgetOpened( waterLevelWidget, true );
  } );

  //*** flow rate widget
  mHydrographComboWidget = new QWidget( this );
  mHydrographComboWidget->setLayout( new QHBoxLayout );
  mHydrographComboWidget->layout()->setContentsMargins( 0, 0, 0, 0 );
  mHydrographComboWidget->layout()->addWidget( new QLabel( tr( "Hydrograph" ), this ) );
  mHydrographCombo = new QComboBox( this );
  mHydrographComboWidget->layout()->addWidget( mHydrographCombo );
  connect( mHydrographCombo, QOverload<int>::of( &QComboBox::currentIndexChanged ), this, [this]
  {
    mNode->setGaugedHydrographIndex( mHydrographCombo->currentIndex() );
  } );
  addWidget( mHydrographComboWidget, 5 );

  addLine( 6 );

  connect( mTypeCombo, QOverload<int>::of( &QComboBox::currentIndexChanged ), this, [this]
  {
    if ( mTypeCombo->currentData() == FlowRate )
    {
      mNode->setDefaultConditionType( ReosHydraulicStructureBoundaryCondition::Type::InputFlow );
      mNode->setInternalHydrographOrigin( ReosHydrographJunction::GaugedHydrograph );
    }

    if ( mTypeCombo->currentData() == WaterLevel )
    {
      mNode->setDefaultConditionType( ReosHydraulicStructureBoundaryCondition::Type::OutputLevel );
      mNode->setInternalHydrographOrigin( ReosHydrographJunction::None );
    }

    if ( mTypeCombo->currentData() == DefinedExternally )
    {
      mNode->setDefaultConditionType( ReosHydraulicStructureBoundaryCondition::Type::DefinedExternally );
    }

    updateWidgetsDisplaying();
  } );

  syncToNode();
  connect( mNode, &ReosDataObject::dataChanged, this, &ReosFormJunctionBoundaryConditionWidget::syncToNode );
  connect( mNode->gaugedHydrographsStore(), &ReosDataObject::dataChanged, this, &ReosFormJunctionBoundaryConditionWidget::syncToNode );
}

ReosFormJunctionBoundaryConditionWidget::ReosFormJunctionBoundaryConditionWidget(
  ReosHydraulicStructureBoundaryCondition *boundary,
  ReosFormJunctionBoundaryConditionWidget::Type type,
  const ReosGuiContext &context )
  : ReosFormJunctionBoundaryConditionWidget( boundary, context )
{
  int i = 0;
  while ( i < mTypeCombo->count() )
  {
    if ( mTypeCombo->itemData( i ) != type &&  mTypeCombo->itemData( i ) != DefinedExternally )
    {
      if ( mTypeCombo->currentIndex() == i )
      {
        mTypeCombo->setCurrentIndex( mTypeCombo->findData( type ) );
      }
      mTypeCombo->removeItem( i );
    }
    else
      ++i;
  }

}

void ReosFormJunctionBoundaryConditionWidget::syncToNode()
{
  mTypeCombo->blockSignals( true );
  switch ( mNode->defaultConditionType() )
  {
    case ReosHydraulicStructureBoundaryCondition::Type::InputFlow:
      mTypeCombo->setCurrentIndex( mTypeCombo->findData( FlowRate ) );
      break;
    case ReosHydraulicStructureBoundaryCondition::Type::NotDefined:
    case ReosHydraulicStructureBoundaryCondition::Type::OutputLevel:
      mTypeCombo->setCurrentIndex( mTypeCombo->findData( WaterLevel ) );
      break;
    case ReosHydraulicStructureBoundaryCondition::Type::DefinedExternally:
      mTypeCombo->setCurrentIndex( mTypeCombo->findData( DefinedExternally ) );
      break;
  }
  updateWidgetsDisplaying();
  mTypeCombo->blockSignals( false );

  mWaterLevelCombo->blockSignals( true );
  mWaterLevelCombo->clear();
  ReosTimeSeriesVariableTimeStepGroup *group = mNode->waterLevelSeriesGroup();
  if ( group )
  {
    mWaterLevelCombo->addItems( group->seriesNames() );
    if ( mNode->waterLevelSeriesIndex() >= mWaterLevelCombo->count() )
      mNode->setWaterLevelSeriesIndex( mWaterLevelCombo->count() - 1 );

    mWaterLevelCombo->setCurrentIndex( mNode->waterLevelSeriesIndex() );
    mWaterLevelCombo->blockSignals( false );
  }

  mHydrographCombo->blockSignals( true );
  mHydrographCombo->clear();
  mHydrographCombo->addItems( mNode->gaugedHydrographsStore()->hydrographNames() );
  if ( mNode->gaugedHydrographIndex() >= mHydrographCombo->count() )
    mNode->setGaugedHydrographIndex( mHydrographCombo->count() - 1 );
  mHydrographCombo->setCurrentIndex( mNode->gaugedHydrographIndex() );
  mHydrographCombo->blockSignals( false );
}

void ReosFormJunctionBoundaryConditionWidget::updateWidgetsDisplaying()
{
  switch ( mNode->defaultConditionType() )
  {
    case ReosHydraulicStructureBoundaryCondition::Type::InputFlow:
      mIsElevationConstant->hide();
      mConstantLevel->setVisible( false );
      mButtonWaterLevelSeries->hide();
      mWaterLevelSeriesWidget->hide();
      mHydrographComboWidget->setVisible( true );
      break;
    case ReosHydraulicStructureBoundaryCondition::Type::NotDefined:
    case ReosHydraulicStructureBoundaryCondition::Type::OutputLevel:
      mIsElevationConstant->show();
      mConstantLevel->setVisible( mNode->isWaterLevelConstant()->value() );
      mButtonWaterLevelSeries->setVisible( !mNode->isWaterLevelConstant()->value() );
      mWaterLevelSeriesWidget->setVisible( !mNode->isWaterLevelConstant()->value() );
      mHydrographComboWidget->hide();
      break;
    case ReosHydraulicStructureBoundaryCondition::Type::DefinedExternally:
      mIsElevationConstant->hide();
      mConstantLevel->setVisible( false );
      mButtonWaterLevelSeries->setVisible( false );
      mWaterLevelSeriesWidget->setVisible( false );
      mHydrographComboWidget->hide();
      break;
  }
}

ReosHydrographJunction *ReosFormJunctionBoundaryConditionWidget::node() const
{
  return mNode;
}

