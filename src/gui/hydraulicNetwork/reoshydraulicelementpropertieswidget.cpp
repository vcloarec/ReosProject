/***************************************************************************
  reoshydraulicelementpropertieswidget.cpp - ReosHydraulicElementPropertiesWidget

 ---------------------
 begin                : 25.5.2021
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

#include <QVBoxLayout>
#include <QLabel>
#include <QComboBox>
#include <QProgressBar>
#include <QTimer>

#include "reoshydraulicelementpropertieswidget.h"
#include "reoshydraulicnetwork.h"

#include "reoswatershedmodule.h"
#include "reoshydrographroutingpropertieswidget.h"
#include "reoshydraulichydrographjunctionpropertieswidget.h"
#include "reoshydrographrouting.h"
#include "reosformwidget.h"
#include "reoshydraulicnetworkwidget.h"

ReosHydraulicElementPropertiesWidget::ReosHydraulicElementPropertiesWidget( ReosWatershedModule *watershedModule, const ReosGuiContext &guiContext )
  : ReosStackedPageWidget( guiContext.parent() )
  , mGuiContext( guiContext )
{
  QVBoxLayout *mainLayout = new QVBoxLayout;
  setLayout( mainLayout );

  layout()->setSizeConstraint( QLayout::SetNoConstraint );

  QHBoxLayout *headerLayout = new QHBoxLayout;
  headerLayout->setContentsMargins( 0, 0, 0, 0 );
  headerLayout->setSpacing( 6 );
  mainLayout->addItem( headerLayout );
  mNameLayout = new QHBoxLayout;
  mNameLayout->setContentsMargins( 0, 0, 0, 0 );
  headerLayout->addItem( mNameLayout );

  mMeteoModelCombo = new QComboBox( this );
  mMeteoModelCombo->setModel( watershedModule->meteoModelsCollection() );
  headerLayout->addWidget( mMeteoModelCombo );

  mMainLayout = mainLayout;

  mDefaultWidgetfactory = new ReosHydraulicElementWidgetFactory( this );

  addWidgetFactory( new ReosHydrographRoutingPropertiesWidgetFactory( this ) );
  addWidgetFactory( new ReosHydraulicHydrographNodePropertiesWidgetFactory( this ) );

  ReosFormWidgetFactories::instance()->addDataWidgetFactory( new ReosFormHydrographRountingMuskingumWidgetFactory );
  ReosFormWidgetFactories::instance()->addDataWidgetFactory( new ReosFormHydrographRountingLagWidgetFactory );
  ReosFormWidgetFactories::instance()->addDataWidgetFactory( new ReosFormWatershedNodeWidgetFactory );
  ReosFormWidgetFactories::instance()->addDataWidgetFactory( new ReosFormJunctionNodeWidgetFactory );

  connect( mMeteoModelCombo, QOverload<int>::of( &QComboBox::currentIndexChanged ), this, &ReosHydraulicElementPropertiesWidget::updateElementCalculation );

  setCurrentElement( nullptr );
}

ReosHydraulicElementPropertiesWidget::~ReosHydraulicElementPropertiesWidget()
{}

void ReosHydraulicElementPropertiesWidget::setCurrentElement( ReosHydraulicNetworkElement *element )
{
  ReosHydraulicElementWidget *newWidget = nullptr;
  QWidget *newNameWidget = nullptr;
  mCurrentElement = element;

  if ( mCurrentElement )
  {
    newWidget = widgetFactory( element->type() )->createWidget( element, ReosGuiContext( mGuiContext, this ) );
    connect( newWidget, &ReosHydraulicElementWidget::stackedPageWidgetOpened, this, &ReosStackedPageWidget::addOtherPage );
    newNameWidget = new ReosParameterStringWidget( element->name(), this );
    mMeteoModelCombo->show();
  }
  else
  {
    setWindowTitle( tr( "No Element Selected" ) );
    mMeteoModelCombo->hide();
    newNameWidget = new QLabel( tr( "No hydraulic element selected" ), this );
  }

  if ( newWidget )
  {
    if ( mCurrentWidget )
      mMainLayout->replaceWidget( mCurrentWidget, newWidget );
    else
      mMainLayout->addWidget( newWidget );
  }
  else if ( mCurrentWidget )
    mMainLayout->removeWidget( mCurrentWidget );

  if ( mNameParameterWidget )
  {
    mNameLayout->replaceWidget( mNameParameterWidget, newNameWidget );
    newNameWidget->show();
  }
  else
  {
    mNameLayout->addWidget( newNameWidget );
  }

  delete mCurrentWidget;
  delete mNameParameterWidget;
  mCurrentWidget = newWidget;
  mNameParameterWidget = newNameWidget;

  updateElementCalculation();
}

void ReosHydraulicElementPropertiesWidget::updateElementCalculation()
{
  ReosCalculationContext context;
  ReosMeteorologicModelsCollection *meteoCollection = qobject_cast<ReosMeteorologicModelsCollection *>( mMeteoModelCombo->model() );

  if ( meteoCollection )
  {
    context.setMeteorologicModel( meteoCollection->meteorologicModel( mMeteoModelCombo->currentIndex() ) );
  }

  if ( mCurrentWidget )
    mCurrentWidget->setCurrentCalculationContext( context );
}

ReosHydraulicElementWidgetFactory *ReosHydraulicElementPropertiesWidget::widgetFactory( const QString &elementType )
{
  if ( mWidgetFactories.contains( elementType ) )
    return mWidgetFactories.value( elementType );

  //! Search for keys that is contained in the element type
  QStringList keys = mWidgetFactories.keys();
  for ( const QString &key : keys )
  {
    if ( elementType.contains( key ) )
      return mWidgetFactories.value( key );
  }

  return mDefaultWidgetfactory;
}

void ReosHydraulicElementPropertiesWidget::addWidgetFactory( ReosHydraulicElementWidgetFactory *factory )
{
  mWidgetFactories.insert( factory->elementType(), factory );
}

ReosHydraulicElementWidget *ReosHydraulicElementWidgetFactory::createWidget( ReosHydraulicNetworkElement *element, const ReosGuiContext &context )
{
  ReosHydraulicElementWidget *ret = new ReosHydraulicElementWidget( context.parent() );
  ret->setLayout( new QVBoxLayout );
  ret->layout()->addWidget( new QLabel( tr( "No widget available for the element of type %1" ).arg( element->type() ), ret ) );
  return ret;
}

ReosHydrauylicNetworkElementCalculationControler::ReosHydrauylicNetworkElementCalculationControler(
  ReosHydraulicNetworkElement *element,
  QObject *parent )
  : QObject( parent )
  , mElement( element )
{
  connect( mElement, &ReosHydraulicNetworkElement::calculationStart, this,
           &ReosHydrauylicNetworkElementCalculationControler::onCalculationStart );

  connect( mElement, &ReosHydraulicNetworkElement::calculationIsUpdated, this,
           &ReosHydrauylicNetworkElementCalculationControler::onCalculationStop );

}

void ReosHydrauylicNetworkElementCalculationControler::setProgressBar( QProgressBar *progBar )
{
  mProgessBar = progBar;

  if ( mElement->calculationInProgress() )
    onCalculationStart();
  else
    updateState();
}

void ReosHydrauylicNetworkElementCalculationControler::onCalculationStart()
{
  if ( mTimer )
    return;

  mTimer = new QTimer( this );

  connect( mTimer, &QTimer::timeout, this, &ReosHydrauylicNetworkElementCalculationControler::updateState );

  mTimer->setInterval( 100 );
  mTimer->start();
}

void ReosHydrauylicNetworkElementCalculationControler::updateState()
{
  if ( mProgessBar.isNull() )
    return;

  mProgessBar->setMaximum( mElement->calculationMaxProgression() );
  mProgessBar->setValue( mElement->calculationProgression() );
}

void ReosHydrauylicNetworkElementCalculationControler::onCalculationStop()
{
  mTimer->deleteLater();
  mTimer = nullptr;

  if ( !mProgessBar.isNull() )
  {
    mProgessBar->setMaximum( 100 );
    mProgessBar->setValue( 100 );
  }
}

ReosHydraulicElementPropertiesActionWidget::ReosHydraulicElementPropertiesActionWidget( ReosWatershedModule *watershedModule, const ReosGuiContext &guiContext )
  : ReosActionStackedWidget( guiContext.parent() )
{
  setWindowFlag( Qt::Dialog );
  mainPage = new ReosHydraulicElementPropertiesWidget( watershedModule, ReosGuiContext( guiContext, this ) );
  addPage( mainPage );
}

void ReosHydraulicElementPropertiesActionWidget::setCurrentElement( ReosHydraulicNetworkElement *element )
{
  if ( element == mCurrentElement )
    return;

  if ( element )
    setWindowTitle( element->name()->value() );
  else
    setWindowTitle( tr( "No Element Selected" ) );

  backToFirstPage();
  mCurrentElement = element;
  mainPage->setCurrentElement( element );
}
