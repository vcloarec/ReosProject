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

#include "reoshydraulicelementpropertieswidget.h"
#include "reoshydraulicnetwork.h"

#include "reoswatershedmodule.h"
#include "reoshydrographroutingpropertieswidget.h"
#include "reoshydraulichydrographjunctionpropertieswidget.h"
#include "reoshydrographtransfer.h"
#include "reosformwidget.h"
#include "reoshydraulicnetworkwidget.h"

ReosHydraulicElementPropertiesWidget::ReosHydraulicElementPropertiesWidget( ReosWatershedModule *watershedModule, QWidget *parent ) :
  ReosActionWidget( parent )
{
  setWindowFlag( Qt::Dialog );
  QVBoxLayout *mainLayout = new QVBoxLayout( this );
  setLayout( mainLayout );

  QHBoxLayout *headerLayout = new QHBoxLayout( this );
  headerLayout->setContentsMargins( 0, 0, 0, 0 );
  headerLayout->setSpacing( 6 );
  mainLayout->addItem( headerLayout );
  mNameLayout = new QHBoxLayout( this );
  mNameLayout->setContentsMargins( 0, 0, 0, 0 );
  headerLayout->addItem( mNameLayout );

  mMeteoModelCombo = new QComboBox( this );
  mMeteoModelCombo->setModel( watershedModule->meteoModelsCollection() );
  headerLayout->addWidget( mMeteoModelCombo );

  mMainLayout = mainLayout;

  mDefaultWidgetfactory = new ReosHydraulicElementWidgetFactory( this );

  addWidgetFactory( new ReosHydrographRoutingPropertiesWidgetFactory( this ) );
  addWidgetFactory( new ReosHydraulicHydrographNodePropertiesWidgetFactory( this ) );

  ReosFormWidgetFactories::instance()->addDataWidgetFactory( new ReosFormMuskingumClassicRoutingWidgetFactory );
  ReosFormWidgetFactories::instance()->addDataWidgetFactory( new ReosFormWatershedNodeWidgetFactory );

  connect( mMeteoModelCombo, QOverload<int>::of( &QComboBox::currentIndexChanged ), this, &ReosHydraulicElementPropertiesWidget::updateElementCalculation );
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
    setWindowTitle( mCurrentElement->name()->value() );
    newWidget = widgetFactory( element->type() )->createWidget( element, this );
    newNameWidget = new ReosParameterStringWidget( element->name(), this );
    mMeteoModelCombo->show();
  }
  else
  {
    setWindowTitle( tr( "No Element Selected" ) );
    mMeteoModelCombo->hide();
  }


  if ( newWidget )
  {
    if ( mCurrentWidget )
      mMainLayout->replaceWidget( mCurrentWidget, newWidget );
    else
      mMainLayout->addWidget( newWidget );

    if ( mNameParameterWidget )
      mNameLayout->replaceWidget( mNameParameterWidget, newNameWidget );
    else
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

ReosHydraulicElementWidget *ReosHydraulicElementWidgetFactory::createWidget( ReosHydraulicNetworkElement *element, QWidget *parent )
{
  ReosHydraulicElementWidget *ret = new ReosHydraulicElementWidget( parent );
  ret->setLayout( new QVBoxLayout );
  ret->layout()->addWidget( new QLabel( tr( "No widget available for the element of type %1" ).arg( element->type() ), ret ) );
  return ret;
}
