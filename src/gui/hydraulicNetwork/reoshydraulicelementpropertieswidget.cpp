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

#include "reoshydraulicelementpropertieswidget.h"
#include "reoshydraulicnetwork.h"

#include "reoshydrographroutingpropertieswidget.h"
#include "reoshydrographtransfer.h"
#include "reosformwidget.h"

ReosHydraulicElementPropertiesWidget::ReosHydraulicElementPropertiesWidget( QWidget *parent ) :
  ReosActionWidget( parent )
{
  setWindowFlag( Qt::Dialog );
  setLayout( new QVBoxLayout );
  mDefaultWidgetfactory = new ReosHydraulicElementWidgetFactory( this );

  mWidgetFactories.insert( ReosHydrographRouting::typeString(), new ReosHydrographRoutingPropertiesWidgetFactory( this ) );
  ReosFormWidgetFactories::instance()->addDataWidgetFactory( new ReosFormMuskingumClassicRoutingWidgetFactory );
}

ReosHydraulicElementPropertiesWidget::~ReosHydraulicElementPropertiesWidget()
{}

void ReosHydraulicElementPropertiesWidget::setCurrentElement( ReosHydraulicNetworkElement *element )
{
  mCurrentElement = element;
  if ( mCurrentElement )
    setWindowTitle( mCurrentElement->type() );
  else
    setWindowTitle( tr( "No Element Selected" ) );

  QWidget *newWidget = widgetFactory( element->type() )->createWidget( element, this );

  if ( newWidget )
  {
    if ( mCurrentWidget )
      layout()->replaceWidget( mCurrentWidget, newWidget );
    else
      layout()->addWidget( newWidget );
  }

  delete mCurrentWidget;
  mCurrentWidget = newWidget;
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

QWidget *ReosHydraulicElementWidgetFactory::createWidget( ReosHydraulicNetworkElement *element, QWidget *parent )
{
  return new QLabel( tr( "No widget available for the element of type %1" ).arg( element->type() ), parent );
}
