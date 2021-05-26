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
#include "reoshydraulicelementpropertieswidget.h"
#include "ui_reoshydraulicelementpropertieswidget.h"

#include "reoshydraulicnetwork.h"

ReosHydraulicElementPropertiesWidget::ReosHydraulicElementPropertiesWidget( QWidget *parent ) :
  ReosActionWidget( parent ),
  ui( new Ui::ReosHydraulicElementPropertiesWidget )
{
  ui->setupUi( this );
  setWindowFlag( Qt::Dialog );
}

ReosHydraulicElementPropertiesWidget::~ReosHydraulicElementPropertiesWidget()
{
  delete ui;
}

void ReosHydraulicElementPropertiesWidget::setCurrentElement( ReosHydraulicNetworkElement *element )
{
  mCurrentElement = element;
  if ( mCurrentElement )
    setWindowTitle( mCurrentElement->type() );
  else
    setWindowTitle( tr( "No Element Selected" ) );
}
