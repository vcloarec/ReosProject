/***************************************************************************
  reoshydraulicstructrure2dproperties.cpp - ReosHydraulicStructrure2DProperties

 ---------------------
 begin                : 10.1.2022
 copyright            : (C) 2022 by Vincent Cloarec
 email                : vcloarec at gmail dot com
 ***************************************************************************
 *                                                                         *
 *   This program is free software; you can redistribute it and/or modify  *
 *   it under the terms of the GNU General Public License as published by  *
 *   the Free Software Foundation; either version 2 of the License, or     *
 *   (at your option) any later version.                                   *
 *                                                                         *
 ***************************************************************************/
#include "reoshydraulicstructure2dproperties.h"
#include "ui_reoshydraulicstructure2dproperties.h"

#include "reoseditstructure2dwidget.h"

ReosHydraulicStructure2DProperties::ReosHydraulicStructure2DProperties( QWidget *parent ) :
  ReosHydraulicElementWidget( parent ),
  ui( new Ui::ReosHydraulicStructure2DProperties )
{
  ui->setupUi( this );

  connect( ui->mEditStructureToolButton, &QToolButton::clicked, this, [this]
  {
    emit stackedPageWidgetOpened( new ReosEditStructure2DWidget( this ) );
  } );
}

ReosHydraulicStructure2DProperties::~ReosHydraulicStructure2DProperties()
{
  delete ui;
}
