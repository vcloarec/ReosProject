/***************************************************************************
  reosgmshresolutioncontroller.cpp - ReosGmshResolutionController

 ---------------------
 begin                : 31.1.2022
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
#include "reosgmshresolutioncontrollerwidget.h"
#include "ui_reosgmshresolutioncontroller.h"

ReosGmshResolutionControllerWidget::ReosGmshResolutionControllerWidget( ReosMeshResolutionController *controller, QWidget *parent ) :
  QWidget( parent ),
  ui( new Ui::ReosGmshResolutionControllerWidget )
{
  ui->setupUi( this );

  ReosGmshResolutionController *gmshController = static_cast<ReosGmshResolutionController *>( controller );
  ui->mDefaultSizeParameterWidget->setDouble( gmshController->defaultSize() );
}

ReosGmshResolutionControllerWidget::~ReosGmshResolutionControllerWidget()
{
  delete ui;
}
