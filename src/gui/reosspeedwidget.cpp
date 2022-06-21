/***************************************************************************
  reosspeedwidget.cpp - ReosSpeedWidget

 ---------------------
 begin                : 20.6.2022
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
#include "reosspeedwidget.h"
#include "ui_reosspeedwidget.h"


ReosSpeedWidget::ReosSpeedWidget( QWidget *parent ) :
  QWidget( parent ),
  ui( new Ui::ReosSpeedWidget )
{
  ui->setupUi( this );

  connect( ui->doubleSpinBox, QOverload<double>::of( &QDoubleSpinBox::valueChanged ), this, &ReosSpeedWidget::speedFactorChanged );
}

ReosSpeedWidget::~ReosSpeedWidget()
{
  delete ui;
}

void ReosSpeedWidget::setSpeedFactor( double speedFactor )
{
  ui->doubleSpinBox->setValue( speedFactor );
}
