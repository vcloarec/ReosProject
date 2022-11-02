/***************************************************************************
  reosdsswidget.cpp - ReosDssWidget

 ---------------------
 begin                : 22.10.2022
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
#include "reosdsswidget.h"
#include "ui_reosdsswidget.h"

ReosDssWidget::ReosDssWidget(QWidget *parent) :
    QWidget(parent),
    ui(new Ui::ReosDssWidget)
{
    ui->setupUi(this);
}

ReosDssWidget::~ReosDssWidget()
{
    delete ui;
}
