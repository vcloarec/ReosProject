/***************************************************************************
                      hdcrsdialogselection.cpp
                     --------------------------------------
Date                 : 18-11-2018
Copyright            : (C) 2018 by Vincent Cloarec
email                : vcloarec@gmail.com projetreos@gmail.com
 ***************************************************************************
 *                                                                         *
 *   This program is free software; you can redistribute it and/or modify  *
 *   it under the terms of the GNU General Public License as published by  *
 *   the Free Software Foundation; either version 2 of the License, or     *
 *   (at your option) any later version.                                   *
 *                                                                         *
 ***************************************************************************/

#include "hdcrsdialogselection.h"
#include "ui_hdcrsdialogselection.h"

HDCRSDialogSelection::HDCRSDialogSelection(QWidget *parent) :
    QDialog(parent),
    ui(new Ui::HDCRSDialogSelection),
    crsWidget(new QgsProjectionSelectionTreeWidget(this))
{
    ui->setupUi(this);
    ui->layout->addWidget(crsWidget);

}

HDCRSDialogSelection::~HDCRSDialogSelection()
{
    delete ui;
}

void HDCRSDialogSelection::setCrs(const QgsCoordinateReferenceSystem &crs)
{
    crsWidget->setCrs(crs);
}

QgsCoordinateReferenceSystem HDCRSDialogSelection::getCrs()
{
    return crsWidget->crs();
}
