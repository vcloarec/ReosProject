/***************************************************************************
                      hdtineditornewdialog.cpp
                     --------------------------------------
Date                 : 10-05-2019
Copyright            : (C) 2019 by Vincent Cloarec
email                : vcloarec at gmail dot com   /  projetreos at gmail dot com
 ***************************************************************************
 *                                                                         *
 *   This program is free software; you can redistribute it and/or modify  *
 *   it under the terms of the GNU General Public License as published by  *
 *   the Free Software Foundation; either version 2 of the License, or     *
 *   (at your option) any later version.                                   *
 *                                                                         *
 ***************************************************************************/

#include "hdtineditornewdialog.h"
#include "ui_hdtineditornewdialog.h"

HdTinEditorNewDialog::HdTinEditorNewDialog(QWidget *parent) :
    QDialog(parent),
    ui(new Ui::HdTinEditorNewDialog)
{
    ui->setupUi(this);

    mCrsWidget=new QgsProjectionSelectionWidget();
    mCrsWidget->setCrs(QgsProject::instance()->crs());
    ui->layoutCRS->addWidget(mCrsWidget);

    connect(ui->toolButtonFile,&QToolButton::clicked,this,&HdTinEditorNewDialog::fileDialog);
}

HdTinEditorNewDialog::~HdTinEditorNewDialog()
{
    delete ui;
}

QString HdTinEditorNewDialog::fileName() const
{
    return ui->lineEditFile->text();
}

QString HdTinEditorNewDialog::name() const
{
    return ui->lineEditName->text();
}

QgsCoordinateReferenceSystem HdTinEditorNewDialog::crs() const
{
    return mCrsWidget->crs();
}

void HdTinEditorNewDialog::fileDialog()
{
    ReosSettings settings;
    QString path=settings.value(QStringLiteral("/Path/TIN")).toString();

    QString fileName=QFileDialog::getSaveFileName(this,tr("File for the TIN"),path,"*.tin");

    if (fileName!="")
        ui->lineEditFile->setText(fileName);
}
