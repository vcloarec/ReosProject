/***************************************************************************
                      hdtineditoruidialog.cpp
                     --------------------------------------
Date                 : 10-05-2019
Copyright            : (C) 2018 by Vincent Cloarec
email                : vcloarec at gmail dot com / projetreos at gmail dot com
 ***************************************************************************
 *                                                                         *
 *   This program is free software; you can redistribute it and/or modify  *
 *   it under the terms of the GNU General Public License as published by  *
 *   the Free Software Foundation; either version 2 of the License, or     *
 *   (at your option) any later version.                                   *
 *                                                                         *
 ***************************************************************************/

#include "hdtineditoruidialog.h"
#include "ui_hdtineditoruidialog.h"

#include "reostineditorgraphic.h"

HdTinEditorUiDialog::HdTinEditorUiDialog(QWidget *parent) :
    QDialog(parent),
    ui(new Ui::HdTinEditorUiDialog),
    toolbar(new QToolBar)
{
    ui->setupUi(this);
    ui->layoutAction->addWidget(toolbar);

    ReosSettings settings;
    restoreGeometry(settings.value(QStringLiteral("TinEditorDialog/geometry")).toByteArray());

    connect(this,&QDialog::rejected,this,&HdTinEditorUiDialog::closed);
    connect(this,&QDialog::rejected,this,&HdTinEditorUiDialog::updateSettings);
}

HdTinEditorUiDialog::~HdTinEditorUiDialog()
{
    updateSettings();
    delete ui;
}


void HdTinEditorUiDialog::setZSpecifierWidet(QWidget *widget)
{
    ui->zSpecifierLayout->addWidget(widget);
}

void HdTinEditorUiDialog::setFocus()
{
//    ui->lineEdit->setFocus(Qt::OtherFocusReason);
//    ui->lineEdit->setSelection(0,100);
//    activateWindow();
}


bool HdTinEditorUiDialog::autoUpdate() const
{
    return ui->checkBoxAutoUpdate->isChecked();
}

void HdTinEditorUiDialog::updateSettings()
{
    ReosSettings settings;
    settings.setValue(QStringLiteral("TinEditorDialog/geometry"),saveGeometry());
}

void HdTinEditorUiDialog::setActions(const QList<QAction *> &actions)
{
    for (auto a:actions)
        toolbar->addAction(a);
}

