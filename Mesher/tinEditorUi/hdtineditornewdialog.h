/***************************************************************************
                      hdtineditornewdialog.h
                     --------------------------------------
Date                 : 10-05-2019
Copyright            : (C) 2019 by Vincent Cloarec
email                : vcloarec at gmail dot com / projetreos at gmail dot com
 ***************************************************************************
 *                                                                         *
 *   This program is free software; you can redistribute it and/or modify  *
 *   it under the terms of the GNU General Public License as published by  *
 *   the Free Software Foundation; either version 2 of the License, or     *
 *   (at your option) any later version.                                   *
 *                                                                         *
 ***************************************************************************/

#ifndef HDTINEDITORNEWDIALOG_H
#define HDTINEDITORNEWDIALOG_H

#include <QDialog>
#include <QFileDialog>

#include <qgsproject.h>
#include <qgsprojectionselectionwidget.h>
#include <qgscoordinatereferencesystem.h>

#include "../../Reos/reossettings.h"

namespace Ui {
class HdTinEditorNewDialog;
}

class HdTinEditorNewDialog : public QDialog
{
    Q_OBJECT

public:
    explicit HdTinEditorNewDialog(QWidget *parent = nullptr);
    ~HdTinEditorNewDialog();

    QString fileName() const;
    QString name() const;
    QgsCoordinateReferenceSystem crs() const;


private slots:
    void fileDialog();

private:
    Ui::HdTinEditorNewDialog *ui;
    QgsProjectionSelectionWidget *mCrsWidget;
};

#endif // HDTINEDITORNEWDIALOG_H
