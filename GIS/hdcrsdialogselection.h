/***************************************************************************
                      hdcrsdialogselection.h
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

#ifndef HDCRSDIALOGSELECTION_H
#define HDCRSDIALOGSELECTION_H

#include <QDialog>

#include <qgsprojectionselectiontreewidget.h>

namespace Ui
{
  class HDCRSDialogSelection;
}

class HdCRSDialogSelection : public QDialog
{
    Q_OBJECT

  public:
    explicit HdCRSDialogSelection( QWidget *parent = nullptr );
    ~HdCRSDialogSelection();

    void setCrs( const QgsCoordinateReferenceSystem &crs );
    QgsCoordinateReferenceSystem getCrs();

  private:
    Ui::HDCRSDialogSelection *ui;
    QgsProjectionSelectionTreeWidget *crsWidget;
};

#endif // HDCRSDIALOGSELECTION_H
