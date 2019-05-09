/***************************************************************************
                      hdvectorlayerpropertiesdialog.h
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

#ifndef HDVECTORLAYERPROPERTIESDIALOG_H
#define HDVECTORLAYERPROPERTIESDIALOG_H

#include <QDialog>
#include <QDesktopServices>


#include <qgsvectorlayer.h>
#include <qgsrendererpropertiesdialog.h>
#include <qgsstyle.h>
#include <qgsmapcanvas.h>
#include <qgsprojectionselectionwidget.h>
#include <qgsgui.h>
#include <qgsnative.h>
#include <qgsapplication.h>


#include <../Reos/reossettings.h>


namespace Ui {
class HdVectorLayerPropertiesDialog;
}

class HdVectorLayerPropertiesDialog : public QDialog
{
    Q_OBJECT

public:
    explicit HdVectorLayerPropertiesDialog(QgsVectorLayer *layer,QgsMapCanvas *canvas);
    ~HdVectorLayerPropertiesDialog();



private slots:

    void apply();
private:
    Ui::HdVectorLayerPropertiesDialog *ui;
    QgsVectorLayer *layer;
    QgsRendererPropertiesDialog *renderDialog=nullptr;
    QgsMapCanvas *canvas;
    QgsProjectionSelectionWidget *crsSelection;

private slots:
    void urlClicked( const QUrl &url );

    void updateSettings();

    // QWidget interface
protected:
    void closeEvent(QCloseEvent *event);

};






#endif // HDVECTORLAYERPROPERTIESDIALOG_H
