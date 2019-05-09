/***************************************************************************
                      hdrasterlayerpropertiesdialog.h
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

#ifndef HDRASTERLAYERPROPERTIESDIALOG_H
#define HDRASTERLAYERPROPERTIESDIALOG_H

#include <QDialog>


#include <qgspalettedrendererwidget.h>
#include <qgsapplication.h>
#include <qgsrasterrendererregistry.h>
#include <qgsrasterlayerproperties.h>
#include <qgsmapcanvas.h>

namespace Ui {
class HdRasterLayerPropertiesDialog;
}

class HdRasterLayerPropertiesDialog : public QDialog
{
    Q_OBJECT

public:
    explicit HdRasterLayerPropertiesDialog(QgsRasterLayer * layer, QgsMapCanvas *canvas = nullptr);
    ~HdRasterLayerPropertiesDialog();

private:
    Ui::HdRasterLayerPropertiesDialog *ui;
};


#endif // HDRASTERLAYERPROPERTIESDIALOG_H
