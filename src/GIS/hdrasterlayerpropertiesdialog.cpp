/***************************************************************************
                      hdrasterlayerpropertiesdialog.cpp
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

#include "hdrasterlayerpropertiesdialog.h"
#include "ui_hdrasterlayerpropertiesdialog.h"

HdRasterLayerPropertiesDialog::HdRasterLayerPropertiesDialog( QgsRasterLayer *layer, QgsMapCanvas *canvas ) :
  QDialog( canvas ),
  ui( new Ui::HdRasterLayerPropertiesDialog )
{
  ui->setupUi( this );

  QgsRasterLayerProperties *es = new QgsRasterLayerProperties( layer, canvas );
  es->exec();


}

HdRasterLayerPropertiesDialog::~HdRasterLayerPropertiesDialog()
{
  delete ui;
}
