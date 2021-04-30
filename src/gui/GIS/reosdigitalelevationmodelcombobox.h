/***************************************************************************
                      reosdigitalelevationmodelcombobox.h
                     --------------------------------------
Date                 : October-2020
Copyright            : (C) 2020 by Vincent Cloarec
email                : vcloarec at gmail dot com
 ***************************************************************************
 *                                                                         *
 *   This program is free software; you can redistribute it and/or modify  *
 *   it under the terms of the GNU General Public License as published by  *
 *   the Free Software Foundation; either version 2 of the License, or     *
 *   (at your option) any later version.                                   *
 *                                                                         *
 ***************************************************************************/

#ifndef REOSDIGITALELEVATIONMODELCOMBOBOX_H
#define REOSDIGITALELEVATIONMODELCOMBOBOX_H

#include <QComboBox>

#include "reosgisengine.h"

class ReosDigitalElevationModelComboBox: public QComboBox
{
    Q_OBJECT
  public:
    ReosDigitalElevationModelComboBox( QWidget *parent, ReosGisEngine *gisEngine = nullptr );

    void setGisEngine( ReosGisEngine *gisEngine );
    QString currentDemLayerId() const;
    void setCurrentDemLayer( const QString &layerId );

  signals:
    void currentDigitalElevationChanged( QString currentId );

  private slots:
    void onDemChanged();

  private:
    ReosGisEngine *mGisEngine = nullptr;

    void updateItems();
};

#endif // REOSDIGITALELEVATIONMODELCOMBOBOX_H
