/***************************************************************************
  reoslightdirectionwidget.h - ReosLightDirectionWidget

 ---------------------
 begin                : 3.3.2022
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
#ifndef REOSLIGHTWIDGET_H
#define REOSLIGHTWIDGET_H

#include <QWidget>
#include <QWidgetAction>

namespace Ui
{
  class ReosLightWidget;
}

class ReosLightWidget : public QWidget
{
    Q_OBJECT

  public:
    explicit ReosLightWidget( QWidget *parent = nullptr );
    ~ReosLightWidget();

    void setDirection( const QVector3D &direction );
    QVector3D direction() const;

    void setLightIntensity( float intensity );
    float lightIntensity() const;

  signals:

    void directionChanged( const QVector3D &direction );
    void intensityChanged( float value );

  private:
    Ui::ReosLightWidget *ui;
};


#endif // REOSLIGHTWIDGET_H
