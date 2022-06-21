/***************************************************************************
  reosspeedwidget.h - ReosSpeedWidget

 ---------------------
 begin                : 20.6.2022
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
#ifndef REOSSPEEDWIDGET_H
#define REOSSPEEDWIDGET_H

#include <QWidget>

namespace Ui
{
  class ReosSpeedWidget;
}

class ReosSpeedWidget : public QWidget
{
    Q_OBJECT

  public:
    explicit ReosSpeedWidget( QWidget *parent = nullptr );
    ~ReosSpeedWidget();

    void setSpeedFactor( double speedFactor );

  signals:
    void speedFactorChanged( double factor );

  private:
    Ui::ReosSpeedWidget *ui;

};

#endif // REOSSPEEDWIDGET_H
