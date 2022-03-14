/***************************************************************************
  reoscolorbutton.h - ReosColorButton

 ---------------------
 begin                : 11.3.2022
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
#ifndef REOSCOLORBUTTON_H
#define REOSCOLORBUTTON_H

#include <QToolButton>

class QMenu;
class QgsColorWheel;

class ReosColorButton : public QToolButton
{
    Q_OBJECT
  public:
    ReosColorButton( QWidget *parent = nullptr );

    QColor color() const;
    void setColor( const QColor &color );

  signals:
    void colorChanged( const QColor &color );

  private slots:
    void onColorWheelChange( const QColor &color );

  private:
    QMenu *mMenu = nullptr;
    QgsColorWheel *mColorWheel = nullptr;
};

#endif // REOSCOLORBUTTON_H
