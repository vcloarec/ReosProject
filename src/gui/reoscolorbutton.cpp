/***************************************************************************
  reoscolorbutton.cpp - ReosColorButton

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
#include "reoscolorbutton.h"

#include <QMenu>
#include <QPainter>

#include <qgscolorwidgets.h>

ReosColorButton::ReosColorButton( QWidget *parent ): QToolButton( parent )
{
  setAutoRaise( true );

  setPopupMode( QToolButton::InstantPopup );

  mMenu = new QMenu( this );
  mColorWheel = new QgsColorWheel( this );
  QgsColorWidgetAction *colorWheelAction = new QgsColorWidgetAction( mColorWheel, mMenu, this );
  colorWheelAction->setDismissOnColorSelection( false );
  mMenu->addAction( colorWheelAction );
  setMenu( mMenu );

  connect( mColorWheel, &QgsColorWidget::colorChanged, this, &ReosColorButton::onColorWheelChange );
}

QColor ReosColorButton::color() const
{
  return mColorWheel->color();
}

void ReosColorButton::setColor( const QColor &color )
{
  mColorWheel->setColor( color );
  onColorWheelChange( color );
}

void ReosColorButton::onColorWheelChange( const QColor &color )
{
  QPixmap pix( 16, 16 );
  pix.fill( Qt::transparent );
  QPainter painter( &pix );

  QBrush brush;
  brush.setStyle( Qt::SolidPattern );
  brush.setColor( color );
  painter.fillRect( QRect( 1, 1, 14, 14 ), brush );

  setIcon( pix );

  emit colorChanged( color );
}
