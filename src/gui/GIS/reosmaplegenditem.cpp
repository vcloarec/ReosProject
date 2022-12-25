/***************************************************************************
  reosmaplegenditem.cpp - ReosMapLegendItem

 ---------------------
 begin                : 20.11.2022
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
#include "reosmaplegenditem.h"

#include <qgsmapcanvas.h>
#include <QDebug>

#include "reosmap.h"
#include "reosmappolygon_p.h"
#include "reosrenderersettings.h"


ReosColorRampMapLegendItem::ReosColorRampMapLegendItem( ReosColorShaderSettings *settings )
  : mSettings( settings )
  , mSize( 7, 20 )
  , mRampBoxSize( 3, 20 )
{
}

QRectF ReosColorRampMapLegendItem::boundingRect() const
{
  qreal penWidth = 1;
  return QRectF( -10 - penWidth / 2, -10 - penWidth / 2,
                 20 + penWidth, 20 + penWidth );
}

void ReosColorRampMapLegendItem::setSize( const QSize &size )
{
  mSize = size;
}

void ReosColorRampMapLegendItem::setHorizontalDistanceFromCanvasBorder( int hd )
{
  mHorDistBorder = hd;
}

void ReosColorRampMapLegendItem::setVerticalDistanceFromCanvasBorder( int vd )
{
  mVertDistBorder = vd;
}

void ReosColorRampMapLegendItem::paint( QPainter *painter, const QStyleOptionGraphicsItem *, QWidget * )
{
  int height = painter->device()->height();
  int width = painter->device()->width();

  int phyDpiX = painter->device()->physicalDpiX();
  int phyDpiY = painter->device()->physicalDpiY();

  if ( mSettings .isNull() || !mSettings->isValid() )
    return;

  painter->save();

  double rampBoxheight = mRampBoxSize.height() / 25.4 * phyDpiY;
  double rampBoxwidth = mRampBoxSize.width() / 25.4 * phyDpiX;
  double vertDistBorder = mVertDistBorder / 25.4 * phyDpiY;
  double horDistBorder = mHorDistBorder / 25.4 * phyDpiY;

  QLinearGradient gradient = mSettings->gradient();
  int topGradientBox = height - ( rampBoxheight + vertDistBorder );
  int bottomGradiantBox = height - vertDistBorder;
  int leftGradientBox = width - ( rampBoxwidth + horDistBorder );

  gradient.setStart( leftGradientBox, bottomGradiantBox );
  gradient.setFinalStop( width - ( rampBoxwidth + horDistBorder ), topGradientBox );

  painter->setBrush( gradient );

  painter->drawRect( leftGradientBox,
                     topGradientBox,
                     rampBoxwidth, rampBoxheight );

  const QString textTop = QLocale().toString( mSettings->classificationMaximum(), 'f', 2 );
  drawLabel( painter, textTop, topGradientBox, leftGradientBox );
  const QString textBottom = QLocale().toString( mSettings->classificationMinimum(), 'f', 2 );
  drawLabel( painter, textBottom, bottomGradiantBox, leftGradientBox );

  painter->restore();
}

void ReosColorRampMapLegendItem::drawLabel( QPainter *painter, const QString &text, double vertPosition, double horizontalItemBorder )
{
  int phyDpiX = painter->device()->physicalDpiX();
  double distFromItem = mDistLabelFromItem / 25.4 * phyDpiX;
  QRectF extentTextBottom( horizontalItemBorder - mDistLabelFromItem, vertPosition, 0, 0 );
  extentTextBottom = painter->boundingRect( extentTextBottom, Qt::AlignRight | Qt::AlignVCenter, text );
  painter->drawText( extentTextBottom, Qt::AlignRight | Qt::AlignVCenter, text );
}

