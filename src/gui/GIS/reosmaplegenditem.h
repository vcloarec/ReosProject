/***************************************************************************
  reosmaplegenditem.h - ReosMapLegendItem

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
#ifndef REOSMAPLEGENDITEM_H
#define REOSMAPLEGENDITEM_H

#include <QGraphicsItem>
#include "reosrenderedobject.h"

class ReosColorRampMapLegendItem : public QGraphicsItem
{
  public:
    ReosColorRampMapLegendItem( ReosColorShaderSettings *settings );

    QRectF boundingRect() const override;

    //! Sets the size of the legend
    void setSize( const QSize &size );

    void setHorizontalDistanceFromCanvasBorder( int hd );
    void setVerticalDistanceFromCanvasBorder( int vd );

    void paint( QPainter *painter, const QStyleOptionGraphicsItem *option, QWidget *widget ) override;

    void drawLabel( QPainter *painter, const QString &text, double vertPosition, double horizontalItemBorder );

  private:
    QSize mSize;
    QSize mRampBoxSize;
    int mHorDistBorder = 5;
    int mVertDistBorder = 5;
    int mDistLabelFromItem = 10;

    QPointer<ReosColorShaderSettings> mSettings;

};

#endif // REOSMAPLEGENDITEM_H
