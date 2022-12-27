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
    explicit ReosColorRampMapLegendItem( ReosColorShaderSettings *settings );

    QRectF boundingRect() const override;


    void paint( QPainter *painter, const QStyleOptionGraphicsItem *option, QWidget *widget ) override;

    void setOrder( int order );
    void setLegendCount( int newLegendCount );

    void resize( QWidget *viewport );

  private:
    QPointer<ReosColorShaderSettings> mSettings;
    QSizeF mSizeHint = QSize( 50.0, 60.0 ); //in mm
    int mOrder = 0;
    int mLegendCount = 0;
    double mHorSpacing = 5.0;
    double mVertSpacing = 5.0;
    double mRampBoxWidth = 7.0;
    double mDistLabelFromItem = 5.0;
    QRectF mBoundingRect;

};

#endif // REOSMAPLEGENDITEM_H
