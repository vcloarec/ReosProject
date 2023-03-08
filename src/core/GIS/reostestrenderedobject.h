/***************************************************************************
  reostestrenderedobject.h - ReosTestRenderedObject

 ---------------------
 begin                : 23.1.2023
 copyright            : (C) 2023 by Vincent Cloarec
 email                : vcloarec at gmail dot com
 ***************************************************************************
 *                                                                         *
 *   This program is free software; you can redistribute it and/or modify  *
 *   it under the terms of the GNU General Public License as published by  *
 *   the Free Software Foundation; either version 2 of the License, or     *
 *   (at your option) any later version.                                   *
 *                                                                         *
 ***************************************************************************/
#ifndef REOSTESTRENDEREDOBJECT_H
#define REOSTESTRENDEREDOBJECT_H

#define SIP_NO_FILE

#include <QImage>

#include "reoscore.h"

class ReosRenderedObject;

class REOSCORE_EXPORT ReosTestRenderedObject
{
  public:
    ReosTestRenderedObject();

    QImage render( ReosRenderedObject *object, const QDateTime &time );

    bool compareRendering( ReosRenderedObject *object,
                           const QDateTime &time,
                           const QString &imageFile,
                           int tolerance = 5 );

  private:
    QSize mOutputSize = QSize( 256, 256 );
    QImage::Format mImageFormat = QImage::Format_ARGB32;
};

#endif // REOSTESTRENDEREDOBJECT_H
