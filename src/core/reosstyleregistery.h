/***************************************************************************
  reosstyleregistery.h - ReosStyleRegistery

 ---------------------
 begin                : 26.11.2021
 copyright            : (C) 2021 by Vincent Cloarec
 email                : vcloarec at gmail dot com
 ***************************************************************************
 *                                                                         *
 *   This program is free software; you can redistribute it and/or modify  *
 *   it under the terms of the GNU General Public License as published by  *
 *   the Free Software Foundation; either version 2 of the License, or     *
 *   (at your option) any later version.                                   *
 *                                                                         *
 ***************************************************************************/
#ifndef REOSSTYLEREGISTERY_H
#define REOSSTYLEREGISTERY_H

#include "reosmodule.h"

class REOSCORE_EXPORT ReosStyleRegistery: public ReosModule
{
  public:
    ReosStyleRegistery( ReosModule *parent = nullptr );
    ~ReosStyleRegistery();

    static void instantiate( ReosModule *parent );
    static ReosStyleRegistery *instance();

    QColor curveColor() const;

    QColor blueReos( int alpha = 255 )
    {
      return QColor( 0, 155, 242, alpha );
    }

    QColor orangeReos( int alpha = 255 )
    {
      return QColor( 250, 175, 100, alpha );
    }

    QColor invalidColor( int alpha = 255 )
    {
      return QColor( 255, 0, 0, alpha );
    }

  private:
    static ReosStyleRegistery *sInstance;

    QList<QColor> mCurveColor;
    mutable int mLastCurveColor = -1;
};

#endif // REOSSTYLEREGISTERY_H
