/***************************************************************************
                      reosmapextent.h
                     --------------------------------------
Date                 : 01-10-2020
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

#ifndef REOSMAPEXTENT_H
#define REOSMAPEXTENT_H

#include <QRectF>

/**
 * Class that represent a extent in a map
 */
class ReosMapExtent
{
  public:
    ReosMapExtent( QRectF extent );
    ReosMapExtent( double xMin, double yMin, double xMax, double Ymax );

    double width();
    double height();

    double xMin();
    double xMax();
    double yMin();
    double yMax();

  protected:
    double mXMin;
    double mXMax;
    double mYMin;
    double mYMax;
};

#endif // REOSMAPEXTENT_H
