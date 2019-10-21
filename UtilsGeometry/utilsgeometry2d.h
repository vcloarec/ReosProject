/***************************************************************************
                      utilsgeometry2D.h
                     --------------------------------------
Date                 : 10-05-2019
Copyright            : (C) 2018 by Vincent Cloarec
email                : vcloarec at gmail dot com / projetreos at gmail dot com
 ***************************************************************************
 *                                                                         *
 *   This program is free software; you can redistribute it and/or modify  *
 *   it under the terms of the GNU General Public License as published by  *
 *   the Free Software Foundation; either version 2 of the License, or     *
 *   (at your option) any later version.                                   *
 *                                                                         *
 ***************************************************************************/

#ifndef UTILSGEOMETRY2D_H
#define UTILSGEOMETRY2D_H

#include <QPointF>
#include <limits>
#include <math.h>

class intersect2D
{
  public:

    //source: http://www.faqs.org/faqs/graphics/algorithms-faq/

    intersect2D( QPointF S1p1, QPointF S1p2, QPointF S2p1, QPointF S2p2 );
    bool isIntersectSegmentExist();

    bool isIntersectLineExist();

    QPointF intersection();

    double placementSegment1();
    double placementSegment2();

  private:
    double denominateur;
    double numerateur_r;
    double numerateur_s;
    double r;
    double s;

    QPointF A;
    QPointF B;
    QPointF C;
    QPointF D;
};

#endif // UTILSGEOMETRY2D_H
