/***************************************************************************
                      utilsgeometry2D.cpp
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

#include "utilsgeometry2d.h"


intersect2D::intersect2D(QPointF S1p1, QPointF S1p2, QPointF S2p1, QPointF S2p2):
    A(S1p1),B(S1p2),C(S2p1),D(S2p2)
{
    denominateur=(S1p2.x()-S1p1.x())*(S2p2.y()-S2p1.y())-(S1p2.y()-S1p1.y())*(S2p2.x()-S2p1.x());
    numerateur_r=(S1p1.y()-S2p1.y())*(S2p2.x()-S2p1.x())-(S1p1.x()-S2p1.x())*(S2p2.y()-S2p1.y());
    numerateur_s=(S1p1.y()-S2p1.y())*(S1p2.x()-S1p1.x())-(S1p1.x()-S2p1.x())*(S1p2.y()-S1p1.y());

    if (fabs(denominateur)>std::numeric_limits<double>::epsilon())
    {
        r=(numerateur_r/denominateur);
        s=(numerateur_s/denominateur);
    }


}

bool intersect2D::isIntersectSegmentExist(){
    if (isIntersectLineExist())
    {
        return ((s>=0)&&(s<=1)&&(r>=0)&&(r<=1));
    }
    return false;
}

bool intersect2D::isIntersectLineExist(){
    return (fabs(denominateur)>std::numeric_limits<double>::epsilon());
}

QPointF intersect2D::intersection()
{
    if (isIntersectLineExist())
    {
        return QPointF(A.x()+r*(B.x()-A.x()),A.y()+r*(B.y()-A.y()));
    }

    return QPointF();
}

double intersect2D::placementSegment1() {return r;}

double intersect2D::placementSegment2() {return s;}
