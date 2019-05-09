#ifndef UTILSGEOMETRY2D_H
#define UTILSGEOMETRY2D_H

#include <QPointF>
#include <limits>
#include <math.h>

class intersect2D
{
public:

    //source: http://www.faqs.org/faqs/graphics/algorithms-faq/

    intersect2D(QPointF S1p1, QPointF S1p2, QPointF S2p1, QPointF S2p2);
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
