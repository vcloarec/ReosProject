#ifndef HDTINEDITOR_H
#define HDTINEDITOR_H

#include "hdtin.h"

class TINEditor
{
public:
    TINEditor(HdTin &tin,std::vector<Segment> &segments);

    VertexPointer addVertex(double x, double y);

    VertexPointer vertex(double x, double y) const;


    int facesCount() const;
    int verticesCount() const;
    int segmentsCount() const;

    double tolerance() const;
    void setTolerance(double tolerance);

    std::unique_ptr<MeshIO> getTinReader() const;

private:
    HdTin &mTin;
    std::vector<Segment> &mSegments;

    double mTolerance=0.01;

};


#endif // HDTINEDITOR_H
