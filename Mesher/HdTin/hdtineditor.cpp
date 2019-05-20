#include "hdtineditor.h"

TINEditor::TINEditor(HdTin &tin, std::vector<Segment> &segments):
    mTin(tin),mSegments(segments)
{
}

VertexPointer TINEditor::addVertex(double x, double y)
{
    VertexPointer vert=mTin.vertex(x,y,tolerance());
    if (vert)
    {
        return vert;
    }
    else {
        return mTin.addVertex(x,y);
    }
}

VertexPointer TINEditor::vertex(double x, double y) const
{
    return mTin.vertex(x,y,tolerance());
}

int TINEditor::facesCount() const
{
    return mTin.facesCount();
}

int TINEditor::verticesCount() const {
    return mTin.verticesCount();
}

int TINEditor::segmentsCount() const
{
    return int(mSegments.size());
}

double TINEditor::tolerance() const
{
    return mTolerance;
}

void TINEditor::setTolerance(double tolerance)
{
    mTolerance = tolerance;
}

std::unique_ptr<MeshIO> TINEditor::getTinReader() const {return mTin.getReader();}

#include "hdtineditor.h"
