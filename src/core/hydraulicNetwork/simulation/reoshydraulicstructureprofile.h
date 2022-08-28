#ifndef REOSHYDRAULICSTRUCTUREPROFILE_H
#define REOSHYDRAULICSTRUCTUREPROFILE_H

#include <QObject>
#include <QPolygonF>
#include <QMap>

class ReosHydraulicStructure2D;

class ReosHydraulicStructureProfile : public QObject
{
  public:
    ReosHydraulicStructureProfile( const QPolygonF &geometry, ReosHydraulicStructure2D *structure );

    QMap<double, QPolygonF> parts() const;

  private:
    QPolygonF mGeometry;
    ReosHydraulicStructure2D *mStructure = nullptr;
    mutable QMap<double, QPolygonF> mParts;

    void initParts() const;
};

#endif // REOSHYDRAULICSTRUCTUREPROFILE_H
