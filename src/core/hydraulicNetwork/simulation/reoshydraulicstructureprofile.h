#ifndef REOSHYDRAULICSTRUCTUREPROFILE_H
#define REOSHYDRAULICSTRUCTUREPROFILE_H

#include <QObject>
#include <QPolygonF>
#include <QMap>
#include <QAbstractListModel>

#include "reosdataobject.h"
#include "reoshydraulicsimulationresults.h"
#include "reosmesh.h"

class ReosHydraulicStructure2D;

class ReosHydraulicStructureProfile : public ReosDataObject
{
  public:
    ReosHydraulicStructureProfile( const QString &name, const QPolygonF &geometry, ReosHydraulicStructure2D *structure );

    QMap<double, QPolygonF> parts() const;
    QVector<QPolygonF> terrainElevationProfile();

    QMap<double, QList<ReosMeshPointValue>> pointValues() const;
  private:
    QPolygonF mGeometry;
    ReosHydraulicStructure2D *mStructure = nullptr;
    mutable QMap<double, QPolygonF> mParts;
    mutable QMap<double, QList<ReosMeshPointValue>> mPointValues;

    void initParts() const;
    void buildProfile() const;
};

class ReosHydraulicStructureProfilesCollection : public QAbstractListModel
{
  public:
    ReosHydraulicStructureProfilesCollection( QObject *parent );

    QModelIndex index( int row, int column, const QModelIndex &parent ) const override;
    QModelIndex parent( const QModelIndex &child ) const override;
    int rowCount( const QModelIndex &parent ) const override;
    int columnCount( const QModelIndex &parent ) const override;
    QVariant data( const QModelIndex &index, int role ) const override;

    void addProfile( ReosHydraulicStructureProfile *profile );

    ReosHydraulicStructureProfile *profile( int profileIndex );

  private:
    QList<ReosHydraulicStructureProfile *> mProfiles;
};

//class ReosHydraulicStructureInterpolation
//{
//  public:
//    double getTerrainValue( ReosHydraulicStructure2D *structure ) const = 0;
//    double getValue( ReosHydraulicSimulationResults *result, ReosHydraulicSimulationResults::DatasetType datasetType, int index ) const;
//};


//class ReosHydraulicStructureEdgeInterpolation: public ReosMeshIntersectionOnEdge
//{
//  public:

//    ReosHydraulicStructureEdgeInterpolation( int vertexIndex1, int vertexIndex2, double posInEdge, double posOnPolyline )
//      : ReosMeshIntersectionOnEdge( vertexIndex1, vertexIndex2, posInEdge, posOnPolyline )
//    {}

//    double getTerrainValue( ReosHydraulicStructure2D *structure ) const;
//    double getValue( ReosHydraulicSimulationResults *result, ReosHydraulicSimulationResults::DatasetType datasetType, int index ) const;

//};

//class ReosHydraulicStructureFaceInterpolation: public ReosMeshIntersectionOnEdge
//{
//  public:

//    ReosHydraulicStructureEdgeInterpolation( int vertexIndex1, int vertexIndex2, double posInEdge, double posOnPolyline )
//      : ReosMeshIntersectionOnEdge( vertexIndex1, vertexIndex2, posInEdge, posOnPolyline )
//    {}

//    double getTerrainValue( ReosHydraulicStructure2D *structure ) const;
//    double getValue( ReosHydraulicSimulationResults *result, ReosHydraulicSimulationResults::DatasetType datasetType, int index ) const;

//};



//class  ReosHydraulicStructureIntepolationFactory: ReosMeshIntersectionFactory
//{
//  public:
//    virtual ReosMeshPointValue *createEdgeIntersection( int vertexIndex1, int vertexIndex2, double posInEdge, double posOnPolyline )
//    {
//      return new ReosHydraulicStructureEdgeInterpolation( vertexIndex1, vertexIndex2, posInEdge, posOnPolyline );
//    }
//    virtual ReosMeshPointValue *createFaceIntersection(
//      int vertexIndex1, int vertexIndex2, int vertexIndex3,
//      double lam1, double lam2, double lam3,
//      double posOnPolyline );


//};

#endif // REOSHYDRAULICSTRUCTUREPROFILE_H
