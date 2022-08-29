#ifndef REOSHYDRAULICSTRUCTUREPROFILE_H
#define REOSHYDRAULICSTRUCTUREPROFILE_H

#include <QObject>
#include <QPolygonF>
#include <QMap>
#include <QAbstractListModel>

#include "reosdataobject.h"

class ReosHydraulicStructure2D;
class ReosHydraulicSimulationResults;

class ReosHydraulicStructureProfile : public ReosDataObject
{
  public:
    ReosHydraulicStructureProfile( const QString &name, const QPolygonF &geometry, ReosHydraulicStructure2D *structure );

    QMap<double, QPolygonF> parts() const;

  private:
    QPolygonF mGeometry;
    ReosHydraulicStructure2D *mStructure = nullptr;
    mutable QMap<double, QPolygonF> mParts;

    void initParts() const;
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



class ReosHydraulicStructureProfileValue
{
  public:
    virtual double getValue( ReosHydraulicSimulationResults *result ) = 0;
};

class ReosHydraulicStructureProfileValueEdgeInterpolation: public ReosHydraulicStructureProfileValue
{
  public:

};

#endif // REOSHYDRAULICSTRUCTUREPROFILE_H
