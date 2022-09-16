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
class ReosHydraulicScheme;

class ReosHydraulicStructureProfile : public ReosDataObject
{
  public:
    ReosHydraulicStructureProfile( const QString &name, const QPolygonF &geometry, ReosHydraulicStructure2D *structure );
    ReosHydraulicStructureProfile( const ReosEncodedElement &element, ReosHydraulicStructure2D *structure );

    /**
     *  Returns the different parts of the geometry intersecting the domain,
     *  parts are sorted following the distance from beginning of the line
     */
    QMap<double, QPolygonF> parts() const;

    //! Returns the point values sorted of each parts
    QMap<double, QList<ReosMeshPointValue>> pointValues() const;

    //! Returns the geometry of the profile in the plan in the associate structure coordinate
    const QPolygonF &geometry() const;

    //! Changes the ceometry in the plan with \a geom that is in \a lineCrs coordinate system
    void changeGeometry( const QPolygonF &geom, const QString &linesCrs );

    //! Returns the terrain profile
    QPolygonF terrainProfile() const;

    //! Returns the results profile corresponding to hydraulic \a scheme, for \a time and with type \a result type
    QPolygonF resultsProfile( ReosHydraulicScheme *scheme, const QDateTime &time, ReosHydraulicSimulationResults::DatasetType resultType ) const;

    ReosEncodedElement encode() const;

  private:
    QPolygonF mGeometry;
    ReosHydraulicStructure2D *mStructure = nullptr;
    mutable QMap<double, QPolygonF> mParts;
    mutable QMap<double, QList<ReosMeshPointValue>> mPointValues;

    void initParts() const;
    void buildProfile() const;

    QPolygonF extractValue( std::function<double( ReosMeshPointValue )> &func ) const;
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
    void removeProfile( int index );
    void renameProfile( int profileIndex, const QString &name );

    ReosHydraulicStructureProfile *profile( int profileIndex );

    ReosEncodedElement encode() const;
    void decode( const ReosEncodedElement &element, ReosHydraulicStructure2D *structure );

  private:
    QList<ReosHydraulicStructureProfile *> mProfiles;
};

#endif // REOSHYDRAULICSTRUCTUREPROFILE_H
