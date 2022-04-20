/***************************************************************************
  reoshydraulicstructure2d.h - ReosHydraulicStructure2D

 ---------------------
 begin                : 9.1.2022
 copyright            : (C) 2022 by Vincent Cloarec
 email                : vcloarec at gmail dot com
 ***************************************************************************
 *                                                                         *
 *   This program is free software; you can redistribute it and/or modify  *
 *   it under the terms of the GNU General Public License as published by  *
 *   the Free Software Foundation; either version 2 of the License, or     *
 *   (at your option) any later version.                                   *
 *                                                                         *
 ***************************************************************************/
#ifndef REOSHYDRAULICSTRUCTURE2D_H
#define REOSHYDRAULICSTRUCTURE2D_H

#include "reoshydraulicnetwork.h"
#include "reospolylinesstructure.h"
#include "reoshydraulicsimulation.h"
#include "reosgmshgenerator.h"
#include "reosmesh.h"
#include "reos3dmapsettings.h"
#include "reoshydraulicsimulationresults.h"

class ReosTopographyCollection;
class ReosRoughnessStructure;
class ReosHydraulicStructureBoundaryCondition;
class QDir;

class ReosHydraulicStructure2D : public ReosHydraulicNetworkElement
{
    Q_OBJECT
  public:
    struct BoundaryVertices
    {
      QVector<int> verticesIndex;
      QPointer<ReosHydraulicStructureBoundaryCondition> boundaryCondition;
    };

    //! Contructor from a \a domain, ccordinate system \a crs and a \a context
    ReosHydraulicStructure2D( const QPolygonF &domain, const QString &crs, const ReosHydraulicNetworkContext &context );

    //! Create a structure from \a encodedElement and a \a context
    static ReosHydraulicStructure2D *create( const ReosEncodedElement &encodedElement, const ReosHydraulicNetworkContext  &context );

    static QString staticType() {return ReosHydraulicNetworkElement::staticType() + QString( ':' ) + QStringLiteral( "structure2D" );}

    QString type() const override {return staticType();}
    virtual void saveConfiguration( ReosHydraulicScheme *scheme ) const override;
    virtual void restoreConfiguration( ReosHydraulicScheme *scheme ) override;

    void updateCalculationContextFromUpstream( const ReosCalculationContext &context, ReosHydraulicStructureBoundaryCondition *boundaryCondition, bool upstreamWillChange ) {}
    bool updateCalculationContextFromDownstream( const ReosCalculationContext &context ) {}

    //! Returns the domain polygon
    QPolygonF domain( const QString &crs = QString() ) const;

    //! Returns the geometrical strcuture
    ReosPolylinesStructure *geometryStructure() const;

    //! Returns the mesh resolution controler
    ReosMeshResolutionController *meshResolutionController() const;

    //! Returns the mesh
    ReosMesh *mesh() const;

    //! Returns the mesh generator
    ReosMeshGenerator *meshGenerator() const;

    //! Returns a process that generate the mesh, caller take ownership
    ReosMeshGeneratorProcess *getGenerateMeshProcess();

    //! Returns the boundary vertices of the structure
    QVector<BoundaryVertices> boundaryVertices() const;

    //! Returns the vertices of the holes classidfied per holes and per lines
    QVector<QVector<QVector<int> > > holesVertices() const;

    //! Returns the topography collection
    ReosTopographyCollection *topographyCollecion() const;

    //! Returns the rougness structure
    ReosRoughnessStructure *roughnessStructure() const;

    //! Returns all the boundary condition of this stucture
    QList<ReosHydraulicStructureBoundaryCondition *> boundaryConditions() const;

    //! Adds a new simulation with \a key corresponding to a engine and sets it the current one. Returns true if the simulation is effectivly added
    bool addSimulation( const QString key );

    //! Returns a pointer to the current simulation
    ReosHydraulicSimulation *currentSimulation() const;

    //! Returns the count of simulation
    int simulationCount() const {return mSimulations.count();}

    //! Returns the index of the current simulation
    int currentSimulationIndex() const;

    //! Sets the current simulation with its \a index
    void setCurrentSimulation( int index );

    //! Returns the name of all the simulations
    QStringList simulationNames() const;

    //! Returns a process that prepare the current simulation, caller take ownership
    ReosProcess *getPreparationProcessSimulation( const ReosCalculationContext &context );

    //! Starts the current simulation, return true if the calculation is effectivly started and returns a pointer to the process
    ReosSimulationProcess *startSimulation( const ReosCalculationContext &context );

    //! Returns  a pointer to the current simulation process
    ReosSimulationProcess *simulationProcess( const ReosCalculationContext &context ) const;

    //! Returns whether a simulation is running
    bool hasSimulationRunning() const;

    //! Returns whether a result corresponding to \a context is existing
    bool hasResult( const ReosCalculationContext &context );

    //! Returns whether a result corresponding to \a context is existing
    QDateTime resultDateTime( const ReosCalculationContext &context );

    //! Sets active the terrain in the mesh
    void activateMeshTerrain();

    //! Returns the id of the terrain dataset
    QString terrainMeshDatasetId() const;

    //! Deactivates any activated scalar dataset
    void deactivateMeshScalar();

    //! Returns the 3D map settings
    Reos3DMapSettings map3dSettings() const;

    //! Sets the 3D map settings
    void setMap3dSettings( const Reos3DMapSettings &value );

    //! Returns the 3D terrain settings
    Reos3DTerrainSettings terrain3DSettings() const;

    //! Sets the 3D terrain settings
    void setTerrain3DSettings( const Reos3DTerrainSettings &settings );

    //! Returns the directory where data and simulation will be stored on the disk
    QDir structureDirectory() const;

    //! Activates the result dataset groups with \a id. If id is void, the current group is reactivated
    void activateResultDatasetGroup( const QString &id = QString() );

    //! Activates the result dataset groups with type \a datasetType.
    void activateResultDatasetGroup( ReosHydraulicSimulationResults::DatasetType datasetType );

    //! Returns the all the dataset ids
    QStringList meshDatasetIds() const;

    //! Returns the name of the dataset with \a id
    QString meshDatasetName( const QString &id ) const;

    //! Returns the id of the current dataset
    QString currentActivatedMeshDataset() const;

    //! Returns the id of the current dataset
    ReosHydraulicSimulationResults::DatasetType currentActivatedDatasetResultType() const;

  public slots:
    void updateCalculationContext( const ReosCalculationContext &context ) override;

  signals:
    void meshGenerated();
    void boundaryChanged();
    void currentSimulationChanged();
    void simulationFinished();
    void simulationResultChanged();

  protected:
    void encodeData( ReosEncodedElement &element, const ReosHydraulicNetworkContext &context ) const override;

  private slots:
    void onBoundaryConditionAdded( const QString &bid );
    void onBoundaryConditionRemoved( const QString &bid );
    void onGeometryStructureChange();
    void onFlowsFromSolverReceived( const QDateTime &time, const QStringList &boundId, const QList<double> &values );
    void onSimulationFinished( ReosHydraulicSimulation *simulation,  const QString &schemeId );

  private:
    ReosHydraulicStructure2D( const ReosEncodedElement &encodedElement, const ReosHydraulicNetworkContext &context );

    ReosMeshGenerator *mMeshGenerator = nullptr;
    std::unique_ptr<ReosPolylinesStructure> mPolylinesStructures;
    ReosMeshResolutionController *mMeshResolutionController = nullptr;
    ReosTopographyCollection   *mTopographyCollection = nullptr;
    std::unique_ptr<ReosMesh> mMesh;
    std::unique_ptr<ReosRoughnessStructure > mRoughnessStructure;
    QVector<QVector<int>> mBoundaryVertices;
    QVector<QVector<QVector<int>>> mHolesVertices;

    QList<ReosHydraulicSimulation *> mSimulations;

    //** configuration
    int mCurrentSimulationIndex = -1;
    //**

    std::map<QString, std::unique_ptr<ReosSimulationProcess>> mSimulationProcesses;
    QMap<QString, ReosHydraulicSimulationResults *> mSimulationResults;

    typedef ReosHydraulicSimulationResults::DatasetType ResultType;
    mutable QMap<ResultType, QByteArray> mResultScalarDatasetSymbologies;
    mutable QByteArray mTerrainSymbology;

    QString mTerrainDatasetId;
    Reos3DMapSettings m3dMapSettings;
    Reos3DTerrainSettings m3dTerrainSettings;

    ReosHydraulicNetworkContext mHydraulicNetworkContext;

    void init();
    void generateMeshInPlace();
    QString directory() const;
    ReosHydraulicStructureBoundaryCondition *boundaryConditionNetWorkElement( const QString boundaryId ) const;
    void onMeshGenerated( const ReosMeshFrameData &meshData );

    void getSymbologiesFromMesh( const QString &schemeId ) const;

    void updateCurrentResults( const QString &schemeId );
    void loadResult( ReosHydraulicSimulation *simulation, const QString &schemeId );
    void setResultsOnMesh( ReosHydraulicSimulationResults *simResults );

    ReosSimulationProcess *processFromScheme( const QString &schemeId ) const;
};

class ReosHydraulicStructure2dFactory : public ReosHydraulicNetworkElementFactory
{
  public:
    ReosHydraulicStructure2dFactory() = default;
    ReosHydraulicNetworkElement *decodeElement( const ReosEncodedElement &encodedElement, const ReosHydraulicNetworkContext &context ) const override;
};


class ReosRoughnessStructure : public ReosDataObject
{
    Q_OBJECT
  public:
    ReosRoughnessStructure( const QString &mCrs );
    ReosRoughnessStructure( const ReosEncodedElement &encodedElement );

    ReosEncodedElement encode() const;
    ReosParameterDouble *defaultRoughness() const;
    ReosPolygonStructure *structure() const;

  private:
    std::unique_ptr<ReosPolygonStructure> mStructure;
    ReosParameterDouble *mDefaultRoughness = nullptr;
};


#endif // REOSHYDRAULICSTRUCTURE2D_H
