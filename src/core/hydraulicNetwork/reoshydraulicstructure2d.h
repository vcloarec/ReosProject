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

#include "reoscore.h"

#include "reoshydraulicnetwork.h"
#include "reospolylinesstructure.h"
#include "reoshydraulicsimulation.h"
#include "reosgmshgenerator.h"
#include "reosmesh.h"
#include "reos3dmapsettings.h"
#include "reoshydraulicsimulationresults.h"
#include "reoshydraulicstructureprofile.h"

class ReosTopographyCollection;
class ReosRoughnessStructure;
class ReosHydraulicStructureBoundaryCondition;
class QDir;

class REOSCORE_EXPORT ReosHydraulicStructure2D : public ReosHydraulicNetworkElement
{
    Q_OBJECT
  public:
    struct BoundaryVertices
    {
      QVector<int> verticesIndex;
      QPointer<ReosHydraulicStructureBoundaryCondition> boundaryCondition;
    };

    enum Structure2DCapability
    {
      GeometryEditable = 1 << 0, //!< If the structure have geometry editable (geometry structure, mesh, ...)
      MultiSimulation = 1 << 1, //!< If the structure can have multiple simulations
    };

    Q_ENUM( Structure2DCapability )
    Q_DECLARE_FLAGS( Structure2DCapabilities, Structure2DCapability )
    Q_FLAG( Structure2DCapabilities )

    //! Contructor from a \a domain, ccordinate system \a crs and a \a context
    ReosHydraulicStructure2D( const QPolygonF &domain, const QString &crs, const ReosHydraulicNetworkContext &context );

    //! Create a structure from \a encodedElement and a \a context
    static ReosHydraulicStructure2D *create( const ReosEncodedElement &encodedElement, const ReosHydraulicNetworkContext  &context );

    static QString staticType() {return ReosHydraulicNetworkElement::staticType() + QString( ':' ) + QStringLiteral( "structure2D" );}

    QString type() const override {return staticType();}
    void saveConfiguration( ReosHydraulicScheme *scheme ) const override;
    void restoreConfiguration( ReosHydraulicScheme *scheme ) override;
    ReosMapExtent extent() const override;

    ReosDuration currentElementTimeStep() const override;

    void updateCalculationContextFromUpstream( const ReosCalculationContext &context, ReosHydraulicStructureBoundaryCondition *boundaryCondition, bool upstreamWillChange ) {}
    bool updateCalculationContextFromDownstream( const ReosCalculationContext &context ) { return false; }

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

    //! Remove the simulation with \a index
    void removeSimulation( int index );

    //! Returns a pointer to the simulation associated with \a scheme
    ReosHydraulicSimulation *simulation( ReosHydraulicScheme *scheme ) const;

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
    ReosSimulationPreparationProcess *getPreparationProcessSimulation( const ReosCalculationContext &context, QString &error );

    //! Returns a process that prepare the current simulation files in a specific \a diectory, caller take ownership
    ReosSimulationPreparationProcess *getPreparationProcessSimulation( const ReosCalculationContext &context, QString &error, const QDir &directory );

    //! Creates a new simulation process and returns a pointer to the process, the process is not starting
    ReosSimulationProcess *createSimulationProcess( const ReosCalculationContext &context, QString &error );

    //! Returns  a pointer to the current simulation process
    ReosSimulationProcess *simulationProcess( const ReosCalculationContext &context ) const;

    //! Returns whether a simulation is running
    bool hasSimulationRunning() const;

    void updateResults( const QString &schemeId );

    //! Returns whether the structure contain any results
    bool hasResults() const;

    //! Returns whether a result corresponding to \a context is existing (loaded)
    bool hasResults( const QString &schemeId ) const;

    //! Returns whether a result corresponding to \a scheme is existing (loaded)
    bool hasResults( const ReosHydraulicScheme *scheme ) const;

    //! Returns whether a results corresponding to \a context is existing
    QDateTime resultsRunDateTime( const QString &schemeId ) const;

    //! Returns the time step count of the results corresponding to \a context
    int resultsTimeStepCount( const QString &schemeId ) const;

    //! Returns the value of the results with type \a datasetType for the specified \a context, \a position and \a time
    double resultsValueAt( const QDateTime &time,
                           const ReosSpatialPosition &position,
                           ReosHydraulicSimulationResults::DatasetType datasetType,
                           const QString &schemeId );

    //! Returns a translated string corresponding to the unit of the results associated with \a context and to the type  \a datasetType
    QString resultsUnits( ReosHydraulicSimulationResults::DatasetType datasetType, const QString &schemeId );

    //! Remove all results in the structure
    void removeAllResults();

    //! Remove the result associated with \a context
    void removeResults( const ReosCalculationContext &context );

    //! Returns the results associated with \a scheme
    ReosHydraulicSimulationResults *results( ReosHydraulicScheme *scheme );

    //! Sets active the terrain in the mesh
    void activateMeshTerrain();

    //! Returns the id of the terrain dataset
    QString terrainMeshDatasetId() const;

    //! Returns the elevation of the terran at \a position in map coordinates
    double terrainElevationAt( const QPointF &position );

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

    //! Activates the result vector dataset groups with \a id. If id is void, the current group is reactivated
    void activateResultVectorDatasetGroup( const QString &id );

    //! Returns the all the dataset ids
    QStringList meshDatasetIds() const;

    //! Returns the all the vector dataset ids
    QStringList meshVectorDatasetIds() const;

    //! Returns the name of the dataset with \a id
    QString meshDatasetName( const QString &id ) const;

    //! Returns the id of the current dataset
    QString currentActivatedMeshDataset() const;

    //! Returns the id of the current vector dataset
    QString currentActivatedVectorMeshDataset() const;

    //! Returns the type of the current dataset
    ReosHydraulicSimulationResults::DatasetType currentActivatedDatasetResultType() const;

    //! Return the name of the current dataset
    QString currentDatasetName() const;

    void exportResultAsMesh( const QString &fileName ) const;

    void exportResultAsMeshInGisProject( const QString &fileName, bool keepLayers );

    //! Returns a pointer to the profile collection
    ReosHydraulicStructureProfilesCollection *profilesCollection() const;

    //! Creates a profile with \a name, a geometry in the plan \a linesInPlan that has \a lineCrs as coordinate system
    int createProfile( const QString &name, const QPolygonF &linesInPlan, const QString &linesCrs );

    //! Removes the profile with \a index
    void removeProfile( int index );

    //! Renames the profile with \a index with \a name
    void renameProfile( int index, const QString &name );

    //! Return the count of profile
    int profilesCount() const;

    //! Returns the profile with \a profileIndex
    ReosHydraulicStructureProfile *profile( int profileIndex ) const;

    //! Returns the index of the \a profile
    int profileIndex( ReosHydraulicStructureProfile *profile );

    //! Returns whether the structure supports the \a capability
    bool hasCapability( Structure2DCapability capability ) const;

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
    void onSimulationFinished( ReosHydraulicSimulation *simulation,  const QString &schemeId, ReosSimulationProcess *process, bool success );

  private:
    ReosHydraulicStructure2D( const ReosEncodedElement &encodedElement, const ReosHydraulicNetworkContext &context );

    Structure2DCapabilities mCapabilities;
    ReosMeshGenerator *mMeshGenerator = nullptr;
    std::unique_ptr<ReosPolylinesStructure> mPolylinesStructures;
    ReosMeshResolutionController *mMeshResolutionController = nullptr;
    ReosTopographyCollection   *mTopographyCollection = nullptr;
    std::unique_ptr<ReosMesh> mMesh;
    std::unique_ptr<ReosRoughnessStructure > mRoughnessStructure;
    QVector<QVector<int>> mBoundaryVertices;
    QVector<QVector<QVector<int>>> mHolesVertices;
    bool mMeshNeedToBeGenerated = true;

    QList<ReosHydraulicSimulation *> mSimulations;

    ReosHydraulicStructureProfilesCollection *mProfilesCollection = nullptr;

    //** configuration
    int mCurrentSimulationIndex = -1;
    //**

    std::map<QString, std::unique_ptr<ReosSimulationProcess>> mSimulationProcesses;
    QMap<QString, ReosHydraulicSimulationResults *> mSimulationResults;

    Reos3DMapSettings m3dMapSettings;
    Reos3DTerrainSettings m3dTerrainSettings;

    void init();
    void generateMeshInPlace();
    QString directory() const;
    ReosHydraulicStructureBoundaryCondition *boundaryConditionNetWorkElement( const QString boundaryId ) const;
    void onMeshGenerated( const ReosMeshFrameData &meshData );

    void loadResult( ReosHydraulicSimulation *simulation, const QString &schemeId );
    void setResultsOnStructure( ReosHydraulicSimulationResults *simResults );

    ReosSimulationProcess *processFromScheme( const QString &schemeId ) const;

    int simulationIndexFromId( const QString &simId ) const;
};

class ReosHydraulicStructure2dFactory : public ReosHydraulicNetworkElementFactory
{
  public:
    ReosHydraulicStructure2dFactory() = default;
    ReosHydraulicNetworkElement *decodeElement( const ReosEncodedElement &encodedElement, const ReosHydraulicNetworkContext &context ) const override;
};


class REOSCORE_EXPORT ReosRoughnessStructure : public ReosDataObject
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
