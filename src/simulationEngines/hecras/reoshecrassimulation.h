/***************************************************************************
  reoshecrassimulation.h - ReosHecRasSimulation

 ---------------------
 begin                : 06.10.2022
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
#ifndef REOSHECRASSIMULATION_H
#define REOSHECRASSIMULATION_H

#include "reoshydraulicsimulation.h"
#include "reosstructureimporter.h"
#include "reoshydraulicstructure2d.h"
#include "reoshecrasproject.h"

class ReosDssPath;
class ReosHecRasController;
class ReosHecRasStructureImporterSource;

class ReosHecRasSimulationProcess: public ReosSimulationProcess
{
    Q_OBJECT
  public:
    ReosHecRasSimulationProcess(
      const ReosHecRasProject &hecRasProject,
      const QString &planId,
      const ReosCalculationContext &context,
      const QList<ReosHydraulicStructureBoundaryCondition *> &boundaries );

    void start();

  private:
    QString mControllerVersion;
    ReosHecRasProject mProject;
    ReosHecRasPlan mPlan;
};

class ReosHecRasSimulation : public ReosHydraulicSimulation
{
    Q_OBJECT
  public:
    explicit ReosHecRasSimulation( ReosHydraulicStructure2D *parent = nullptr );
    explicit ReosHecRasSimulation( const ReosEncodedElement &element, ReosHydraulicStructure2D *parent = nullptr );

    static QString staticKey() { return QStringLiteral( "hecras" ); }

    QString key() const override;

    void prepareInput( ReosHydraulicStructure2D *hydraulicStructure, const ReosSimulationData &data, const ReosCalculationContext &calculationContext ) override;
    void prepareInput( ReosHydraulicStructure2D *, const ReosSimulationData &, const ReosCalculationContext &, const QDir & ) override {}

    ReosSimulationProcess *getProcess( ReosHydraulicStructure2D *hydraulicStructure, const ReosCalculationContext &calculationContext ) const override;
    ReosDuration representativeTimeStep() const override;
    ReosDuration representative2DTimeStep() const override;
    void saveSimulationResult( const ReosHydraulicStructure2D *hydraulicStructure, const QString &shemeId, ReosSimulationProcess *process, bool success ) const override;
    ReosHydraulicSimulationResults *loadSimulationResults( ReosHydraulicStructure2D *hydraulicStructure, const QString &schemeId, QObject *parent = nullptr ) const override;
    bool hasResult( const ReosHydraulicStructure2D *hydraulicStructure, const QString &shemeId ) const override;
    void removeResults( const ReosHydraulicStructure2D *hydraulicStructure, const QString &shemeId ) const override;
    QString engineName() const override {return tr( "HECRAS" );}
    ReosTimeWindow externalTimeWindow() const override;
    ReosTimeWindow externalBoundaryConditionTimeWindow( const QString &boundaryId ) const override;
    ReosHydraulicNetworkElementCompatibilty checkCompatiblity( ReosHydraulicScheme *scheme ) const override;

    void saveConfiguration( ReosHydraulicScheme *scheme ) const override;
    void restoreConfiguration( ReosHydraulicScheme *scheme ) override;

    ReosEncodedElement encode() const override;

    void setProject( std::shared_ptr<ReosHecRasProject> newProject );
    ReosHecRasProject *project() const;

    void setCurrentPlan( const QString &planId, ReosHydraulicScheme *scheme = nullptr );
    QString currentPlan( ReosHydraulicScheme *scheme = nullptr ) const;

    ReosDuration minimumInterval( ReosHydraulicScheme *scheme = nullptr ) const;
    void setMinimumInterval( const ReosDuration &newMinimumInterval, ReosHydraulicScheme *scheme = nullptr );

    ReosDuration computeInterval( ReosHydraulicScheme *scheme = nullptr ) const;
    void setComputeInterval( const ReosDuration &newComputeInterval, ReosHydraulicScheme *scheme = nullptr );

    ReosDuration outputInterval( ReosHydraulicScheme *scheme = nullptr ) const;
    void setOutputInterval( const ReosDuration &newOutputInterval, ReosHydraulicScheme *scheme = nullptr );

    ReosDuration detailedInterval( ReosHydraulicScheme *scheme = nullptr ) const;
    void setDetailledInterval( const ReosDuration &newDetailledInterval, ReosHydraulicScheme *scheme = nullptr );

    ReosDuration mappingInterval( ReosHydraulicScheme *scheme = nullptr ) const;
    void setMappingInterval( const ReosDuration &newMappingInterval, ReosHydraulicScheme *scheme = nullptr );

    static void updateBoundaryConditions( ReosHecRasProject *project,
                                          const QSet<QString> &currentBoundaryId,
                                          ReosHydraulicStructure2D *structure,
                                          const ReosHydraulicNetworkContext &context );

    /**
     * Checks the compatibility if the HEC-RAS plan with \a planId of this simulation
     * considering the complete hydraulic network (for all schemes)
     */
    ReosHydraulicNetworkElementCompatibilty checkPlanCompability(
      const QString &planId ) const;

  private:
    ReosHydraulicStructure2D *mStructure = nullptr;
    QString mProjectFileName;
    std::shared_ptr<ReosHecRasProject> mProject;

    //*** Configuration attributes
    QString mCurrentPlan;
    ReosDuration mMinimumInterval = ReosDuration( 1.0, ReosDuration::minute );
    ReosDuration mComputeInterval = ReosDuration( 10.0, ReosDuration::second );
    ReosDuration mOutputInterval = ReosDuration( 1.0, ReosDuration::minute );
    ReosDuration mDetailledInterval = ReosDuration( 5.0, ReosDuration::minute );
    ReosDuration mMappingInterval = ReosDuration( 5.0, ReosDuration::minute );
    //***

    static void transformVariableTimeStepToConstant( ReosTimeSeriesVariableTimeStep *variable, ReosTimeSeriesConstantInterval *constant, const ReosDuration &miniInter );
    bool writeDssConstantTimeSeries( ReosTimeSeriesConstantInterval *series, const QString &fileName, const ReosDssPath &path, QString &error ) const;

    void accordCurrentPlan();

    void setCurrentPlanForScheme( const QString &planId, ReosHydraulicScheme *scheme ) const;
    void setMinimumIntervalForScheme( const ReosDuration &newMinimumInterval, ReosHydraulicScheme *scheme ) const;
    void setComputeIntervalForScheme( const ReosDuration &newComputeInterval, ReosHydraulicScheme *scheme ) const;
    void setOutputIntervalForScheme( const ReosDuration &newOutputInterval, ReosHydraulicScheme *scheme ) const;
    void setDetailledIntervalForScheme( const ReosDuration &newDetailledInterval, ReosHydraulicScheme *scheme ) const;
    void setMappingIntervalForScheme( const ReosDuration &newMappingInterval, ReosHydraulicScheme *scheme ) const;
};


class ReosHecRasSimulationEngineFactory : public ReosSimulationEngineFactory
{
  public:
    ReosHecRasSimulationEngineFactory();

    //! Create a new simulation, for HEC-RAS model, it is not possible to create new one, there is only one simulation per model, so this method returns nullptr
    ReosHydraulicSimulation *createSimulation( ReosHydraulicStructure2D * ) const override
    { return nullptr; }

    //! Create a simulation from an encoded element.
    ReosHecRasSimulation *createSimulation( const ReosEncodedElement &element, ReosHydraulicStructure2D *parent ) const override;

    ReosStructureImporterSource *createImporterSource( const ReosEncodedElement &element, const ReosHydraulicNetworkContext &context ) const override;

    virtual QString key() const override { return ReosHecRasSimulation::staticKey(); }
    QString displayName() const override { return QObject::tr( "HEC-RAS Simulation" ); }

    void initializeSettings() override;
};

/**
 * Class that is used to iport HEC-RAS model. This importer is rely to a HEC-RAC project file (*.prj) and is responsible to provide all elements
 * necessary to the hydraulic structure (mesh, simulation instance, boundary condition,...)
 */
class ReosHecRasStructureImporter: public ReosStructureImporter
{
  public:

    struct CreationOptions
    {
      bool createSchemeWithPlan = false;
      bool removePreviousScheme = false;
    };

    ReosHecRasStructureImporter( const QString &file, const ReosHydraulicNetworkContext &context, const ReosHecRasStructureImporterSource *source );
    ~ReosHecRasStructureImporter();

    QString importerKey() const override;

    ReosHydraulicStructure2D::Structure2DCapabilities capabilities() const override;
    QString crs() const override;
    QPolygonF domain() const override;
    ReosMesh *mesh( const QString &destinationCrs ) const override;
    ReosMesh *mesh( ReosHydraulicStructure2D *structure, ReosHydraulicScheme *scheme, const QString &destinationCrs ) const override;

    QList<ReosHydraulicStructureBoundaryCondition *> createBoundaryConditions( ReosHydraulicStructure2D *structure, const ReosHydraulicNetworkContext &context ) const override;

    QList<ReosHydraulicSimulation *> createSimulations( ReosHydraulicStructure2D *parent ) const override;

    void updateBoundaryConditions( const QSet<QString> &currentBoundaryId,
                                   ReosHydraulicStructure2D *structure,
                                   const ReosHydraulicNetworkContext &context ) const override;

    bool isValid() const override;

    bool projectFileExists() const;

    int planCount() const;

    const ReosStructureImporterSource *source() const override;

    void setCreationOption( const CreationOptions &newCreationOption );

  private:
    const ReosHecRasStructureImporterSource *mSource = nullptr;
    bool mIsValid = false;
    std::shared_ptr<ReosHecRasProject> mProject;
    QString mCrs;
    CreationOptions mCreationOption;

    void init( const QString &mFileName );
};

class ReosHecRasStructureImporterSource: public ReosStructureImporterSource
{
  public:
    ReosHecRasStructureImporterSource( const QString &file, const ReosHydraulicNetworkContext &context );
    ReosHecRasStructureImporterSource( const ReosEncodedElement &element, const ReosHydraulicNetworkContext &context );

    ReosHecRasStructureImporterSource *clone() const override;
    ReosEncodedElement encode( const ReosHydraulicNetworkContext &context ) const override;
    ReosHecRasStructureImporter *createImporter() const override;

  private:
    QString mHecRasProjectFile;
    QPointer<ReosHydraulicNetwork> mNetwork;
};


#endif //REOSHECRASSIMULATION_H
