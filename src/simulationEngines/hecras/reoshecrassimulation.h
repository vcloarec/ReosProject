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
#include "reoshydraulicstructure2d.h"
#include "reoshecrasproject.h"

class ReosDssPath;
class ReosHecRasController;

class ReosHecRasSimulationProcess: public ReosSimulationProcess
{
  public:
    ReosHecRasSimulationProcess(
      const ReosHecRasProject &hecRasProject,
      const QString &planId,
      const ReosCalculationContext &context,
      const QList<ReosHydraulicStructureBoundaryCondition *> boundaries );

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
    ReosHecRasSimulation( QObject *parent = nullptr );
    ReosHecRasSimulation( const ReosEncodedElement &element, QObject *parent = nullptr );

    static QString staticKey() { return QStringLiteral( "hecras" ); }

    QString key() const override;

    void prepareInput( ReosHydraulicStructure2D *hydraulicStructure, const ReosCalculationContext &calculationContext ) override;
    void prepareInput( ReosHydraulicStructure2D *hydraulicStructure, const ReosCalculationContext &calculationContext, const QDir &directory ) {}

    ReosSimulationProcess *getProcess( ReosHydraulicStructure2D *hydraulicStructure, const ReosCalculationContext &calculationContext ) const override;
    QList<QDateTime> theoricalTimeSteps( ReosHydraulicScheme *scheme ) const {return QList<QDateTime>();}
    ReosDuration representativeTimeStep() const {return ReosDuration();}
    ReosDuration representative2DTimeStep() const {return ReosDuration();}
    void saveSimulationResult( const ReosHydraulicStructure2D *hydraulicStructure, const QString &shemeId, ReosSimulationProcess *process, bool success ) const override;
    ReosHydraulicSimulationResults *loadSimulationResults( ReosHydraulicStructure2D *hydraulicStructure, const QString &shemeId, QObject *parent ) const;
    bool hasResult( const ReosHydraulicStructure2D *hydraulicStructure, const QString &shemeId ) const override;
    void removeResults( const ReosHydraulicStructure2D *hydraulicStructure, const QString &shemeId ) const override;
    QString engineName() const override {return tr( "HECRAS" );}
    void saveConfiguration( ReosHydraulicScheme *scheme ) const override;
    void restoreConfiguration( ReosHydraulicScheme *scheme ) override;

    ReosEncodedElement encode() const override;

    void setProject( std::shared_ptr<ReosHecRasProject> newProject );
    void setCurrentPlan( const QString &planId );
    ReosHecRasProject *project() const;

    const QString &currentPlan() const;

  private:
    QString mProjectFileName;
    std::shared_ptr<ReosHecRasProject> mProject;

    //*** Configuration attributes
    QString mCurrentPlan;
    ReosParameterDuration *mMinimumInterval = nullptr;
    //***

    void transformVariableTimeStepToConstant( ReosTimeSerieVariableTimeStep *variable, ReosTimeSerieConstantInterval *constant ) const;
    bool writeDssConstantTimeSeries( ReosTimeSerieConstantInterval *series, const QString fileName, const ReosDssPath &path, QString &error ) const;

    void accordCurrentPlan();
};


class ReosHecRasSimulationEngineFactory : public ReosSimulationEngineFactory
{
  public:
    ReosHecRasSimulationEngineFactory();

    virtual ReosHydraulicSimulation *createSimulation( QObject * ) const override
    { return nullptr; }
    virtual ReosHydraulicSimulation *createSimulation( const ReosEncodedElement &element, QObject *parent ) const override;

    ReosStructureImporter *createImporter( const ReosEncodedElement &element, const ReosHydraulicNetworkContext &context ) const override;

    virtual QString key() const override { return ReosHecRasSimulation::staticKey(); }
    QString displayName() const override { return QObject::tr( "HEC-RAS Simulation" ); }

    void initializeSettings() override;
};

class ReosHecRasStructureImporter: public ReosStructureImporter
{
  public:
    ReosHecRasStructureImporter( const QString &file );
    ReosHecRasStructureImporter( const ReosEncodedElement &element, const ReosHydraulicNetworkContext &context );
    ~ReosHecRasStructureImporter();

    QString importerKey() const override;

    ReosHydraulicStructure2D::Structure2DCapabilities capabilities() const override;
    QString crs() const override;
    QPolygonF domain() const override;
    ReosMesh *mesh( const QString &destinationCrs ) const override;


    QList<ReosHydraulicStructureBoundaryCondition *> createBoundaryConditions( ReosHydraulicStructure2D *structure, const ReosHydraulicNetworkContext &context ) const override;
    QList<ReosHydraulicSimulation *> createSimulations( QObject *parent ) const override;
    void updateBoundaryConditions( QSet<QString> &currentBoundaryId,
                                   ReosHydraulicStructure2D *structure,
                                   const ReosHydraulicNetworkContext &context ) const override;

    bool isValid() const override;

    ReosEncodedElement encode( const ReosHydraulicNetworkContext &context ) const override;

    bool projectFileExists() const;

  private:
    bool mIsValid = false;
    std::shared_ptr<ReosHecRasProject> mProject;
    QString mCrs;

    void init( const QString &mFileName );
};


#endif //REOSHECRASSIMULATION_H
