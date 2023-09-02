/***************************************************************************
  reostelemac2dsimulation.h - ReosTelemac2DSimulation

 ---------------------
 begin                : 31.3.2022
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
#ifndef REOSTELEMAC2DSIMULATION_H
#define REOSTELEMAC2DSIMULATION_H

#include <QRegularExpression>

#include "reoshydraulicsimulation.h"
#include "reoshydraulicstructureboundarycondition.h"
#include "reostelemac2dinitialcondition.h"
#include "reoscore.h"
#include "reosduration.h"

class ReosHydraulicSimulationResults;
class QgsMeshDatasetGroup;

class ReosTelemac2DSimulation : public ReosHydraulicSimulation
{
    Q_OBJECT
  public:
    enum class Equation
    {
      FiniteVolume,
      FiniteElement
    };

    enum class VolumeFiniteScheme
    {
      Roe,
      Kinetic,
      Zokagoa,
      Tchamen,
      HLLC,
      WAF
    };

    struct TelemacBoundaryCondition
    {
      int rank = -1;
      QString header;
      QString unit;
      ReosTimeSeriesVariableTimeStep *timeSeries = nullptr;
      QString boundaryId;
      ReosHydraulicStructureBoundaryCondition::Type type;

    };

    ReosTelemac2DSimulation( ReosHydraulicStructure2D *parent = nullptr );
    explicit ReosTelemac2DSimulation( const ReosEncodedElement &element, ReosHydraulicStructure2D *parent = nullptr );
    static QString staticKey() {return QStringLiteral( "telemac2D" );}

    QString key() const override {return ReosTelemac2DSimulation::staticKey();}
    bool hasCapability( Capability cap ) const override;
    ReosModule::Message prepareSimulationData( ReosSimulationData &simData, const QString &schemeId ) override;
    void prepareInput( const ReosSimulationData &simulationData, const ReosCalculationContext &calculationContext ) override;
    void prepareInput( const ReosSimulationData &simulationData, const ReosCalculationContext &calculationContext, const QDir &directory ) override;
    ReosSimulationProcess *getProcess( const ReosCalculationContext &calculationContext ) const override;
    ReosDuration representativeTimeStep() const override;
    ReosDuration representative2DTimeStep() const override;
    bool hasResult( const QString &shemeId ) const override;
    void saveSimulationResult( const QString &shemeId, ReosSimulationProcess *process, bool success ) const override;
    ReosHydraulicSimulationResults *loadSimulationResults( const QString &shemeId, QObject *parent = nullptr ) const override;
    void removeResults( const QString &shemeId ) const override;
    ReosTimeWindow externalTimeWindow() const override {return ReosTimeWindow();}
    ReosTimeWindow externalBoundaryConditionTimeWindow( const QString & ) const override {return ReosTimeWindow();}

    void saveConfiguration( ReosHydraulicScheme *scheme ) const override;
    void restoreConfiguration( ReosHydraulicScheme *scheme ) override;
    QFileInfoList cleanScheme( ReosHydraulicScheme *scheme ) override;

    ReosEncodedElement encode() const override;

    //***** Configuration parameters

    void setHotStartSchemeId( const QString &schemeId ) override;
    void setHotStartTimeStepIndex( int index ) override;
    void setHotStartUseLastTimeStep( bool b ) override;

    ReosParameterDuration *timeStep() const;
    ReosParameterInteger *outputPeriodResult2D() const;
    ReosParameterInteger *outputPeriodResultHydrograph() const;

    void setInitialCondition( ReosTelemac2DInitialCondition::Type type );
    ReosTelemac2DInitialCondition *initialCondition() const;

    Equation equation() const;
    void setEquation( const Equation &equation );

    VolumeFiniteScheme volumeFiniteScheme( ReosHydraulicScheme *scheme = nullptr ) const;
    void setVolumeFiniteEquation( VolumeFiniteScheme VFscheme, ReosHydraulicScheme *hydraulicScheme = nullptr );
    ReosParameterDouble *courantNumber() const;
    //***********

    QString engineName() const override;

  protected:
    QString directoryName() const override {return  QStringLiteral( "TELEMAC" );}

  private:
    //****** config
    ReosParameterDuration *mTimeStep = nullptr;
    ReosParameterInteger *mOutputPeriodResult2D = nullptr;
    ReosParameterInteger *mOutputPeriodResultHyd = nullptr;
    Equation mEquation = Equation::FiniteVolume;
    VolumeFiniteScheme mVFScheme = VolumeFiniteScheme::HLLC;
    ReosParameterDouble *mVfCourantNumber = nullptr;
    int mCurrentInitialCondition = 0;
    //*********
    QList<ReosTelemac2DInitialCondition *> mInitialConditions;

    QList<TelemacBoundaryCondition> mBoundaries;

    QString mGeomFileName = QStringLiteral( "geom_input.slf" );
    QString mResultFileName = QStringLiteral( "results.slf" );
    QString mInitialConditionFile = QStringLiteral( "initial_condition.slf" );
    QString mBoundaryFileName = QStringLiteral( "boundary.cli" );
    QString mBoundaryConditionFileName = QStringLiteral( "boundaryCondition.liq" );
    QString mSteeringFileName = QStringLiteral( "simulation.cas" );

    Capabilities mCapabilities;

    ReosDuration timeStepValueFromScheme( ReosHydraulicScheme *scheme ) const;

    QList<ReosHydraulicStructureBoundaryCondition *> createBoundaryFiles(
      const ReosSimulationData &simulationData,
      QVector<int> &verticesPosInBoundary,
      const QDir &directory );

    void createSelafinMeshFrame( const QVector<int> &verticesPosInBoundary,
                                 const QString &fileName );

    void createSelafinBaseFile( const ReosSimulationData &simulationData,
                                const QVector<int> &verticesPosInBoundary,
                                const QString &fileName );

    void createSelafinInitialConditionFile( const ReosSimulationData &simulationData,
                                            const QVector<int> &verticesPosInBoundary,
                                            const QDir &directory );

    QList<TelemacBoundaryCondition> createBoundaryConditionFiles(
      const QList<ReosHydraulicStructureBoundaryCondition *> &boundaryConditions,
      const ReosCalculationContext &context,
      const QDir &directory );

    void createSteeringFile( const ReosSimulationData &simulationData,
                             const QList<ReosHydraulicStructureBoundaryCondition *> &boundaryConditions,
                             const QVector<int> &verticesPosInBoundary,
                             const ReosCalculationContext &context,
                             const QDir &directory );

    void init();
    void initInitialCondition();

    void setVolumeFiniteEquationForScheme( VolumeFiniteScheme VFscheme, ReosHydraulicScheme *hydraulicScheme ) const;
};

typedef ReosTelemac2DSimulation::TelemacBoundaryCondition BoundaryCondition;

class ReosTelemac2DSimulationProcess : public ReosSimulationProcess
{
    Q_OBJECT
  public:
    ReosTelemac2DSimulationProcess(
      const ReosCalculationContext &context,
      const ReosDuration &timeStep,
      const QString &simulationfilePath,
      const QList<ReosHydraulicStructureBoundaryCondition *> &boundElem,
      const QMap<int, BoundaryCondition> &boundaries );

    void start() override;
    void stop( bool b ) override;

  signals:
    void askToStop();

  private slots:
    void onStopAsked();

  private:
    QString mSimulationFilePath;
    QProcess *mProcess = nullptr;
    QString mStandartOutputBuffer;
    QRegularExpression mTimeRegEx;
    QRegularExpression mBlockRegEx;
    QRegularExpression mBoundaryFlowRegEx;
    bool mIsPreparation = false;
    double mCurrentTime = 0;
    double mTotalTime = 0;
    QDateTime mStartTime;
    ReosDuration mTimeStep;
    const QMap<int, BoundaryCondition> mBoundaries;

    void addToOutput( const QString &txt );
    void extractInformation( const QRegularExpressionMatch &blockMatch );
};

class ReosTelemac2DSimulationEngineFactory : public ReosSimulationEngineFactory
{
  public:
    ReosTelemac2DSimulationEngineFactory();

    virtual ReosHydraulicSimulation *createSimulation( ReosHydraulicStructure2D *parent ) const override;
    virtual ReosHydraulicSimulation *createSimulation( const ReosEncodedElement &element, ReosHydraulicStructure2D *parent ) const override;

    virtual QString key() const override {return ReosTelemac2DSimulation::staticKey();}
    QString displayName() const override {return QObject::tr( "TELEMAC 2D Simulation" );}
    ReosStructureImporterSource *createImporterSource( const ReosEncodedElement &, const ReosHydraulicNetworkContext & ) const override {return nullptr;}

    void initializeSettings() override;

    static void initializeSettingsStatic();
};

#endif // REOSTELEMAC2DSIMULATION_H
