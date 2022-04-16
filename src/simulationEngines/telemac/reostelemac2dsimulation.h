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
#include "reoscore.h"
#include "reosduration.h"

class ReosHydraulicSimulationResults;


class ReosTelemac2DSimulation : public ReosHydraulicSimulation
{
    Q_OBJECT
  public:
    enum class Equation
    {
      FiniteVolume,
      FiniteElement
    };

    struct TelemacBoundaryCondition
    {
      int rank = -1;
      QString header;
      QString unit;
      ReosTimeSerieVariableTimeStep *timeSeries = nullptr;
      QString boundaryId;
      ReosHydraulicStructureBoundaryCondition::Type type;

    };

    ReosTelemac2DSimulation( QObject *parent = nullptr );
    ReosTelemac2DSimulation( const ReosEncodedElement &element, QObject *parent = nullptr );
    static QString staticKey() {return QStringLiteral( "telemac2D" );}

    QString key() const override {return ReosTelemac2DSimulation::staticKey();}
    QString directoryName() const override {return  QStringLiteral( "TELEMAC_simulation" );}
    void prepareInput( ReosHydraulicStructure2D *hydraulicStructure, const ReosCalculationContext &context ) override;
    virtual ReosSimulationProcess *getProcess( ReosHydraulicStructure2D *hydraulicStructure, const ReosCalculationContext &calculationContext ) const override;
    ReosEncodedElement encode() const override;

    ReosParameterDuration *timeStep() const;
    ReosParameterInteger *outputResultPeriod() const;
    ReosSimulationInitialConditions *initialCondition() const;

    Equation equation() const;
    void setEquation( const Equation &equation );

    virtual bool hasResult( ReosHydraulicStructure2D *hydraulicStructure, const ReosCalculationContext &context ) const override;
    ReosHydraulicSimulationResults *createResults( ReosHydraulicStructure2D *hydraulicStructure, const ReosCalculationContext &calculationContext ) const override;

  private:
    ReosParameterDuration *mTimeStep = nullptr;
    ReosParameterInteger *mOutputResultPeriod = nullptr;
    Equation mEquation = Equation::FiniteElement;
    ReosSimulationInitialConditions *mInitialCondition = nullptr;
    QList<TelemacBoundaryCondition> mBoundaries;

    QList<ReosHydraulicStructureBoundaryCondition *> createBoundaryFiles(
      ReosHydraulicStructure2D *hydraulicStructure,
      QVector<int> &verticesPosInBoundary,
      const ReosCalculationContext &context );

    void createSelafinMeshFrame(
      ReosHydraulicStructure2D *hydraulicStructure,
      const QVector<int> &verticesPosInBoundary,
      const ReosCalculationContext &context );

    void createSelafinInputGeometry(
      ReosHydraulicStructure2D *hydraulicStructure,
      const QVector<int> &verticesPosInBoundary,
      const ReosCalculationContext &context );

    QList<TelemacBoundaryCondition> createBoundaryConditionFiles(
      ReosHydraulicStructure2D *hydraulicStructure,
      QList<ReosHydraulicStructureBoundaryCondition *> boundaryConditions,
      const ReosCalculationContext &context );

    void createSteeringFile(
      ReosHydraulicStructure2D *hydraulicStructure,
      QList<ReosHydraulicStructureBoundaryCondition *> boundaryConditions,
      const ReosCalculationContext &context );
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

    virtual ReosHydraulicSimulation *createSimulation( QObject *parent ) const;
    virtual ReosHydraulicSimulation *createSimulation( const ReosEncodedElement &element, QObject *parent ) const;

    virtual QString key() const {return ReosTelemac2DSimulation::staticKey();}
    QString displayName() const {return QObject::tr( "TELEMAC 2D Simulation" );}
};

#endif // REOSTELEMAC2DSIMULATION_H
