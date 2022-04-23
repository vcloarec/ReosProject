/***************************************************************************
  reoshydraulicsimulation.h - ReosHydraulicSimulation

 ---------------------
 begin                : 19.3.2022
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
#ifndef REOSHYDRAULICSIMULATION_H
#define REOSHYDRAULICSIMULATION_H

#include "reosdataobject.h"

#include "reosprocess.h"
#include "reoscalculationcontext.h"

class QProcess;
class QFile;
class QTextStream;
class QDir;

class ReosHydraulicStructure2D;
class ReosParameterDateTime;
class ReosParameterDuration;
class ReosParameterInteger;
class ReosHydraulicStructureBoundaryCondition;
class ReosSimulationInitialConditions;
class ReosHydraulicSimulationResults;
class ReosHydraulicSimulation;


class REOSCORE_EXPORT ReosSimulationPreparationProcess: public ReosProcess
{
    Q_OBJECT
  public:

    ReosSimulationPreparationProcess( ReosHydraulicStructure2D *hydraulicStructure,
                                      ReosHydraulicSimulation *simulation,
                                      const ReosCalculationContext &context );

    void setDestination( const QDir &destination );

    void start() override;

  signals:
    void allBoundariesUpdated();

  private slots:
    void onBoundaryUpdated( const QString &id );
  private:
    QPointer<ReosHydraulicStructure2D> mStructure;
    QPointer<ReosHydraulicSimulation> mSimulation;
    ReosCalculationContext mContext;

    QStringList mWaitedBoundaryId;
    int mBoundaryCount;

    QString mDestinationPath;

};



class REOSCORE_EXPORT ReosSimulationProcess : public ReosProcess
{
    Q_OBJECT
  public:
    ReosSimulationProcess( const ReosCalculationContext &context, const QList<ReosHydraulicStructureBoundaryCondition *> boundaries );

    QMap<QString, ReosHydrograph *> outputHydrographs() const;

  signals:
    void sendBoundaryFlow( const QDateTime &time, const QStringList &boundaryIds, const QList<double> &values ) const;

  private slots:
    void onReceiveFlow( const QDateTime &time, const QStringList &boundaryIds, const QList<double> &values );

  private:
    QMap<QString, ReosHydrograph *> mOutputHydrographs;

};

class REOSCORE_EXPORT ReosHydraulicSimulation : public ReosDataObject
{
    Q_OBJECT
  public:
    ReosHydraulicSimulation( QObject *parent = nullptr );

    virtual QString directoryName() const = 0;

    virtual void prepareInput( ReosHydraulicStructure2D *hydraulicStructure, const ReosCalculationContext &calculationContext ) = 0;

    virtual void prepareInput( ReosHydraulicStructure2D *hydraulicStructure, const ReosCalculationContext &calculationContext, const QDir &directory ) = 0;

    virtual ReosSimulationProcess *getProcess( ReosHydraulicStructure2D *hydraulicStructure, const ReosCalculationContext &calculationContext ) const = 0;

    virtual QString key() const = 0;
    virtual ReosEncodedElement encode() const = 0;

    virtual void saveSimulationResult( const ReosHydraulicStructure2D *hydraulicStructure, const QString &shemeId, bool success ) const = 0;

    virtual ReosHydraulicSimulationResults *loadSimulationResults( ReosHydraulicStructure2D *hydraulicStructure, const QString &shemeId ) const = 0;

    virtual bool hasResult( ReosHydraulicStructure2D *hydraulicStructure, const QString &shemeId ) const = 0;

    QDir simulationDir( const ReosHydraulicStructure2D *hydraulicStructure, const QString &schemeId ) const;

    virtual QString engineName() const = 0;

};


class ReosSimulationEngineFactory
{
  public:
    virtual ReosHydraulicSimulation *createSimulation( QObject *parent ) const = 0;
    virtual ReosHydraulicSimulation *createSimulation( const ReosEncodedElement &element, QObject *parent ) const = 0;
    virtual QString key() const  = 0;
    virtual QString displayName() const = 0;
};

class REOSCORE_EXPORT ReosSimulationEngineRegistery
{
  public:
    ReosSimulationEngineRegistery();

    //! Creates and returns a simuation corresponding to the \a key
    ReosHydraulicSimulation *createSimulation( const QString &key, QObject *parent ) const ;

    //! Creates and returns a simuation corresponding to the encoded \a element
    ReosHydraulicSimulation *createSimulation( const ReosEncodedElement &element, QObject *parent )const;

    //! Returns a pointer to the static instance of this registery
    static ReosSimulationEngineRegistery *instance();

    const QMap<QString, QString> availableEngine();

  private:
#ifdef _MSC_VER
    std::unique_ptr<ReosSimulationEngineFactory> dummy; // workaround for MSVC, if not, the line after create an compilation error if this class is exported (REOSCORE_EXPORT)
#endif
    //! Registers a \a factory
    void registerEngineFactory( ReosSimulationEngineFactory *factory );

    std::map<QString, std::unique_ptr<ReosSimulationEngineFactory>> mFactories;
    static ReosSimulationEngineRegistery *sInstance;
    void loadDynamicLibrary();
};


#endif // REOSHYDRAULICSIMULATION_H
