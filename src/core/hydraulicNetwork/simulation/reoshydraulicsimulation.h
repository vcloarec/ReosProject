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
#include "reosmesh.h"
#include "reosgisengine.h"

class QProcess;
class QFile;
class QTextStream;
class QDir;

class ReosParameterDateTime;
class ReosParameterDuration;
class ReosParameterInteger;
class ReosHydraulicStructureBoundaryCondition;
class ReosSimulationInitialConditions;
class ReosHydraulicSimulationResults;
class ReosHydraulicSimulation;
class ReosHydraulicScheme;
class ReosStructureImporterSource;
class ReosHydraulicNetworkContext;
class ReosPolygonStructureValues;
struct ReosHydraulicNetworkElementCompatibilty;


#ifndef SIP_RUN
struct ReosSimulationData
{
  struct BoundaryVertices
  {
    QVector<int> verticesIndex;
    QPointer<ReosHydraulicStructureBoundaryCondition> boundaryCondition;
  };

  QVector<BoundaryVertices> boundaryVertices;
  QVector<QVector<QVector<int>>> holesVertices;

  ReosMeshData meshData;

  std::shared_ptr<ReosPolygonStructureValues> roughnessValues;
  double defaultRoughness = 0.03;

  ReosCoordinateSystemTransformer coordinateTransformer;
};

class REOSCORE_EXPORT ReosSimulationPreparationProcess: public ReosProcess
{
    Q_OBJECT
  public:

    ReosSimulationPreparationProcess( ReosHydraulicStructure2D *hydraulicStructure,
                                      ReosHydraulicSimulation *simulation,
                                      const ReosCalculationContext &context );

    void setDestination( const QDir &destination );
    void start() override;

    const ReosCalculationContext &calculationContext() const;

  signals:
    void allBoundariesUpdated();

  private slots:
    void onBoundaryUpdated( const QString &id );
  private:
    QPointer<ReosHydraulicStructure2D> mStructure;
    QPointer<ReosHydraulicSimulation> mSimulation;
    ReosSimulationData mSimulationData;
    ReosCalculationContext mContext;
    bool mHasMesh = false;

    QStringList mWaitedBoundaryId;
    int mBoundaryCount = 0;

    QString mDestinationPath;

};


class REOSCORE_EXPORT ReosSimulationProcess : public ReosProcess
{
    Q_OBJECT
  public:
    ReosSimulationProcess( const ReosCalculationContext &context, const QList<ReosHydraulicStructureBoundaryCondition *> &boundaries );

    QMap<QString, ReosHydrograph *> outputHydrographs() const;

    ReosTimeWindow timeWindow() const;

  signals:
    void sendBoundaryFlow( QDateTime time, QStringList boundaryIds, QList<double> values );

  private slots:
    void onReceiveFlow( QDateTime time, QStringList boundaryIds, QList<double> values );

  private:
    QMap<QString, ReosHydrograph *> mOutputHydrographs;
    ReosTimeWindow mTimewWindow;
};

#endif // No SIP_RUN

class REOSCORE_EXPORT ReosHydraulicSimulation : public ReosDataObject SIP_ABSTRACT
{
    Q_OBJECT
  public:

    enum class Capability
    {
      Hotstart = 1 << 0, //!< If the simulation support hot start
    };
    Q_ENUM( Capability )
    Q_DECLARE_FLAGS( Capabilities, Capability )
    Q_FLAG( Capabilities )

    ReosHydraulicSimulation( ReosHydraulicStructure2D *parent = nullptr );

    virtual bool hasCapability( Capability cap ) const;

    virtual QString key() const = 0;
    virtual ReosEncodedElement encode() const = 0 SIP_SKIP;

    virtual void prepareInput( ReosHydraulicStructure2D *hydraulicStructure, const ReosSimulationData &simulationData, const ReosCalculationContext &calculationContext ) = 0 SIP_SKIP;

    virtual void prepareInput( ReosHydraulicStructure2D *hydraulicStructure, const ReosSimulationData &simulationData, const ReosCalculationContext &calculationContext, const QDir &directory ) = 0 SIP_SKIP;

    virtual ReosSimulationProcess *getProcess( ReosHydraulicStructure2D *hydraulicStructure, const ReosCalculationContext &calculationContext ) const = 0 SIP_SKIP;

    virtual ReosDuration representativeTimeStep() const = 0;

    virtual ReosDuration representative2DTimeStep() const = 0;

    virtual void saveSimulationResult( const ReosHydraulicStructure2D *hydraulicStructure, const QString &shemeId, ReosSimulationProcess *process, bool success ) const = 0 SIP_SKIP;

    virtual ReosHydraulicSimulationResults *loadSimulationResults( ReosHydraulicStructure2D *hydraulicStructure, const QString &shemeId, QObject *parent = nullptr ) const = 0 SIP_SKIP;

    virtual bool hasResult( const ReosHydraulicStructure2D *hydraulicStructure, const QString &shemeId ) const = 0;

    virtual void removeResults( const ReosHydraulicStructure2D *hydraulicStructure, const QString &shemeId ) const = 0;

    virtual QString engineName() const = 0;

    virtual void saveConfiguration( ReosHydraulicScheme *scheme ) const =  0 SIP_SKIP;

    virtual void restoreConfiguration( ReosHydraulicScheme *scheme ) = 0 SIP_SKIP;

    virtual ReosTimeWindow externalTimeWindow() const = 0;

    virtual ReosTimeWindow externalBoundaryConditionTimeWindow( const QString &boundaryId ) const = 0 SIP_SKIP;

    virtual ReosHydraulicNetworkElementCompatibilty checkCompatiblity( ReosHydraulicScheme *scheme ) const SIP_SKIP;

    virtual QFileInfoList cleanScheme( ReosHydraulicStructure2D *hydraulicStructure, ReosHydraulicScheme *scheme ) SIP_SKIP {return QFileInfoList();};

    virtual void setHotStartSchemeId( const QString &schemeId ) {}

    virtual void setHotStartTimeStepIndex( int index ) {}

    virtual void setHotStartUseLastTimeStep( bool b ) {}

#ifndef SIP_RUN

  signals:

    void timeStepChanged();

    void onBoundaryConditionRemove( const QString &bcId );

  protected:
    QDir simulationDir( const ReosHydraulicStructure2D *hydraulicStructure, const QString &schemeId ) const;
    virtual QString directoryName() const {return QString();}

#endif // No SIP_RUN

};

#ifndef SIP_RUN
class ReosSimulationEngineFactory
{
    Q_GADGET
  public:

    enum SimulationEngineCapability
    {
      ImportStructure2D = 1 << 0, //!< If the simulation engine support importing 2D structure
      CanBeCreated = 1 << 1 //!< If simulation of this engine can be created by a factory
    };

    Q_ENUM( SimulationEngineCapability )
    Q_DECLARE_FLAGS( SimulationEngineCapabilities, SimulationEngineCapability )
    Q_FLAG( SimulationEngineCapabilities )

    virtual ~ReosSimulationEngineFactory() = default;

    virtual ReosHydraulicSimulation *createSimulation( ReosHydraulicStructure2D *parent ) const = 0;
    virtual ReosHydraulicSimulation *createSimulation( const ReosEncodedElement &element, ReosHydraulicStructure2D *parent ) const = 0;
    virtual QString key() const  = 0;
    virtual QString displayName() const = 0;
    virtual ReosStructureImporterSource *createImporterSource( const ReosEncodedElement &element, const ReosHydraulicNetworkContext &context ) const = 0;

    virtual void initializeSettings() = 0;

    bool hasCapability( SimulationEngineCapability capability ) const;

  protected:
    SimulationEngineCapabilities mCapabilities = QFlags<SimulationEngineCapability>();

};

class REOSCORE_EXPORT ReosSimulationEngineRegistery
{
  public:
    ReosSimulationEngineRegistery();

    //! Creates and returns a simuation corresponding to the \a key
    ReosHydraulicSimulation *createSimulation( const QString &key, ReosHydraulicStructure2D *parent ) const ;

    //! Creates and returns a simuation corresponding to the encoded \a element
    ReosHydraulicSimulation *createSimulation( const ReosEncodedElement &element, ReosHydraulicStructure2D *parent )const;

    //! Returns a pointer to the static instance of this registery
    static ReosSimulationEngineRegistery *instance();

    //! Returns all the engine available
    const QMap<QString, QString> availableEngine();

    const QMap<QString, QString> availableEngine( ReosSimulationEngineFactory::SimulationEngineCapability capability );

    //! Returns whether the registery contains engine that importation
    bool canImportSrtucture2D() const;

    ReosStructureImporterSource *createStructureImporterSource( const ReosEncodedElement &element, const ReosHydraulicNetworkContext &context );

    //! Registers a \a factory
    void registerEngineFactory( ReosSimulationEngineFactory *factory );

  private:
#ifdef _MSC_VER
    std::unique_ptr<ReosSimulationEngineFactory> dummy; // workaround for MSVC, if not, the line after create a compilation error if this class is exported (REOSCORE_EXPORT)
#endif
    std::map<QString, std::unique_ptr<ReosSimulationEngineFactory>> mFactories;
    static ReosSimulationEngineRegistery *sInstance;
    void loadDynamicLibrary();
};

class ReosHydraulicSimulationDummy;

class ReosSimulationProcessDummy : public ReosSimulationProcess
{
    Q_OBJECT
  public:
    ReosSimulationProcessDummy(
      const ReosHydraulicSimulationDummy *sim,
      const ReosCalculationContext &context,
      const QList<ReosHydraulicStructureBoundaryCondition *> &boundaries );

    void start();

    const ReosHydrograph *output() const;

  private:
    ReosHydrograph mOuput;
    QString mSchemeId;
    const ReosHydraulicSimulationDummy *mSim;
};

/**
 * Dummy class used for test
 */
class REOSCORE_EXPORT ReosHydraulicSimulationDummy : public ReosHydraulicSimulation
{
    Q_OBJECT
  public:
    ReosHydraulicSimulationDummy( ReosHydraulicStructure2D *parent = nullptr ) : ReosHydraulicSimulation( parent )  {}

    virtual QString key() const override {return QStringLiteral( "dummy-simulation" );}
    virtual ReosEncodedElement encode() const override {return ReosEncodedElement();};

    virtual void prepareInput( ReosHydraulicStructure2D *, const ReosSimulationData &, const ReosCalculationContext & ) override {};

    virtual void prepareInput( ReosHydraulicStructure2D *, const ReosSimulationData &, const ReosCalculationContext &, const QDir & ) override {};

    virtual ReosSimulationProcess *getProcess( ReosHydraulicStructure2D *structure, const ReosCalculationContext &calculationContext ) const override;;

    virtual ReosDuration representativeTimeStep() const override {return ReosDuration( 5, ReosDuration::minute );}

    virtual ReosDuration representative2DTimeStep() const override {return ReosDuration( 5, ReosDuration::minute );}

    virtual void saveSimulationResult( const ReosHydraulicStructure2D *structure, const QString &, ReosSimulationProcess *, bool ) const override;;

    virtual ReosHydraulicSimulationResults *loadSimulationResults( ReosHydraulicStructure2D *, const QString &, QObject *parent ) const override;;

    virtual bool hasResult( const ReosHydraulicStructure2D *, const QString &schemeId ) const override;;

    virtual void removeResults( const ReosHydraulicStructure2D *, const QString & ) const override {};

    virtual QString engineName() const override {return QStringLiteral( "dummy" );}

    virtual void saveConfiguration( ReosHydraulicScheme * ) const override {};

    virtual void restoreConfiguration( ReosHydraulicScheme * ) override {};

    ReosTimeWindow externalTimeWindow() const override {return ReosTimeWindow();}

    ReosTimeWindow externalBoundaryConditionTimeWindow( const QString & ) const override {return ReosTimeWindow();}

  private:
    mutable QMap<QString, ReosHydrograph *> mLastHydrographs;

    mutable QSet<QString> mSchemeIdHasResult;

    friend class ReosHydraulicSimulationResultsDummy;
    friend class ReosSimulationProcessDummy;;
};

class ReosSimulationEngineFactoryDummy : public ReosSimulationEngineFactory
{
  public:
    ReosSimulationEngineFactoryDummy() {}

    virtual ReosHydraulicSimulation *createSimulation( ReosHydraulicStructure2D *parent ) const override {return new ReosHydraulicSimulationDummy( parent );}
    virtual ReosHydraulicSimulation *createSimulation( const ReosEncodedElement &, ReosHydraulicStructure2D * ) const override { return nullptr;}

    virtual QString key() const override {return QStringLiteral( "dummy-simulation" );}
    QString displayName() const override {return QObject::tr( "Dummy" );}
    ReosStructureImporterSource *createImporterSource( const ReosEncodedElement &, const ReosHydraulicNetworkContext & ) const override {return nullptr;}

    void initializeSettings() override {}
};

#endif // No SIP_RUN

#endif // REOSHYDRAULICSIMULATION_H
