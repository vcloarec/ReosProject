/***************************************************************************
  reoshydrographsource.h - ReosHydrographSource

 ---------------------
 begin                : 19.5.2021
 copyright            : (C) 2021 by Vincent Cloarec
 email                : vcloarec at gmail dot com
 ***************************************************************************
 *                                                                         *
 *   This program is free software; you can redistribute it and/or modify  *
 *   it under the terms of the GNU General Public License as published by  *
 *   the Free Software Foundation; either version 2 of the License, or     *
 *   (at your option) any later version.                                   *
 *                                                                         *
 ***************************************************************************/
#ifndef REOSHYDROGRAPHSOURCE_H
#define REOSHYDROGRAPHSOURCE_H

#include <QObject>
#include <QPointer>

#include "reoshydraulicnode.h"
#include "reoshydrauliclink.h"
#include "reoshydrograph.h"
#include "reoswatershedmodule.h"
#include "reosparameter.h"

class ReosCalculationContext;
class ReosHydraulicLink;
class ReosHydrographRoutingLink;
class ReosWatershed;
class ReosRunoffHydrographsStore;
class ReosMeteorologicModelsCollection;

class ReosHydraulicNetworkUtils
{
  public:

    template<typename T>
    static QList<T *> upstreamLinkOfType( const ReosHydraulicNode *node )
    {
      QList<T *> ret;
      for ( ReosHydraulicLink *link : node->linksBySide2() )
      {
        T *linkOfType = qobject_cast<T *>( link );
        if ( linkOfType )
          ret.append( linkOfType );
      }

      return ret;
    }

    template<typename T>
    static QList<T *> downstreamLinkOfType( const ReosHydraulicNode *node )
    {
      QList<T *> ret;
      for ( ReosHydraulicLink *link : node->linksBySide1() )
      {
        T *linkOfType = qobject_cast<T *>( link );
        if ( linkOfType )
          ret.append( linkOfType );
      }

      return ret;
    }
};


//! Abstract class that represent a node for hydrograph transfer
class REOSCORE_EXPORT ReosHydrographNode : public ReosHydraulicNode
{
    Q_OBJECT
  public:
    ReosHydrographNode( ReosHydraulicNetwork *parent = nullptr );

    QString type() const override {return staticType();}
    static QString staticType() {return ReosHydraulicNode::staticType() + QString( ':' ) + QStringLiteral( "hydrograph" );}

    QPointF position( const QString &destinationCrs ) const override  {return QPointF();}

    virtual void updateCalculationContextFromUpstream( const ReosCalculationContext &context, ReosHydrographRoutingLink *upstreamLink, bool upstreamWillChange ) = 0;

  public slots:
    virtual void onUpstreamRoutingUpdated( const QString &routingId ) = 0;

  protected:
    ReosHydrographNode( const ReosEncodedElement &encodedElement, ReosHydraulicNetwork *parent = nullptr );

};

//! Abstract class that represent a hydrograph source, that is a node that has a hydrograph as output
class REOSCORE_EXPORT ReosHydrographSource : public ReosHydrographNode
{
    Q_OBJECT
  public:
    ReosHydrographSource( ReosHydraulicNetwork *parent = nullptr );

    virtual ReosHydrograph *outputHydrograph() = 0;

    ReosHydrographRoutingLink *outputHydrographTransfer() const;

    QString type() const override {return staticType(); }
    static QString staticType() {return ReosHydrographNode::staticType() + QString( ':' ) + QStringLiteral( "source" );}

    virtual bool updateCalculationContextFromDownstream( const ReosCalculationContext &context, ReosHydrographRoutingLink *downstreamLink ) = 0;

    ReosParameterBoolean *useForceOutputTimeStep() const;
    ReosParameterDuration *forceOutputTimeStep() const;

  protected:
    ReosHydrographSource( const ReosEncodedElement &encodedElement, ReosHydraulicNetwork *parent = nullptr );
    void encodeData( ReosEncodedElement &element,  const ReosHydraulicNetworkContext & ) const override;

    ReosParameterBoolean *mUseForceOutputTimeStep = nullptr;
    ReosParameterDuration *mForceOutputTimeStep = nullptr;
};

//! Class that represent an hydrograph source with a fixed hydrograph
class REOSCORE_EXPORT ReosHydrographSourceFixed: public ReosHydrographSource
{
    Q_OBJECT
  public:
    ReosHydrographSourceFixed( ReosHydraulicNetwork *parent = nullptr );

    ReosHydrograph *outputHydrograph() override;

    QString type() const override {return staticType();}
    static QString staticType() {return ReosHydrographSource::staticType() + QString( ':' ) + QStringLiteral( "fixed" );}

    void setPosition( const ReosSpatialPosition & ) override {};

    //! Sets the hydrographs, take ownership
    void setHydrograph( ReosHydrograph *hydrograph );

    bool updateCalculationContextFromDownstream( const ReosCalculationContext &, ReosHydrographRoutingLink * ) override;
    void updateCalculationContextFromUpstream( const ReosCalculationContext &, ReosHydrographRoutingLink *, bool ) override;

  public slots:
    void updateCalculationContext( const ReosCalculationContext &context ) override;;
    void onUpstreamRoutingUpdated( const QString & )  override {}

  protected:
    ReosHydrographSourceFixed( const ReosEncodedElement &encodedElement, ReosHydraulicNetwork *parent = nullptr );

  private:
    ReosHydrograph *mHydrograph = nullptr;

    void encodeData( ReosEncodedElement &,  const ReosHydraulicNetworkContext & ) const override {}
};

/**
 * Class that represents an node that can collect and sum hydrograph
 */
class REOSCORE_EXPORT ReosHydrographJunction : public ReosHydrographSource
{
    Q_OBJECT
  public:
    enum InternalHydrographOrigin
    {
      None,
      RunoffHydrograph,
      GaugedHydrograph
    };

    ReosHydrographJunction( const QPointF &position, ReosHydraulicNetwork *parent = nullptr );

    ReosHydrograph *outputHydrograph() override;

    QString type() const override {return staticType(); }
    static QString staticType() {return ReosHydrographSource::staticType() + QString( ':' ) + QStringLiteral( "junction" );}

    QPointF position( const QString &destinationCrs ) const override;
    void setPosition( const ReosSpatialPosition &pos ) override;
    QString defaultDisplayName() const override {return tr( "Junction node" );}
    bool calculationInProgress() const override;
    int calculationMaxProgression() const override;
    int calculationProgression() const override;

    static ReosHydrographJunction *decode( const ReosEncodedElement &encodedElement, const ReosHydraulicNetworkContext &context );

    virtual void updateCalculationContext( const ReosCalculationContext &context ) override;
    void updateCalculationContextFromUpstream( const ReosCalculationContext &context, ReosHydrographRoutingLink *upstreamLink, bool upstreamWillChange ) override;
    bool updateCalculationContextFromDownstream( const ReosCalculationContext &context, ReosHydrographRoutingLink * ) override;

    ReosHydrographRoutingLink *downstreamRouting() const;

    ReosHydrograph *internalHydrograph() const;

    InternalHydrographOrigin internalHydrographOrigin() const;
    void setInternalHydrographOrigin( InternalHydrographOrigin origin );

    int gaugedHydrographIndex() const;

    ReosHydrographsStore *gaugedHydrographsStore() const;

  signals:
    //! Emitted when the internal hydrograph pointer change
    void internalHydrographPointerChange();

  public slots:
    void onUpstreamRoutingUpdated( const QString &routingId ) override;
    void setGaugedHydrographIndex( int gaugedHydrographIndex );

  protected slots:
    void calculateIfAllReady();
    void onInternalHydrographChanged();
    void onTimeStepChange();

  protected:
    mutable ReosHydrograph *mOutputHydrograph;
    ReosHydrographsStore *mHydrographsStore = nullptr;
    QPointer<ReosHydrograph> mInternalHydrograph;
    InternalHydrographOrigin mInternalHydrographOrigin = None;
    int mGaugedHydrographIndex = -1;
    bool mInternalHydrographUpdated = false;
    bool mNeedCalculation = true;

    ReosHydrographJunction( const ReosEncodedElement &encodedElement, ReosHydraulicNetwork *parent = nullptr );
    void encodeData( ReosEncodedElement &element,  const ReosHydraulicNetworkContext &context ) const override;

    bool setCurrentInternalHydrograph( ReosHydrograph *newHydrograph );

  private:
    ReosSpatialPosition mPosition;
    QSet<QString> mWaitingForUpstreamLinksUpdated;
    bool mCalculationIsInProgress = false;

    class HydrographSumCalculation: public ReosHydrographCalculation
    {
      public:
        //! Constructor
        HydrographSumCalculation();

        /**
         *  Adds a hydrograph for the calculation, a deep copy (implicitly shared if \a hydro id a memory provider type) is done
         *  to ensure the operation is thread safe
         */
        void addHydrograph( ReosHydrograph *hydro );

        void forceOutputTimeStep( const ReosDuration &timeStep );

        void start() override;

      private:
        QList<ReosHydrograph *> mHydrographsToAdd;
        bool mForceOutputTimeStep = false;
        ReosDuration mTimeStep;
    };

    HydrographSumCalculation *mSumCalculation = nullptr;

    virtual bool updateInternalHydrographCalculationContext( const ReosCalculationContext & );
    void init();

    virtual bool updateInternalHydrograph();
    virtual void calculateInternalHydrograph();
    void calculateOuputHydrograph();
};

class ReosHydrographJunctionFactory : public ReosHydraulicNetworkElementFactory
{
  public:
    ReosHydrographJunctionFactory() = default;
    ReosHydraulicNetworkElement *decodeElement( const ReosEncodedElement &encodedElement, const ReosHydraulicNetworkContext &context ) const override;
};


//! Class that represent a node for a specified watershed
class REOSCORE_EXPORT ReosHydrographNodeWatershed : public ReosHydrographJunction
{
    Q_OBJECT
  public:
    //! Constructor with \a watershed
    ReosHydrographNodeWatershed( ReosWatershed *watershed, ReosMeteorologicModelsCollection *meteoModelCollection, ReosHydraulicNetwork *parent = nullptr );

    QString type() const override {return staticType();}
    static QString staticType() {return ReosHydrographJunction::staticType() + QString( ':' ) + QStringLiteral( "watershed" );}

    ReosHydrograph *outputHydrograph() override;
    QPointF position( const QString &destinationCrs ) const override;
    void setPosition( const ReosSpatialPosition & ) override {}; // position of this node can't be set because this is the outlet of the watershed
    QString defaultDisplayName() const override {return tr( "Watershed node" );}

    ReosWatershed *watershed() const;

    static ReosHydrographNodeWatershed *decode( const ReosEncodedElement &encodedElement, const ReosHydraulicNetworkContext &context );

  public slots:
  protected:
    void encodeData( ReosEncodedElement &element,  const ReosHydraulicNetworkContext &context ) const override;
    ReosHydrographNodeWatershed( const ReosEncodedElement &encodedElement, ReosWatershed *watershed, ReosMeteorologicModelsCollection *meteoModelCollection, ReosHydraulicNetwork *parent = nullptr );

  private:
    // persitent data
    QPointer<ReosWatershed> mWatershed;

    // Runtime variable
    ReosRunoffHydrographsStore *mRunoffHydrographs;
    ReosMeteorologicModel *mLastMeteoModel = nullptr;

    void init();

    void calculateInternalHydrograph() override;
    bool updateInternalHydrographCalculationContext( const ReosCalculationContext &context ) override;
    bool updateInternalHydrograph() override;
};


class ReosHydrographNodeWatershedFactory : public ReosHydraulicNetworkElementFactory
{
  public:
    ReosHydrographNodeWatershedFactory() = default;
    ReosHydraulicNetworkElement *decodeElement( const ReosEncodedElement &encodedElement, const ReosHydraulicNetworkContext &context ) const override;
};

#endif // REOSHYDROGRAPHSOURCE_H
