/***************************************************************************
  reoshydrographtransfer.h - ReosHydrographTransfer

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
#ifndef REOSHYDROGRAPHTRANSFER_H
#define REOSHYDROGRAPHTRANSFER_H

#include <QPointer>
#include <QPointF>

#include "reoshydrographsource.h"
#include "reoshydrauliclink.h"
#include "reoscalculationcontext.h"

class ReosHydrographRoutingLink;

class ReosHydrographRoutingMethod : public ReosDataObject
{
  public:
    ReosHydrographRoutingMethod( ReosHydrographRoutingLink *routingLink );
    virtual void calculateOutputHydrograph( ReosHydrograph *inputHydrograph, ReosHydrograph *outputHydrograph, const ReosCalculationContext &context ) = 0;

    QString type() const override {return staticType();}
    static QString staticType() {return QStringLiteral( "hydrographRoutingMethod" );}

    virtual ReosHydrographCalculation *calculationProcess( ReosHydrograph *inputHydrograph, const ReosCalculationContext &context ) = 0;

    virtual ReosEncodedElement encode() const = 0;
};

class ReosHydrographRoutingMethodFactory
{
  public:
    virtual ReosHydrographRoutingMethod *createRoutingMethod( ReosHydrographRoutingLink *routingLink ) const = 0;
    virtual ReosHydrographRoutingMethod *createRoutingMethod( const ReosEncodedElement &encodedElement, ReosHydrographRoutingLink *routingLink ) const = 0;
    virtual QString type() const = 0;
    virtual QString displayName() const = 0;
    virtual QString htmlDescription() const = 0;
};


class ReosHydrographRoutingMethodFactories : public ReosModule
{
  public:
    ~ReosHydrographRoutingMethodFactories();

    static void instantiate( ReosModule *parent );

    static bool isInstantiate();
    static ReosHydrographRoutingMethodFactories *instance();

    void addFactory( ReosHydrographRoutingMethodFactory *factory );

    ReosHydrographRoutingMethod *createRoutingMethod( const QString &type, ReosHydrographRoutingLink *link );
    ReosHydrographRoutingMethod *createRoutingMethod( const ReosEncodedElement &encodedElement, ReosHydrographRoutingLink *link );

    QString displayName( const QString &type ) const;
    QStringList methodTypes() const;
    QString htmlDescription( const QString &type );

  private:
    ReosHydrographRoutingMethodFactories( ReosModule *parent = nullptr );
    static ReosHydrographRoutingMethodFactories *sInstance;
    using Factory = std::unique_ptr<ReosHydrographRoutingMethodFactory>;
    std::map<QString, Factory> mFactories;
};


/**
 * Abstract class that can be derived to make hydraulic link thats transfer hydrograph between a hydrograph source to another hydrograph node
 */
class ReosHydrographRoutingLink : public ReosHydraulicLink
{
    Q_OBJECT
  public:
    //! Constructor
    ReosHydrographRoutingLink( ReosHydraulicNetwork *parent = nullptr );

    //! Constructor with input and ouptut
    ReosHydrographRoutingLink( ReosHydrographSource *hydrographSource, ReosHydrographNode *destination, ReosHydraulicNetwork *parent = nullptr );

    bool setCurrentRoutingMethod( const QString &routingType );
    ReosHydrographRoutingMethod *currentRoutingMethod() const;

    //! Sets the input hydrograph source
    void setInputHydrographSource( ReosHydrographSource *hydrographSource );

    //! Returns the input hydrograph source
    ReosHydrographSource *inputHydrographSource() const;

    ReosHydrographNode *destinationNode() const;

    //! Sets the destination node
    void setHydrographDestination( ReosHydrographNode *destination );

    //! Returns the input hydrograph
    virtual ReosHydrograph *inputHydrograph() const;

    //! Returns the output hydrograph
    virtual ReosHydrograph *outputHydrograph() const;

    QString type() const override {return staticType();}
    QString static staticType() {return ReosHydraulicLink::staticType() + QString( ':' ) + QStringLiteral( "routing" ); }
    QString defaultDisplayName() const override {return tr( "Hydrograph routing" );}
    bool calculationInProgress() const override;
    int calculationMaxProgression() const override;
    int calculationProgression() const override;

    static ReosHydrographRoutingLink *decode( const ReosEncodedElement &encodedElement, const ReosHydraulicNetworkContext &context );

    //! Updates the output hydrograph for the context calculation \a context
    void updateCalculationContext( const ReosCalculationContext &context ) override;

    void updateCalculationContextFromUpstream( const ReosCalculationContext &context, bool upstreamWillChange );
    bool updateCalculationContextFromDownstream( const ReosCalculationContext &context );

  public slots:
    void calculateRouting();

  protected:
    ReosHydrographRoutingLink( ReosHydrographSource *hydrographSource,
                               ReosHydrographNode *destination,
                               const ReosEncodedElement &encodedElement,
                               ReosHydraulicNetwork *parent = nullptr );

  private slots:
    void onSourceUpdated();

  private:

    QMap<QString, ReosHydrographRoutingMethod *> mRoutingMethods;
    QString mCurrentRoutingMethod;
    ReosHydrographCalculation *mCalculation = nullptr;
    bool mCalculationIsInProgress = false;

    ReosHydrograph *mOutputHydrograph = nullptr;

    void encodeData( ReosEncodedElement &element,  const ReosHydraulicNetworkContext &context ) const override;

    void init();
};

class ReosHydrographRoutingLinkFactory: public ReosHydraulicNetworkElementFactory
{
  public:
    ReosHydrographRoutingLinkFactory() = default;

    virtual ReosHydraulicNetworkElement *decodeElement( const ReosEncodedElement &encodedElement, const ReosHydraulicNetworkContext &context ) const;
};

//! Class that transfers hydrograph between node without altering the hydrograph
class ReosHydrographRoutingMethodDirect: public ReosHydrographRoutingMethod
{
    Q_OBJECT
  public:
    ReosHydrographRoutingMethodDirect( ReosHydrographRoutingLink *routingLink );

    void calculateOutputHydrograph( ReosHydrograph *inputHydrograph,
                                    ReosHydrograph *outputHydrograph,
                                    const ReosCalculationContext &context ) override;

    ReosHydrographCalculation *calculationProcess( ReosHydrograph *inputHydrograph, const ReosCalculationContext &context ) override;

    QString type() const override {return staticType();}
    QString static staticType() {return ReosHydrographRoutingMethod::staticType() + QString( ':' ) + QStringLiteral( "direct" ); }

    ReosEncodedElement encode() const override;

  private:

    class Calculation: public ReosHydrographCalculation
    {
      public:
        Calculation( ReosHydrograph *inputHydrograph );

        void start();
      private:
        std::unique_ptr<ReosHydrograph> mInputHydrograph;
    };
};

class ReosHydrographRoutingMethodDirectFactory : public ReosHydrographRoutingMethodFactory
{
  public:
    ReosHydrographRoutingMethod *createRoutingMethod( ReosHydrographRoutingLink *routingLink ) const override
    {return new ReosHydrographRoutingMethodDirect( routingLink );}

    ReosHydrographRoutingMethod *createRoutingMethod( const ReosEncodedElement &, ReosHydrographRoutingLink *routingLink ) const override
    {return new ReosHydrographRoutingMethodDirect( routingLink );};

    virtual QString type() const override
    {return ReosHydrographRoutingMethodDirect::staticType();}

    QString displayName() const override {return QObject::tr( "Without distortion" );}

    QString htmlDescription() const override;
};

class ReosHydrographRoutingMethodMuskingum : public ReosHydrographRoutingMethod
{
    Q_OBJECT
  public:
    ReosHydrographRoutingMethodMuskingum( ReosHydrographRoutingLink *parent = nullptr );
    ReosHydrographRoutingMethodMuskingum( const ReosEncodedElement &encodedElement, ReosHydrographRoutingLink *parent = nullptr );

    void calculateOutputHydrograph( ReosHydrograph *inputHydrograph, ReosHydrograph *outputHydrograph, const ReosCalculationContext &context ) override;
    ReosHydrographCalculation *calculationProcess( ReosHydrograph *inputHydrograph, const ReosCalculationContext &context ) override;

    QString type() const override {return staticType();}
    static QString staticType() {return ReosHydrographRoutingMethod::staticType() + QString( ':' ) + QStringLiteral( "muskingum" );}

    ReosParameterDuration *kParameter() const;
    ReosParameterDouble *xParameter() const;

    ReosEncodedElement encode() const override;

    static void calculate( ReosHydrograph *inputHydrograph, ReosHydrograph *outputHydrograph, const ReosDuration &K, double x, ReosProcess *process = nullptr );

  private:
    class Calculation: public ReosHydrographCalculation
    {
      public:
        Calculation( ReosHydrograph *inputHydrograph, const ReosDuration &K, double X );

        void start() override;
      private:
        std::unique_ptr<ReosHydrograph> mInputHydrograph;
        ReosDuration mK;
        double mX = 0;
    };

    ReosParameterDuration *mKParameter = nullptr;
    ReosParameterDouble *mXParameter = nullptr;
};

class ReosHydrographRoutingMethodMuskingumFactory : public ReosHydrographRoutingMethodFactory
{
  public:
    ReosHydrographRoutingMethod *createRoutingMethod( ReosHydrographRoutingLink *routingLink ) const override;
    ReosHydrographRoutingMethod *createRoutingMethod( const ReosEncodedElement &encodedElement, ReosHydrographRoutingLink *routingLink ) const override;
    QString type() const override;
    QString displayName() const override {return QObject::tr( "Muskingum" );}
    QString htmlDescription() const override;
};

class ReosHydrographRoutingMethodLag : public ReosHydrographRoutingMethod
{
    Q_OBJECT
  public:
    ReosHydrographRoutingMethodLag( ReosHydrographRoutingLink *parent = nullptr );
    ReosHydrographRoutingMethodLag( const ReosEncodedElement &encodedElement, ReosHydrographRoutingLink *parent = nullptr );

    void calculateOutputHydrograph( ReosHydrograph *inputHydrograph, ReosHydrograph *outputHydrograph, const ReosCalculationContext &context ) override;
    ReosHydrographCalculation *calculationProcess( ReosHydrograph *inputHydrograph, const ReosCalculationContext &context ) override;

    QString type() const override {return staticType();}
    static QString staticType() {return ReosHydrographRoutingMethod::staticType() + QString( ':' ) + QStringLiteral( "lag" );}

    ReosParameterDuration *lagParameter() const;

    ReosEncodedElement encode() const override;

    static void calculate( ReosHydrograph *inputHydrograph, ReosHydrograph *outputHydrograph, const ReosDuration &lag, ReosProcess *process = nullptr );

  private:
    class Calculation: public ReosHydrographCalculation
    {
      public:
        Calculation( ReosHydrograph *inputHydrograph, const ReosDuration &lag );

        void start() override;
      private:
        std::unique_ptr<ReosHydrograph> mInputHydrograph;
        ReosDuration mLag;
    };

    ReosParameterDuration *mLagParameter = nullptr;
};

class ReosHydrographRoutingMethodLagFactory : public ReosHydrographRoutingMethodFactory
{
  public:
    ReosHydrographRoutingMethod *createRoutingMethod( ReosHydrographRoutingLink *routingLink ) const override;
    ReosHydrographRoutingMethod *createRoutingMethod( const ReosEncodedElement &encodedElement, ReosHydrographRoutingLink *routingLink ) const override;
    QString type() const override;
    QString displayName() const override {return QObject::tr( "Lag" );}
    QString htmlDescription() const override;
};


#endif // REOSHYDROGRAPHTRANSFER_H
