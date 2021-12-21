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

class ReosHydrographRoutineLink;

class ReosHydrographRoutineMethod : public ReosDataObject
{
  public:
    ReosHydrographRoutineMethod( ReosHydrographRoutineLink *routingLink );
    virtual void calculateOutputHydrograph( ReosHydrograph *inputHydrograph, ReosHydrograph *outputHydrograph, const ReosCalculationContext &context ) = 0;

    QString type() const override {return staticType();}
    static QString staticType() {return QStringLiteral( "hydrographRoutingMethod" );}

    virtual ReosHydrographCalculation *calculationProcess( ReosHydrograph *inputHydrograph, const ReosCalculationContext &context ) = 0;

    virtual ReosEncodedElement encode() const = 0;
};

class ReosHydrographRoutingMethodFactory
{
  public:
    virtual ReosHydrographRoutineMethod *createRoutingMethod( ReosHydrographRoutineLink *routingLink ) const = 0;
    virtual ReosHydrographRoutineMethod *createRoutingMethod( const ReosEncodedElement &encodedElement, ReosHydrographRoutineLink *routingLink ) const = 0;
    virtual QString type() const = 0;
};


class ReosHydrographRoutingMethodFactories : public ReosModule
{
  public:
    ~ReosHydrographRoutingMethodFactories();

    static void instantiate( ReosModule *parent );

    static bool isInstantiate();
    static ReosHydrographRoutingMethodFactories *instance();

    void addFactory( ReosHydrographRoutingMethodFactory *factory );

    ReosHydrographRoutineMethod *createRoutingMethod( const QString &type, ReosHydrographRoutineLink *link );
    ReosHydrographRoutineMethod *createRoutingMethod( const ReosEncodedElement &encodedElement, ReosHydrographRoutineLink *link );

  private:
    ReosHydrographRoutingMethodFactories( ReosModule *parent = nullptr );
    static ReosHydrographRoutingMethodFactories *sInstance;
    using Factory = std::unique_ptr<ReosHydrographRoutingMethodFactory>;
    std::map<QString, Factory> mFactories;
};


/**
 * Abstract class that can be derived to make hydraulic link thats transfer hydrograph between a hydrograph source to another hydrograph node
 */
class ReosHydrographRoutineLink : public ReosHydraulicLink
{
    Q_OBJECT
  public:
    //! Constructor
    ReosHydrographRoutineLink( ReosHydraulicNetwork *parent = nullptr );

    //! Constructor with input and ouptut
    ReosHydrographRoutineLink( ReosHydrographSource *hydrographSource, ReosHydrographNode *destination, ReosHydraulicNetwork *parent = nullptr );

    bool setCurrentRoutingMethod( const QString &routingType );
    ReosHydrographRoutineMethod *currentRoutingMethod() const;

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
    QString defaultDisplayName() const {return tr( "Hydrograph routing" );}

    static ReosHydrographRoutineLink *decode( const ReosEncodedElement &encodedElement, const ReosHydraulicNetworkContext &context );

    //! Updates the output hydrograph for the context calculation \a context
    void updateCalculationContext( const ReosCalculationContext &context ) override;

    void updateCalculationContextFromUpstream( const ReosCalculationContext &context, bool upstreamWillChange );
    bool updateCalculationContextFromDownstream( const ReosCalculationContext &context );

  public slots:
    void calculateRoutine();

  protected:
    ReosHydrographRoutineLink( ReosHydrographSource *hydrographSource,
                               ReosHydrographNode *destination,
                               const ReosEncodedElement &encodedElement,
                               ReosHydraulicNetwork *parent = nullptr );

  private slots:
    void onSourceUpdated();

  private:

    QMap<QString, ReosHydrographRoutineMethod *> mRoutineMethods;
    QString mCurrentRoutingMethod;
    ReosHydrographCalculation *mCalculation = nullptr;

    ReosHydrograph *mOutputHydrograph = nullptr;

    void encodeData( ReosEncodedElement &element,  const ReosHydraulicNetworkContext &context ) const override;



};

class ReosHydrographRoutineLinkFactory: public ReosHydraulicNetworkElementFactory
{
  public:
    ReosHydrographRoutineLinkFactory() = default;

    virtual ReosHydraulicNetworkElement *decodeElement( const ReosEncodedElement &encodedElement, const ReosHydraulicNetworkContext &context ) const;
};

//! Class that transfers hydrograph between node without altering the hydrograph
class ReosDirectHydrographRoutine: public ReosHydrographRoutineMethod
{
    Q_OBJECT
  public:
    ReosDirectHydrographRoutine( ReosHydrographRoutineLink *routingLink );

    void calculateOutputHydrograph( ReosHydrograph *inputHydrograph,
                                    ReosHydrograph *outputHydrograph,
                                    const ReosCalculationContext &context ) override;

    ReosHydrographCalculation *calculationProcess( ReosHydrograph *inputHydrograph, const ReosCalculationContext &context ) override;

    QString type() const override {return staticType();}
    QString static staticType() {return ReosHydrographRoutineMethod::staticType() + QString( ':' ) + QStringLiteral( "direct" ); }

    ReosEncodedElement encode() const override
    {
      ReosEncodedElement element( type() );
      return element;
    }

  private:

    class Calculation: public ReosHydrographCalculation
    {
      public:
        Calculation( ReosHydrograph *inputHydrograph )
        {
          mInputHydrograph = std::make_unique<ReosHydrograph>();
          mInputHydrograph->copyFrom( inputHydrograph );
        }

        void start()
        {
          mHydrograph.reset( new ReosHydrograph );
          mHydrograph->copyFrom( mInputHydrograph.get() );
          mIsSuccessful = true;
        }
      private:
        std::unique_ptr<ReosHydrograph> mInputHydrograph;
    };
};

class ReosDirectHydrographRoutingFactory : public ReosHydrographRoutingMethodFactory
{
  public:
    ReosHydrographRoutineMethod *createRoutingMethod( ReosHydrographRoutineLink *routingLink ) const override
    {return new ReosDirectHydrographRoutine( routingLink );}

    ReosHydrographRoutineMethod *createRoutingMethod( const ReosEncodedElement &, ReosHydrographRoutineLink *routingLink ) const override
    {return new ReosDirectHydrographRoutine( routingLink );};

    virtual QString type() const override
    {return ReosDirectHydrographRoutine::staticType();}
};



#endif // REOSHYDROGRAPHTRANSFER_H
