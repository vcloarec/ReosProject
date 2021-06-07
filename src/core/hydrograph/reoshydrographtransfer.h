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

class ReosHydrographRouting;

class ReosHydrographRoutingMethod : public ReosDataObject
{
  public:
    ReosHydrographRoutingMethod( ReosHydrographRouting *routingLink );
    virtual void calculateOutputHydrograph( ReosHydrograph *inputHydrograph, ReosHydrograph *outputHydrograph, const ReosCalculationContext &context ) = 0;

    QString type() const override {return typeString();}
    static QString typeString() {return QStringLiteral( "hydrographRoutingMethod" );}
};

class ReosHydrographRoutingMethodFactory
{
  public:
    virtual ReosHydrographRoutingMethod *createRoutingMethod( ReosHydrographRouting *routingLink ) const = 0;
    virtual QString type() const = 0;
};


class ReosHydrographRoutingMethodFactories : public ReosModule
{
  public:
    static void instantiate( ReosModule *parent );

    static bool isInstantiate();
    static ReosHydrographRoutingMethodFactories *instance();

    void addFactory( ReosHydrographRoutingMethodFactory *factory )
    {
      mFactories.emplace( factory->type(), factory );
    }

    ReosHydrographRoutingMethod *createRoutingMethod( const QString &type, ReosHydrographRouting *link )
    {
      auto it = mFactories.find( type );
      if ( it != mFactories.end() )
        return it->second->createRoutingMethod( link );

      return nullptr;
    }

  private:
    ReosHydrographRoutingMethodFactories( ReosModule *parent = nullptr );
    static ReosHydrographRoutingMethodFactories *sInstance;
    using Factory = std::unique_ptr<ReosHydrographRoutingMethodFactory>;
    std::map<QString, Factory> mFactories;
};



/**
 * Abstract class that can be derived to make hydraulic link thats transfer hydrograph between a source hydrograph to another hydrograph node
 */
class ReosHydrographRouting : public ReosHydraulicLink
{
    Q_OBJECT
  public:
    //! Constructor
    ReosHydrographRouting( ReosHydraulicNetwork *parent = nullptr );

    //! Constructor with input and ouptut
    ReosHydrographRouting( ReosHydrographSource *hydrographSource, ReosHydrographNode *destination, ReosHydraulicNetwork *parent = nullptr );

    bool setCurrentRoutingMethod( const QString &routingType );
    ReosHydrographRoutingMethod *currentRoutingMethod() const;

    //! Sets the input hydrograph source
    void setInputHydrographSource( ReosHydrographSource *hydrographSource );

    //! Returns the input hydrograph source
    ReosHydrographSource *inputHydrographSource() const;

    //! Sets the destination node
    void setHydrographDestination( ReosHydrographNode *destination );

    //! Returns the output hydrograph for the context calculation \a context
    virtual ReosHydrograph *outputHydrograph() const;

    QString type() const override {return typeString();}
    QString static typeString() {return ReosHydraulicLink::typeString() + QString( ':' ) + QStringLiteral( "routing" ); }

  public slots:
    void updateCalculation( const ReosCalculationContext &context );

  private:
    QMap<QString, ReosHydrographRoutingMethod *> mRoutingMethods;
    QString mCurrentRoutingMethod;
    ReosHydrograph *mOutputHydrograph = nullptr;

};

//! Class that transfers hydrograph between node without altering the hydrograph
class ReosDirectHydrographRouting: public ReosHydrographRoutingMethod
{
    Q_OBJECT
  public:
    ReosDirectHydrographRouting( ReosHydrographRouting *routingLink );

    virtual void calculateOutputHydrograph( ReosHydrograph *inputHydrograph,
                                            ReosHydrograph *outputHydrograph,
                                            const ReosCalculationContext &context ) override;

    QString type() const override {return typeString();}
    QString static typeString() {return ReosHydrographRoutingMethod::typeString() + QString( ':' ) + QStringLiteral( "direct" ); }
};

class ReosDirectHydrographRoutingFactory : public ReosHydrographRoutingMethodFactory
{
  public:
    ReosHydrographRoutingMethod *createRoutingMethod( ReosHydrographRouting *routingLink ) const override    {return new ReosDirectHydrographRouting( routingLink );}
    virtual QString type() const override    {return ReosDirectHydrographRouting::typeString();}
};



#endif // REOSHYDROGRAPHTRANSFER_H
