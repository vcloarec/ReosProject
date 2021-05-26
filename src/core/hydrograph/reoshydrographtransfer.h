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

class ReosHydrographRoutingMethod
{
  public:
    virtual ReosHydrograph *outputHydrograph( ReosHydrograph *inputHydrograph, const ReosCalculationContext &context ) = 0;

};

class ReosHydrographRoutingDirect
{
  public:

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

    //! Sets the input hydrograph source
    void setInputHydrographSource( ReosHydrographSource *hydrographSource );

    //! Returns the input hydrograph source
    ReosHydrographSource *inputHydrographSource() const;

    //! Sets the destination node
    void setHydrographDestination( ReosHydrographNode *destination );

    //! Returns the output hydrograph for the context calculation \a context
    virtual ReosHydrograph *outputHydrograph( const ReosCalculationContext &context )
    {
      return mOutputHydrograph;
    }

    QString type() const override {return hydrographRoutingType();}
    QString static hydrographRoutingType() {return hydraulicLinkType() + QString( ':' ) + QStringLiteral( "routing" ); }

  private:
    ReosHydrographRoutingMethod *mRoutingMethod;
    ReosHydrograph *mOutputHydrograph;

};

//! Class that transfers hydrograph between node without altering the hydrograph
class ReosHydrographTransferDirect: public ReosHydrographRouting
{
    Q_OBJECT
  public:
    ReosHydrographTransferDirect( ReosHydraulicNetwork *parent = nullptr );
    ReosHydrographTransferDirect( ReosHydrographSource *hydrographSource, ReosHydrographNode *destination, ReosHydraulicNetwork *parent = nullptr );

    ReosHydrograph *outputHydrograph( const ReosCalculationContext &context ) override;

    QString type() const override {return hydrographTransferDirectType();}
    QString static hydrographTransferDirectType() {return hydrographRoutingType() + QString( ':' ) + QStringLiteral( "direct" ); }
};


#endif // REOSHYDROGRAPHTRANSFER_H
