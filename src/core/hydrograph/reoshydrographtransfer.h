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

#include "reoshydrographsource.h"
#include "reoshydrauliclink.h"
#include "reoscalculationcontext.h"

/**
 * Abstract class that can be derived to make hydraulic link thats transfer hydrograph between a source hydrograph to another hydrograph node
 */
class ReosHydrographTransfer : public ReosHydraulicLink
{
    Q_OBJECT
  public:
    //! Constructor
    ReosHydrographTransfer( QObject *parent = nullptr );

    //! Sets the input hydrograph source
    void setInputHydrographSource( ReosHydrographSource *hydrographSource );

    //! Returns the input hydrograph source
    ReosHydrographSource *inputHydrographSource() const;

    //! Sets the destination node
    void setHydrographDestination( ReosHydrographNode *destination );

    //! Returns the outpu hydrograph for the context calculation \a context
    virtual ReosHydrograph *outputHydrograph( const ReosCalculationContext &context ) = 0;
};

//! Class that transfers hydrograph between node without altering the hydrograph
class ReosHydrographTransferDirect: public ReosHydrographTransfer
{
  public:
    ReosHydrographTransferDirect( QObject *parent = nullptr ): ReosHydrographTransfer( parent ) {}

    ReosHydrograph *outputHydrograph( const ReosCalculationContext &context ) override;
};

/**
 * Class that represents an node collecting and sum hydrograph
 */
class ReosHydrographJunction : public ReosHydrographSource
{
  public:
    ReosHydrographJunction( QObject *parent = nullptr );
    ReosHydrograph *outputHydrograph( const ReosCalculationContext &context ) override;

  private:
    mutable ReosHydrograph *mHydrograph;


};

#endif // REOSHYDROGRAPHTRANSFER_H
