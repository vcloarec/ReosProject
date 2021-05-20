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

class ReosHydrograph;
class ReosCalculationContext;
class ReosHydraulicLink;
class ReosWatershed;

//! Abstract class that represent a node for hydrograph transfer
class ReosHydrographNode : public ReosHydraulicNode
{
  public:
    ReosHydrographNode( QObject *parent = nullptr );
};

//! Abstract class that represent a hydrograph source, that is a node that has a hydrograph as output
class ReosHydrographSource : public ReosHydrographNode
{
    Q_OBJECT
  public:
    ReosHydrographSource( QObject *parent = nullptr );
    virtual ReosHydrograph *outputHydrograph( const ReosCalculationContext &context ) = 0;
};

//! Class that represent an hydrograph source with a fixed hydrograph
class ReosHydrographSourceFixed: public ReosHydrographSource
{
  public:
    ReosHydrographSourceFixed( QObject *parent = nullptr );
    ReosHydrograph *outputHydrograph( const ReosCalculationContext &context ) override;

    //! Sets the hydrographs, take ownership
    void setHydrograph( ReosHydrograph *hydrograph );

  private:
    ReosHydrograph *mHydrograph = nullptr;
};

//! Class that represent a hydrograph source for a specified watershed
class ReosHydrographSourceWatershed : public ReosHydrographSource
{
  public:
    //! Constructor with \a watershed
    ReosHydrographSourceWatershed( ReosWatershed *watershed, QObject *parent = nullptr ):
      ReosHydrographSource( parent )
      , mWatershed( watershed )
    {}

    ReosHydrograph *outputHydrograph( const ReosCalculationContext &context );

  private:
    QPointer<ReosWatershed> mWatershed;
    ReosHydrograph *mHydrograph = nullptr;
};

#endif // REOSHYDROGRAPHSOURCE_H
