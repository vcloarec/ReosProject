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
class ReosHydrographRouting;
class ReosWatershed;

//! Abstract class that represent a node for hydrograph transfer
class ReosHydrographNode : public ReosHydraulicNode
{
    Q_OBJECT
  public:
    ReosHydrographNode( ReosHydraulicNetwork *parent = nullptr );

    QString type() const override {return typeString();}
    static QString typeString() {return ReosHydraulicNode::typeString() + QString( ':' ) + QStringLiteral( "hydrograph" );}

    QPointF position() const override  {return QPointF();}

};

//! Abstract class that represent a hydrograph source, that is a node that has a hydrograph as output
class ReosHydrographSource : public ReosHydrographNode
{
    Q_OBJECT
  public:
    ReosHydrographSource( ReosHydraulicNetwork *parent = nullptr );
    virtual ReosHydrograph *outputHydrograph( const ReosCalculationContext &context ) = 0;

    ReosHydrographRouting *outputHydrographTransfer() const;

    QString type() const override {return typeString(); }
    static QString typeString() {return ReosHydrographNode::typeString() + QString( ':' ) + QStringLiteral( "source" );}

};

//! Class that represent an hydrograph source with a fixed hydrograph
class ReosHydrographSourceFixed: public ReosHydrographSource
{
  public:
    ReosHydrographSourceFixed( ReosHydraulicNetwork *parent = nullptr );
    ReosHydrograph *outputHydrograph( const ReosCalculationContext &context ) override;

    QString type() const override {return typeString();}
    static QString typeString() {return ReosHydrographSource::typeString() + QString( ':' ) + QStringLiteral( "fixed" );}

    //! Sets the hydrographs, take ownership
    void setHydrograph( ReosHydrograph *hydrograph );

  private:
    ReosHydrograph *mHydrograph = nullptr;
};

/**
 * Class that represents an node collecting and sum hydrograph
 */
class ReosHydrographJunction : public ReosHydrographSource
{
    Q_OBJECT
  public:
    ReosHydrographJunction( const QPointF &position, ReosHydraulicNetwork *parent = nullptr );
    ReosHydrograph *outputHydrograph( const ReosCalculationContext &context ) override;

    QString type() const override {return typeString(); }
    static QString typeString() {return ReosHydrographSource::typeString() + QString( ':' ) + QStringLiteral( "junction" );}

    QPointF position() const override;

  private:
    mutable ReosHydrograph *mHydrograph;

    QPointF mPosition;
};


//! Class that represent a hydrograph source for a specified watershed
class ReosHydrographSourceWatershed : public ReosHydrographJunction
{
    Q_OBJECT
  public:
    //! Constructor with \a watershed
    ReosHydrographSourceWatershed( ReosWatershed *watershed, ReosHydraulicNetwork *parent = nullptr );

    QString type() const override {return typeString();}
    static QString typeString() {return ReosHydrographJunction::typeString() + QString( ':' ) + QStringLiteral( "watershed" );}

    ReosHydrograph *outputHydrograph( const ReosCalculationContext &context ) override;
    QPointF position() const override;

    ReosWatershed *watershed() const;

  private:
    QPointer<ReosWatershed> mWatershed;
    ReosHydrograph *mHydrograph = nullptr;
};

#endif // REOSHYDROGRAPHSOURCE_H
