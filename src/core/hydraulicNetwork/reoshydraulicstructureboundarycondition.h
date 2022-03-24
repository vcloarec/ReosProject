/***************************************************************************
  reoshydraulicstructureboundarycondition.h - ReosHydraulicStructureBoundaryCondition

 ---------------------
 begin                : 23.3.2022
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
#ifndef REOSHYDRAULICSTRUCTUREBOUNDARYCONDITION_H
#define REOSHYDRAULICSTRUCTUREBOUNDARYCONDITION_H

#include "reoshydrographsource.h"
#include "reoshydraulicnetwork.h"

class ReosHydraulicStructure2D;

class ReosHydraulicStructureBoundaryCondition : public ReosHydrographJunction
{
    Q_OBJECT
  public:
    ReosHydraulicStructureBoundaryCondition(
      ReosHydraulicStructure2D *hydStructure,
      const QString &boundaryConditionId,
      const ReosHydraulicNetworkContext &context );

    QString type() const override {return staticType();}
    static QString staticType() {return ReosHydrographNode::staticType() + QString( ':' ) + QStringLiteral( "structure-boundary-condition" );}

    QPointF position( const QString &destinationCrs ) const override;
    void setPosition( const ReosSpatialPosition & ) override {};
    bool isAutoSelectable() const override;
    bool isRemovable() const override {return false;}
    bool canAcceptLink( const QString &linkId, int positionInLink ) override;
    void updateCalculationContextFromUpstream( const ReosCalculationContext &context, ReosHydrographRoutingLink *upstreamLink, bool upstreamWillChange ) {};

    QString boundaryConditionId() const;

  public slots:
    void updateCalculationContext( const ReosCalculationContext &context ) {}
    virtual void onUpstreamRoutingUpdated( const QString &routingId ) {}

  private slots:
    void onBoundaryClassesChange();
    void onParameterNameChange();

  private:
    ReosHydraulicNetworkContext mContext;
    QPointer<ReosHydraulicStructure2D> mStructure;
    QString mBoundaryConditionId;

  protected:
    void encodeData( ReosEncodedElement &, const ReosHydraulicNetworkContext & ) const {}


};

#endif // REOSHYDRAULICSTRUCTUREBOUNDARYCONDITION_H
