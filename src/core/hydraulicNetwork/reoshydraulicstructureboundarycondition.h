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
    enum class Type
    {
      NotDefined,
      InputFlow,
      OutputLevel,
    };

    ReosHydraulicStructureBoundaryCondition(
      ReosHydraulicStructure2D *hydStructure,
      const QString &boundaryConditionId,
      const ReosHydraulicNetworkContext &context );

    QString type() const override {return staticType();}
    static QString staticType() {return ReosHydrographJunction::staticType() + QString( ':' ) + QStringLiteral( "structure-boundary-condition" );}
    static ReosHydraulicStructureBoundaryCondition *decode( const ReosEncodedElement &encodedElement, const ReosHydraulicNetworkContext &context );

    QPointF position( const QString &destinationCrs ) const override;
    void setPosition( const ReosSpatialPosition & ) override {};
    bool isAutoSelectable() const override;
    bool isRemovable() const override {return false;}
    bool canAcceptLink( const QString &linkId, int positionInLink ) override;
    void updateCalculationContextFromUpstream( const ReosCalculationContext &context, ReosHydrographRoutingLink *upstreamLink, bool upstreamWillChange ) override;

    QString boundaryConditionId() const;

    void attachStructure( ReosHydraulicStructure2D *structure );

    Type conditionType() const;

  public slots:
    //void updateCalculationContext( const ReosCalculationContext &context );
    //virtual void onUpstreamRoutingUpdated( const QString &routingId ) {}

  private slots:
    void onBoundaryClassesChange();
    void onParameterNameChange();

  private:
    ReosHydraulicNetworkContext mContext;
    QPointer<ReosHydraulicStructure2D> mStructure;
    QString mBoundaryConditionId;

  protected:
    ReosHydraulicStructureBoundaryCondition( const ReosEncodedElement &encodedElement, ReosHydraulicNetwork *parent = nullptr );
    void encodeData( ReosEncodedElement &, const ReosHydraulicNetworkContext & ) const;

};

class ReosHydraulicStructureBoundaryConditionFactory : public ReosHydraulicNetworkElementFactory
{
  public:
    ReosHydraulicStructureBoundaryConditionFactory() = default;
    ReosHydraulicNetworkElement *decodeElement( const ReosEncodedElement &encodedElement, const ReosHydraulicNetworkContext &context ) const override;
};

#endif // REOSHYDRAULICSTRUCTUREBOUNDARYCONDITION_H
