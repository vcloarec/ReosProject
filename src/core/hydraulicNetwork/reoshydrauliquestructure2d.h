/***************************************************************************
  reoshydrauliquestructure2d.h - ReosHydrauliqueStructure2D

 ---------------------
 begin                : 9.1.2022
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
#ifndef REOSHYDRAULIQUESTRUCTURE2D_H
#define REOSHYDRAULIQUESTRUCTURE2D_H

#include "reoshydraulicnetwork.h"

class ReosHydraulicStructure2D : public ReosHydraulicNetworkElement
{
    Q_OBJECT
  public:
    ReosHydraulicStructure2D( const QPolygonF &domain, ReosHydraulicNetwork *parent = nullptr );

    QString type() const override {return staticType();}
    static QString staticType() {return ReosHydraulicNetworkElement::staticType() + QString( ':' ) + QStringLiteral( "structure2D" );}

    //! Returns the domain polygon
    QPolygonF domain() const;

  public slots:
    void updateCalculationContext( const ReosCalculationContext &context ) {}

  protected:
    void encodeData( ReosEncodedElement &element, const ReosHydraulicNetworkContext &context ) const {}

  private:
    QPolygonF mDomain;
};

#endif // REOSHYDRAULIQUESTRUCTURE2D_H
