/***************************************************************************
  reoshydrauliclink.h - ReosHydraulicLink

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
#ifndef REOSHYDRAULICLINK_H
#define REOSHYDRAULICLINK_H

#include <QObject>
#include <QPointer>

#include "reoshydraulicnode.h"
#include "reoshydraulicnetwork.h"

class ReosHydraulicLink : public ReosHydraulicNetworkElement
{
  public:
    ReosHydraulicLink( ReosHydraulicNetwork *parent = nullptr );

    QString type() const override {return hydraulicLinkType();}
    static QString hydraulicLinkType() {return hydraulicElementType() + QString( ':' ) + QStringLiteral( "link" );}

    ReosHydraulicNode *firstNode() const;
    ReosHydraulicNode *secondNode() const;

  protected:
    void attachOnSide1( ReosHydraulicNode *node );
    void attachOnSide2( ReosHydraulicNode *node );

    QPointer<ReosHydraulicNode> mNode_1;
    QPointer<ReosHydraulicNode> mNode_2;

    friend class ReosHydraulicNode;
};

#endif // REOSHYDRAULICLINK_H
