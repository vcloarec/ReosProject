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

#include "reoscore.h"
#include "reoshydraulicnode.h"
#include "reoshydraulicnetwork.h"

class REOSCORE_EXPORT ReosHydraulicLink : public ReosHydraulicNetworkElement
{
    Q_OBJECT
  public:
    ReosHydraulicLink( ReosHydraulicNetwork *parent = nullptr );

    QString type() const override {return staticType();}
    static QString staticType() {return ReosHydraulicNetworkElement::staticType() + QString( ':' ) + QStringLiteral( "link" );}

    ReosHydraulicNode *firstNode() const;
    ReosHydraulicNode *secondNode() const;

    static QPair<QString, QString> decodeNodesId( const ReosEncodedElement &element )
    {
      QString idNode1;
      QString idNode2;

      element.getData( QStringLiteral( "id-node-1" ), idNode1 );
      element.getData( QStringLiteral( "id-node-2" ), idNode2 );

      return {idNode1, idNode2};
    }

    void destroy() override;

  protected:
    ReosHydraulicLink( const ReosEncodedElement &encodedElement, ReosHydraulicNetwork *parent = nullptr );

    void attachOnSide1( ReosHydraulicNode *node );
    void attachOnSide2( ReosHydraulicNode *node );

    void encodeData( ReosEncodedElement &element,  const ReosHydraulicNetworkContext & ) const override
    {
      element.addData( QStringLiteral( "id-node-1" ), mNode_1->id() );
      element.addData( QStringLiteral( "id-node-2" ), mNode_2->id() );
    }

    QPointer<ReosHydraulicNode> mNode_1;
    QPointer<ReosHydraulicNode> mNode_2;

  private:
    friend class ReosHydraulicNode;
};

#endif // REOSHYDRAULICLINK_H
