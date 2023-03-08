/***************************************************************************
  reoshydraulicnode.h - ReosHydraulicNode

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
#ifndef REOSHYDRAULICNODE_H
#define REOSHYDRAULICNODE_H

#define SIP_NO_FILE

#include <QObject>
#include <QPointer>

#include "reoshydraulicnetwork.h"

class ReosHydraulicLink;
class ReosSpatialPosition;

class REOSCORE_EXPORT ReosHydraulicNode : public ReosHydraulicNetworkElement
{
    Q_OBJECT
  public:
    ReosHydraulicNode( ReosHydraulicNetwork *parent );
    ~ReosHydraulicNode();

    QString type() const override {return staticType();}
    static QString staticType() {return ReosHydraulicNetworkElement::staticType() + QString( ':' ) + QStringLiteral( "node" );}

    QList<ReosHydraulicLink *> links() const;

    QList<ReosHydraulicLink *> linksBySide1() const;
    QList<ReosHydraulicLink *> linksBySide2() const;

    virtual QPointF position( const QString &destinationCrs ) const = 0;
    virtual ReosSpatialPosition spatialPosition() const = 0;
    virtual void setPosition( const ReosSpatialPosition &pos ) = 0;

    //! Default inmplementation return false
    virtual bool canAcceptLink( const QString &linkId, int positionInLink );

    ReosMapExtent extent() const override;

  protected:
    ReosHydraulicNode( const ReosEncodedElement &encodedElement, ReosHydraulicNetwork *parent = nullptr );

    QList<QPointer<ReosHydraulicLink>> mLinksBySide1;
    QList<QPointer<ReosHydraulicLink>> mLinksBySide2;

    void attachBySide1( ReosHydraulicLink *link );
    void detachFromSide1( ReosHydraulicLink *link );
    void attachBySide2( ReosHydraulicLink *link );
    void detachFromSide2( ReosHydraulicLink *link );

    friend class ReosHydraulicLink;

};

#endif // REOSHYDRAULICNODE_H
