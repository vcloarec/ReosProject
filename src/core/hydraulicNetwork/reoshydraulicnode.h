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

#include <QObject>
#include <QPointer>

#include "reoshydraulicnetwork.h"

class ReosHydraulicLink;

class ReosHydraulicNode : public ReosHydraulicNetworkElement
{
  public:
    ReosHydraulicNode( ReosHydraulicNetwork *parent );
    ~ReosHydraulicNode();

    QString type() const override {return QStringLiteral( "node" );}

  protected:
    QList<QPointer<ReosHydraulicLink>> mLinksBySide1;
    QList<QPointer<ReosHydraulicLink>> mLinksBySide2;

    friend class ReosHydraulicLink;

};

#endif // REOSHYDRAULICNODE_H
