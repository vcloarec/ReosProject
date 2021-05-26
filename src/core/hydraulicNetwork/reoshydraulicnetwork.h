/***************************************************************************
  reoshydraulicnetwork.h - ReosHydraulicNetwork

 ---------------------
 begin                : 20.5.2021
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
#ifndef REOSHYDRAULICNETWORK_H
#define REOSHYDRAULICNETWORK_H

#include <QPointer>
#include <QHash>

#include "reosmodule.h"

class ReosHydraulicNode;
class ReosHydraulicLink;
class ReosHydraulicNetwork;

class ReosHydraulicNetworkElement : public QObject
{
    Q_OBJECT
  public:
    ReosHydraulicNetworkElement( ReosHydraulicNetwork *parent = nullptr );
    virtual ~ReosHydraulicNetworkElement();

    QString id() const;

    virtual QString type() const {return hydraulicElementType();}
    static QString hydraulicElementType() {return QStringLiteral( "hydraulicNetwork" );}

    //! Destroy the element (the instance will be deleted later).
    virtual void destroy();

    void positionChanged();

  private:
    QPointer<ReosHydraulicNetwork> mNetWork = nullptr;
    QString mUid;

};

class ReosHydraulicNetwork : public ReosModule
{
    Q_OBJECT
  public:
    ReosHydraulicNetwork( ReosModule *parent ): ReosModule( parent ) {}
    QList<ReosHydraulicNetworkElement *> getElements( const QString &type ) const;
    ReosHydraulicNetworkElement *getElement( const QString &elemId ) const;

    void addElement( ReosHydraulicNetworkElement *elem );
    void removeElement( ReosHydraulicNetworkElement *elem );

  signals:
    void elementAdded( ReosHydraulicNetworkElement *elem );
    void elementRemoved( ReosHydraulicNetworkElement *elem );
    void elementPositionHasChanged( ReosHydraulicNetworkElement *elem );

  private:
    QHash<QString, ReosHydraulicNetworkElement *> mElements;
    void elemPositionChangedPrivate( ReosHydraulicNetworkElement *elem );


    friend class ReosHydraulicNetworkElement;

};

#endif // REOSHYDRAULICNETWORK_H
