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
#include "reosdataobject.h"
#include "reosparameter.h"

class ReosHydraulicNode;
class ReosHydraulicLink;
class ReosHydraulicNetwork;
class ReosCalculationContext;

class ReosHydraulicNetworkElement : public ReosDataObject
{
    Q_OBJECT
  public:
    ReosHydraulicNetworkElement( ReosHydraulicNetwork *parent = nullptr );
    virtual ~ReosHydraulicNetworkElement();

    QString id() const;

    QString type() const override {return typeString();}
    static QString typeString() {return QStringLiteral( "hydraulicNetwork" );}

    //! Destroy the element (the instance will be deleted later).
    virtual void destroy();

    void positionChanged();

    ReosParameterString *name() const;

    QString defaultDisplayName() const {return type();}

  public slots:
    virtual void updateCalculation( const ReosCalculationContext &context ) = 0;

  protected:
    void calculationUpdated()
    {
      emit calculationIsUpdated( id(), QPrivateSignal() );
    }

  signals:
    void calculationIsUpdated( const QString &id, QPrivateSignal );

  private:
    QPointer<ReosHydraulicNetwork> mNetWork = nullptr;
    QString mUid;
    ReosParameterString *mNameParameter = nullptr;

};

class ReosHydraulicNetwork : public ReosModule
{
    Q_OBJECT
  public:
    ReosHydraulicNetwork( ReosModule *parent );
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

    QHash<QString, int> mElementIndexesCounter;

    friend class ReosHydraulicNetworkElement;

};

#endif // REOSHYDRAULICNETWORK_H
