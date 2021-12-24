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

#include<memory>
#include <QPointer>
#include <QHash>

#include "reosmodule.h"
#include "reosdataobject.h"
#include "reosparameter.h"

class ReosHydraulicNode;
class ReosHydraulicLink;
class ReosHydraulicNetwork;
class ReosCalculationContext;
class ReosHydraulicNetworkContext;
class ReosWatershedModule;

class ReosHydraulicNetworkElement : public ReosDataObject
{
    Q_OBJECT
  public:
    ReosHydraulicNetworkElement( ReosHydraulicNetwork *parent = nullptr );
    ReosHydraulicNetworkElement( const ReosEncodedElement &encodedElement, ReosHydraulicNetwork *parent = nullptr );
    virtual ~ReosHydraulicNetworkElement();

    QString id() const;

    QString type() const override {return staticType();}
    static QString staticType() {return QStringLiteral( "hydraulicNetwork" );}

    ReosParameterString *name() const;
    virtual QString defaultDisplayName() const {return type();}

    //! Destroy the element (the instance will be deleted later).
    virtual void destroy();

    //! Called when the position oh the item is changed
    void positionChanged();

    //! Returns the parameter of duration used as time step in table when constant time step is used
    ReosParameterDuration *constantTimeStepInTable() const;

    //! Returns the parameter whether constant tule step is used in table
    ReosParameterBoolean *useConstantTimeStepInTable() const;

    //! Returns whether calculation is in progress, default implementation returns false
    virtual bool calculationInProgress() const {return false;}

    //! Returns the maximum progression of calculation
    virtual int calculationMaxProgression() const {return 0;}

    //! Returns the progression of calculation
    virtual int calculationProgression() const {return 0;}

    ReosEncodedElement encode( const ReosHydraulicNetworkContext &context ) const;

    void notify( const ReosModule::Message &message );

    ReosModule::Message lastMessage() const;

  public slots:
    virtual void updateCalculationContext( const ReosCalculationContext &context ) = 0;

  protected:
    void calculationUpdated()
    {
      setActualized();
      emit calculationIsUpdated( id(), QPrivateSignal() );
    }

    virtual void encodeData( ReosEncodedElement &element,  const ReosHydraulicNetworkContext &context ) const = 0;

  signals:
    void calculationStart();
    void calculationIsUpdated( const QString &id, QPrivateSignal );

  private:
    QPointer<ReosHydraulicNetwork> mNetWork = nullptr;
    QString mUid;
    ReosParameterString *mNameParameter = nullptr;
    ReosParameterDuration *mConstantTimeStepInTable = nullptr;
    ReosParameterBoolean *mUseConstantTimeStepInTable = nullptr;
    ReosModule::Message mLastMessage;

};

class ReosHydraulicNetworkContext
{
  public:
    ReosWatershedModule *watershedModule() const;
    ReosHydraulicNetwork *network() const;

  private:
    ReosHydraulicNetworkContext() {}

    ReosHydraulicNetwork *mNetwork = nullptr;
    ReosWatershedModule *mWatershedModule;

    friend class ReosHydraulicNetwork;

};

class ReosHydraulicNetworkElementFactory
{
  public:
    ReosHydraulicNetworkElementFactory();
    virtual ~ReosHydraulicNetworkElementFactory();

    virtual ReosHydraulicNetworkElement *decodeElement( const ReosEncodedElement &encodedElement, const ReosHydraulicNetworkContext &context ) const = 0;
};

class ReosHydraulicNetwork : public ReosModule
{
    Q_OBJECT
  public:
    ReosHydraulicNetwork( ReosModule *parent, ReosWatershedModule *watershedModule );
    QList<ReosHydraulicNetworkElement *> getElements( const QString &type ) const;
    ReosHydraulicNetworkElement *getElement( const QString &elemId ) const;

    ReosHydraulicNetworkElement *addElement( ReosHydraulicNetworkElement *elem );
    void removeElement( ReosHydraulicNetworkElement *elem );

    void decode( const ReosEncodedElement &element );
    ReosEncodedElement encode() const;

  signals:
    void elementAdded( ReosHydraulicNetworkElement *elem );
    void elementRemoved( ReosHydraulicNetworkElement *elem );
    void elementPositionHasChanged( ReosHydraulicNetworkElement *elem );

  private:
    ReosWatershedModule *mWatershedModule = nullptr;
    QHash<QString, ReosHydraulicNetworkElement *> mElements;
    void elemPositionChangedPrivate( ReosHydraulicNetworkElement *elem );

    QHash<QString, int> mElementIndexesCounter;

    friend class ReosHydraulicNetworkElement; //TODO ccheck if really necessary

    ReosHydraulicNetworkContext context() const
    {
      ReosHydraulicNetworkContext context;
      context.mWatershedModule = mWatershedModule;
      context.mNetwork = const_cast<ReosHydraulicNetwork *>( this );
      return context;
    }

    std::map<QString, std::unique_ptr<ReosHydraulicNetworkElementFactory>> mElementFactories;
    void addEncodedElement( const ReosEncodedElement &element );

};

#endif // REOSHYDRAULICNETWORK_H
