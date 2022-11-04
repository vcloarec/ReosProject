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

#include "reoscore.h"
#include "reosmodule.h"
#include "reosdataobject.h"
#include "reosparameter.h"

class ReosHydraulicNode;
class ReosHydraulicLink;
class ReosHydraulicNetwork;
class ReosCalculationContext;
class ReosHydraulicNetworkContext;
class ReosWatershedModule;
class ReosGisEngine;
class ReosHydraulicSchemeCollection;
class ReosHydraulicScheme;
class ReosMapExtent;

class REOSCORE_EXPORT ReosHydraulicNetworkElement : public ReosDataObject
{
    Q_OBJECT
  public:
    ReosHydraulicNetworkElement( ReosHydraulicNetwork *parent = nullptr );
    ReosHydraulicNetworkElement( const ReosEncodedElement &encodedElement, ReosHydraulicNetwork *parent = nullptr );
    virtual ~ReosHydraulicNetworkElement();

    QString type() const override {return staticType();}
    static QString staticType() {return QStringLiteral( "hydraulicNetwork" );}

    ReosParameterString *elementName() const;

    virtual QString defaultDisplayName() const {return type();}

    //! Destroy the element (the instance will be deleted later).
    virtual void destroy();

    //! Called when the position of the item is changed
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

    virtual bool isAutoSelectable() const {return true;}

    virtual bool isRemovable() const {return true;}

    virtual void saveConfiguration( ReosHydraulicScheme *scheme ) const;
    virtual void restoreConfiguration( ReosHydraulicScheme *scheme );

    virtual ReosDuration currentElementTimeStep() const;

    ReosHydraulicNetwork *network() const;

    virtual ReosMapExtent extent() const = 0;

  public slots:
    virtual void updateCalculationContext( const ReosCalculationContext &context ) = 0;

  protected:
    QPointer<ReosHydraulicNetwork> mNetwork = nullptr;

    void calculationUpdated()
    {
      setActualized();
      emit calculationIsUpdated( id(), QPrivateSignal() );
    }

    virtual void encodeData( ReosEncodedElement &element,  const ReosHydraulicNetworkContext &context ) const = 0;

  signals:
    void calculationStart();
    void calculationIsUpdated( const QString &id, QPrivateSignal );
    void timeStepChanged();
    void dirtied();

  private:
    ReosParameterString *mNameParameter = nullptr;
    ReosParameterDuration *mConstantTimeStepInTable = nullptr;
    ReosParameterBoolean *mUseConstantTimeStepInTable = nullptr;
    ReosModule::Message mLastMessage;

    void init();

};

class REOSCORE_EXPORT ReosHydraulicNetworkContext
{
  public:
    ReosWatershedModule *watershedModule() const;
    ReosHydraulicNetwork *network() const;

    QString crs() const;

    QString projectPath() const;
    QString projectName() const;

  private:
    ReosHydraulicNetworkContext() {}

    ReosHydraulicNetwork *mNetwork = nullptr;
    ReosWatershedModule *mWatershedModule;
    QString mProjectPath;
    QString mProjectName;

    friend class ReosHydraulicNetwork;

};

class ReosHydraulicNetworkElementFactory
{
  public:
    ReosHydraulicNetworkElementFactory();
    virtual ~ReosHydraulicNetworkElementFactory();

    virtual ReosHydraulicNetworkElement *decodeElement( const ReosEncodedElement &encodedElement, const ReosHydraulicNetworkContext &context ) const = 0;
};

class REOSCORE_EXPORT ReosHydraulicNetwork : public ReosModule
{
    Q_OBJECT
  public:
    ReosHydraulicNetwork( ReosModule *parent, ReosGisEngine *gisEngine, ReosWatershedModule *watershedModule );
    QList<ReosHydraulicNetworkElement *> getElements( const QString &type ) const;
    ReosHydraulicNetworkElement *getElement( const QString &elemId ) const;

    ReosHydraulicNetworkElement *addElement( ReosHydraulicNetworkElement *elem, bool select = true );
    void removeElement( ReosHydraulicNetworkElement *elem );

    void decode( const ReosEncodedElement &element, const QString &projectPath, const QString &projectFileName );
    ReosEncodedElement encode( const QString &projectPath, const QString &projectFileName ) const;

    //! Clears the network
    void clear();

    //! Reset the network, that is, clear all and creates just one hydraulic scheme
    void reset();


    ReosGisEngine *gisEngine() const;

    ReosHydraulicNetworkContext context() const;

    ReosCalculationContext calculationContext() const;

    ReosHydraulicSchemeCollection *hydraulicSchemeCollection() const;

    int currentSchemeIndex() const;

    ReosHydraulicScheme *currentScheme() const;

    void setCurrentScheme( int newSchemeIndex );

    ReosDuration currentTimeStep() const;

    ReosMapExtent networkExtent() const;

  signals:
    void elementAdded( ReosHydraulicNetworkElement *elem, bool select );
    void elementRemoved( ReosHydraulicNetworkElement *elem );
    void elementPositionHasChanged( ReosHydraulicNetworkElement *elem );
    void hasBeenReset();
    void schemeChanged();
    void loaded();
    void timeStepChanged();

  public slots:
    void changeScheme( int newSchemeIndex );

  private:
    ReosGisEngine *mGisEngine = nullptr;
    ReosWatershedModule *mWatershedModule = nullptr;
    ReosHydraulicSchemeCollection *mHydraulicSchemeCollection = nullptr;
    int mCurrentSchemeIndex = -1;
    QHash<QString, ReosHydraulicNetworkElement *> mElements;
    mutable QString mProjectPath;
    mutable QString mProjectName;

    void elemPositionChangedPrivate( ReosHydraulicNetworkElement *elem );

    QHash<QString, int> mElementIndexesCounter;

    friend class ReosHydraulicNetworkElement;

    std::map<QString, std::unique_ptr<ReosHydraulicNetworkElementFactory>> mElementFactories;
    void addEncodedElement( const ReosEncodedElement &element );

};

#endif // REOSHYDRAULICNETWORK_H
