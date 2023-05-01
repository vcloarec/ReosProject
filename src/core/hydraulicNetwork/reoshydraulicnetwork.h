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

#define SIP_NO_FILE

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
class ReosMeteorologicModel;

struct REOSCORE_EXPORT ReosHydraulicNetworkElementCompatibilty
{
  bool isCompatible = true;
  QStringList incompatibilityReasons;

  void combine( const ReosHydraulicNetworkElementCompatibilty &other )
  {
    incompatibilityReasons.append( other.incompatibilityReasons );
    isCompatible &= other.isCompatible;
  }
};

class REOSCORE_EXPORT ReosHydraulicNetworkElement : public ReosDataObject
{
    Q_OBJECT
  public:
    ReosHydraulicNetworkElement( ReosHydraulicNetwork *parent = nullptr );
    ReosHydraulicNetworkElement( const ReosEncodedElement &encodedElement, ReosHydraulicNetwork *parent = nullptr );
    virtual ~ReosHydraulicNetworkElement();

    QString type() const override {return staticType();}
    static QString staticType() {return QStringLiteral( "hydraulicNetwork" );}

    ReosParameterString *elementNameParameter() const SIP_SKIP;

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

    //! Cleans all data related to \a scheme, returns a list of files or folder that should be removec when saving the project
    virtual QFileInfoList cleanScheme( ReosHydraulicScheme *scheme );

    virtual ReosDuration currentElementTimeStep() const;

    virtual ReosTimeWindow timeWindow() const;

    virtual ReosDuration mapTimeStep() const;

    ReosHydraulicNetwork *network() const;

    virtual ReosMapExtent extent() const = 0;

    virtual QIcon icon() const {return QIcon();}

    //! Return information about the compatibilty of this element with the \a scheme
    virtual ReosHydraulicNetworkElementCompatibilty checkCompatiblity( ReosHydraulicScheme *scheme ) const;

  public slots:
    virtual void updateCalculationContext( const ReosCalculationContext &context ) = 0;

  signals:
    void timeWindowChanged();
    void mapTimeStepChanged();

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

    ReosEncodeContext encodeContext() const;

    QString currentSchemeId() const;

  private:
    ReosHydraulicNetworkContext() {}

    ReosHydraulicNetwork *mNetwork = nullptr;
    ReosWatershedModule *mWatershedModule = nullptr;
    QString mProjectPath;
    QString mProjectName;

    friend class ReosHydraulicNetwork;

};

class ReosHydraulicNetworkElementFactory
{
  public:
    ReosHydraulicNetworkElementFactory();
    virtual ~ReosHydraulicNetworkElementFactory();

    //! Decodes hydraulic element from \a encodedElement and and it to the network of \a context
    virtual ReosHydraulicNetworkElement *decodeElement( const ReosEncodedElement &encodedElement, const ReosHydraulicNetworkContext &context ) const = 0;
};

class REOSCORE_EXPORT ReosHydraulicNetwork : public ReosModule
{
    Q_OBJECT
  public:
    ReosHydraulicNetwork( ReosModule *parent, ReosGisEngine *gisEngine, ReosWatershedModule *watershedModule );

    QFileInfoList uselessFiles( bool clean ) const override;

    //! Returns element with id \a elementId
    ReosHydraulicNetworkElement *getElement( const QString &elemId ) const;

    //! Returns the count of elements present in the network
    int elementsCount() const;

    //! Returns a list of elements of \a type. Returns all element if type is empty or not prvided
    QList<ReosHydraulicNetworkElement *> hydraulicNetworkElements( const QString &type = QString() ) const;

    ReosHydraulicNetworkElement *addElement( ReosHydraulicNetworkElement *elem, bool select = true );

    //! Removes the element \a elem. The element is destoyed by calling this method.
    void removeElement( ReosHydraulicNetworkElement *elem );

    void decode( const ReosEncodedElement &element, const QString &projectPath, const QString &projectFileName );
    ReosEncodedElement encode( const QString &projectPath, const QString &projectFileName ) const;

    //! Clears the network
    void clear();

    //! Reset the network, that is, clear all and creates just one hydraulic scheme
    void reset();

    ReosGisEngine *gisEngine() const;

    //! Returns an hydraulic network context corresponding to this network
    ReosHydraulicNetworkContext context() const;

    ReosCalculationContext calculationContext() const;

    ReosHydraulicSchemeCollection *hydraulicSchemeCollection() const;

    int schemeCount() const;
    int currentSchemeIndex() const;
    ReosHydraulicScheme *currentScheme() const;
    ReosHydraulicScheme *scheme( const QString &schemeId ) const;
    ReosHydraulicScheme *scheme( int index ) const;
    ReosHydraulicScheme *schemeByName( const QString &schemeName ) const;
    int schemeIndex(const QString &schemeId ) const;
    void setCurrentScheme( int newSchemeIndex );
    ReosHydraulicScheme *addNewScheme( const QString &schemeName, ReosMeteorologicModel *meteoModel = nullptr );
    void addExistingScheme( ReosHydraulicScheme *scheme );
    void removeScheme( int schemeIndex );

    ReosDuration currentTimeStep() const;

    ReosMapExtent networkExtent() const;

    //! Return time window including all element that can be rendered on map
    ReosTimeWindow mapTimeWindow() const;

    ReosHydraulicNetworkElementCompatibilty checkSchemeCompatibility( ReosHydraulicScheme *scheme ) const;

    static QString staticName() {return QStringLiteral( "hydraulic-network" );}

  signals:
    void elementAdded( ReosHydraulicNetworkElement *elem, bool select );
    void elementWillBeRemoved( ReosHydraulicNetworkElement *elem );
    void elementRemoved();
    void elementPositionHasChanged( ReosHydraulicNetworkElement *elem );
    void hasBeenReset();
    void schemeChanged();
    void loaded();
    void timeStepChanged();

  public slots:
    void changeScheme( int newSchemeIndex );

  private slots:
    void onMapTimeWindowChanged();

  private:
    ReosGisEngine *mGisEngine = nullptr;
    ReosWatershedModule *mWatershedModule = nullptr;
    ReosHydraulicSchemeCollection *mHydraulicSchemeCollection = nullptr;
    int mCurrentSchemeIndex = -1;
    QHash<QString, ReosHydraulicNetworkElement *> mElements;
    mutable QString mProjectPath;
    mutable QString mProjectName;
    mutable QFileInfoList mUselessFile;

    void elemPositionChangedPrivate( ReosHydraulicNetworkElement *elem );

    QHash<QString, int> mElementIndexesCounter;

    friend class ReosHydraulicNetworkElement;

    std::map<QString, std::unique_ptr<ReosHydraulicNetworkElementFactory>> mElementFactories;
    void addEncodedElement( const ReosEncodedElement &element );

};

#endif // REOSHYDRAULICNETWORK_H
